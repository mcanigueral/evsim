
# Utils -------------------------------------------------------------------
#' Round a numeric value to interval
#'
#' @param dbl numeric value
#' @param interval decimal interval (from 0 to 1)
#'
#' @keywords internal
#'
round_to_interval <- function (dbl, interval) {
  if (is.null(interval)) {
    return( dbl )
  }
  round(dbl/interval) * interval
}

#' Convert numeric time value to a datetime period (hour-based)
#'
#' @param time_num Numeric time value (hour-based)
#'
#' @importFrom lubridate hours minutes
#' @keywords internal
#'
convert_time_num_to_period <- function(time_num) {
  h <- time_num %/% 1
  m <- (time_num - h)*60 %/% 1
  hours(as.integer(h)) + minutes(as.integer(m))
}


#' Adapt charging features
#'
#' Calculate connection and charging times according to energy, power and time resolution
#'
#' All sessions' `Power` must be higher than `0`, to avoid `NaN` values from dividing
#' by zero.
#' The `ConnectionStartDateTime` is first aligned to the desired time resolution,
#' and the `ConnectionEndDateTime` is calculated according to the `ConnectionHours`.
#' The `ChargingHours` is recalculated with the values of `Energy` and `Power`,
#' limited by `ConnectionHours`. Finally, the charging times are also calculated.
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param time_resolution integer, time resolution (in minutes) of the sessions' datetime variables
#' @param power_resolution numeric, power resolution (in kW) of the sessions' power
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom lubridate round_date
#'
#' @examples
#' suppressMessages(library(dplyr))
#'
#' sessions <- head(evsim::california_ev_sessions, 10)
#'
#' sessions %>%
#'   select(ConnectionStartDateTime, ConnectionEndDateTime, Power)
#'
#' adapt_charging_features(
#'   sessions,
#'   time_resolution = 60,
#'   power_resolution = 0.01
#' ) %>%
#'   select(ConnectionStartDateTime, ConnectionEndDateTime, Power)
#'
#' adapt_charging_features(
#'   sessions,
#'   time_resolution = 15,
#'   power_resolution = 1
#' ) %>%
#'   select(ConnectionStartDateTime, ConnectionEndDateTime, Power)
#'
#'
adapt_charging_features <- function (sessions, time_resolution = 15, power_resolution = 0.01) {
  sessions %>%
    mutate(
      ConnectionStartDateTime = round_date(.data$ConnectionStartDateTime, paste(time_resolution, "mins")),
      ConnectionEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ConnectionHours),
      Power = round_to_interval(.data$Power, interval = power_resolution),
      ConnectionHours = round(as.numeric(.data$ConnectionEndDateTime - .data$ConnectionStartDateTime, unit="hours"), 2),
      ChargingHours = round(pmin(.data$Energy/.data$Power, .data$ConnectionHours), 2),
      Energy = round(.data$Power * .data$ChargingHours, 2),
      ChargingStartDateTime = .data$ConnectionStartDateTime,
      ChargingEndDateTime = .data$ChargingStartDateTime + convert_time_num_to_period(.data$ChargingHours)
    )
}


#' Charging rates distribution
#'
#' Get charging rates distribution in percentages from a charging sessions data set
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))t
#' @param unit character. Valid base units are `second`, `minute`, `hour`, `day`,
#' `week`, `month`, `bimonth`, `quarter`, `season`, `halfyear` and `year`.
#' It corresponds to `unit` parameter in `lubridate::floor_date` function.
#' @param power_interval numeric, interval of kW between power rates.
#' It is used to round the `Power` values into this interval resolution.
#' It can also be `NULL` to use all the original `Power` values.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% select mutate filter group_by ungroup summarise n all_of
#' @importFrom lubridate floor_date
#' @importFrom rlang .data
#'
#' @examples
#' get_charging_rates_distribution(evsim::california_ev_sessions, unit = "year")
#'
#'
get_charging_rates_distribution <- function(sessions, unit="year", power_interval = NULL) {
  sessions_power_round <- sessions %>%
    select(all_of(c("ConnectionStartDateTime", "Power"))) %>%
    mutate(
      power = round_to_interval(.data$Power, power_interval)
    )
  sessions_power_round %>%
    group_by(
      datetime = floor_date(.data$ConnectionStartDateTime, unit = unit),
      power = .data$power
    ) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(.data$datetime) %>%
    mutate(
      ratio = .data$n/sum(.data$n)
    ) %>%
    ungroup()
}



# Simulate sessions -------------------------------------------------------
#'
#' Estimate sessions energy values following a Gaussian distribution.
#' The minimum considered value is 1kWh based on real data analysis.
#'
#' @param n integer, number of sessions
#' @param mu numeric, mean of Gaussian distribution
#' @param sigma numeric, standard deviation of Gaussian distribution.
#' If unknown, a recommended value is `sd = mu/3`.
#' @param log logical, true if models have logarithmic transformation
#'
#' @return numeric vector
#' @keywords internal
#'
#' @importFrom stats rnorm
#'
estimate_energy <- function(n, mu, sigma, log) {
  energy_sim <- rnorm(n, mean = mu, sd = sigma)
  if (log) {
    energy_sim <- exp(energy_sim)
  }
  # Minimum 1kWh
  energy <- pmax(energy_sim, 1)
  return( energy )
}

#' Estimate energy given energy models tibble
#'
#' @param power_vct numeric vector of power values of simulated sessions
#' @param energy_models energy models tibble
#' @param energy_log Logical, true if models have logarithmic transformation
#'
#' @return list of numeric vectors
#' @keywords internal
#'
#' @importFrom purrr pmap map_lgl simplify
#' @importFrom dplyr tibble bind_rows
#'
get_estimated_energy <- function(power_vct, energy_models, energy_log) {
  n <- length(power_vct)
  energy_from_all_powers <- list()

  if (is.numeric(energy_models$charging_rate)) {
    # Check if we want to simulate energy for a charging rate that is not in the models
    charging_powers <- unique(power_vct)
    powers_not_in_models <- which(!(charging_powers %in% energy_models$charging_rate))
    if (length(powers_not_in_models) > 0) {
      for (power_extra in charging_powers[powers_not_in_models]) {
        power_extra_closest_rate <- which.min(abs(energy_models$charging_rate - power_extra))
        power_extra_model <- tibble(
          charging_rate = power_extra,
          energy_models = energy_models$energy_models[power_extra_closest_rate]
        )
        energy_models <- bind_rows(energy_models, power_extra_model)
        message(paste(
          "Warning:", power_extra,
          "kW rate not in models. Using energy models from",
          energy_models$charging_rate[power_extra_closest_rate], "kW rate."
        ))
      }
    }
  }

  for (rate in energy_models$charging_rate) {
    power_energy_model <- energy_models$energy_models[[which(energy_models$charging_rate == rate)]]
    power_energy <- as.numeric(simplify(pmap(
      power_energy_model, ~ estimate_energy(floor(n*..3)+1, ..1, ..2, energy_log)
    )))
    energy_from_all_powers[[as.character(rate)]] <- power_energy
  }

  if ("Unknown" %in% energy_models$charging_rate) {
    return(
      sample(
        energy_from_all_powers[["Unknown"]],
        size = n
      )
    )
  }

  energy_vct <- map_dbl(
    power_vct,
    ~ sample(
      energy_from_all_powers[[as.character(.x)]],
      size = 1
    )
  )

  if (any(map_lgl(energy_vct, ~ sum(is.null(.x)) | sum(is.na(.x))))) {
    message("Warning: NULL or NA values in energy simulation. Charging rates values must correspond to EV energy models charging rates.")
  }

  return( energy_vct )
}


#' Estimate sessions connection values
#'
#' Estimate sessions connection values following a Multi-variate Guassian
#' distribution.
#' The minimum considered value for duration is 30 minutes.
#'
#' @param n integer, number of sessions
#' @param mu numeric vector, means of bivariate GMM
#' @param sigma numeric matrix, covariance matrix of bivariate GMM
#' @param log logical, true if models have logarithmic transformation
#'
#' @return vector of numeric values
#' @keywords internal
#'
#' @importFrom MASS mvrnorm
#' @importFrom dplyr slice_sample
#'
estimate_connection <- function(n, mu, sigma, log) {
  ev_connections <- as.data.frame(matrix(mvrnorm(n = n, mu = mu, Sigma = sigma), ncol = 2))
  if (log) {
    ev_connections <- exp(ev_connections)
  }
  ev_connections[[1]] <- pmax(ev_connections[[1]], 0)
  ev_connections[[2]] <- pmax(ev_connections[[2]], 0.5)
  return( ev_connections )
}


#' Get estimated profiles
#'
#' @param n number of sessions
#' @param profile_models models of the profile
#' @param log Logical, true if models have logarithmic transformation
#'
#' @return list with sessions connection values
#' @keywords internal
#'
#' @importFrom purrr pmap
#'
get_estimated_connections <- function(n, profile_models, log) {
  return(pmap(
    profile_models,
    ~ estimate_connection(floor(n*..3)+1, ..1, ..2, log) # +1 to avoid n=0
  ))
}


#' Estimate sessions parameters of a specific profile
#'
#' @param profile_name character, profile name
#' @param n_sessions integer, total number of sessions per day
#' @param power numeric, charging power of the session
#' @param connection_models tibble, bivariate GMM of the profile
#' @param energy_models tibble, univariate GMM of the profile
#' @param connection_log logical, true if connection models have logarithmic transformations
#' @param energy_log logical, true if energy models have logarithmic transformations
#' @param charging_powers tibble with variables `power` and `ratio`
#' The powers must be in kW and the ratios between 0 and 1.
#'
#' @return tibble
#' @keywords internal
#'
#' @importFrom dplyr tibble bind_rows slice_sample sample_frac mutate select everything
#' @importFrom purrr map
#' @importFrom tidyr fill
#'
estimate_sessions <- function(profile_name, n_sessions, power, connection_models, energy_models, connection_log, energy_log, charging_powers) {

  if (n_sessions == 0) {
    return( NULL )
  }

  ev_sessions <- tibble()
  n_sessions_objective <- n_sessions - nrow(ev_sessions)

  while (n_sessions_objective > 0) {

    # Connections ----------------------------------------------------
    estimated_connections <- do.call(
      rbind,
      get_estimated_connections(n_sessions_objective, connection_models, connection_log)
    )

    # Power ----------------------------------------------------
    if (is.na(power)) {
      # Create all possible power bags to match the ratios
      power_bags <- map(
        charging_powers$ratio,
        ~ ceiling(.x*n_sessions_objective)
      )

      # Create a vector with random power indexes to choose from
      # From 1000 upwards the obtained ratios already matches the objective ratios
      random_idxs <- sample(
        seq_len(nrow(charging_powers)),
        size = 1000,
        prob = charging_powers$ratio,
        replace = TRUE
      )

      # Assign power from the bags
      estimated_power <- c()
      for (i in seq_len(n_sessions_objective)) {
        power_valid <- FALSE
        while (!power_valid) {
          power_idx <- sample(random_idxs, 1)
          if (power_bags[[power_idx]] > 0) {
            power_valid <- TRUE
          }
        }
        estimated_power <- c(estimated_power, charging_powers$power[power_idx])
        power_bags[[power_idx]] <- power_bags[[power_idx]] - 1
      }
    } else {
      estimated_power <- rep(power, times = n_sessions_objective)
    }

    # Energy ----------------------------------------------------
    estimated_energy <- get_estimated_energy(estimated_power, energy_models, energy_log)

    estimated_sessions <- tibble(
      start = round(estimated_connections[[1]], 2),
      duration = round(estimated_connections[[2]], 2),
      power = estimated_power[seq_len(nrow(estimated_connections))],
      energy = round(estimated_energy[seq_len(nrow(estimated_connections))], 2)
    ) %>%
      drop_na()

    ev_sessions <- bind_rows(ev_sessions, estimated_sessions)

    n_sessions_objective <- n_sessions - nrow(ev_sessions)
  }

  if (nrow(ev_sessions) > n_sessions) {
    ev_sessions <- ev_sessions %>%
      slice_sample(n = n_sessions)
  }

  ev_sessions <- ev_sessions %>%
    mutate(Profile = profile_name) %>%
    select("Profile", "start", "duration", "power", "energy")

  return( ev_sessions )
}


get_day_features <- function(day, ev_models) {
  month_day <- lubridate::month(day)
  wday_day <- lubridate::wday(day, week_start = 1)

  models_month_idx <- purrr::map_lgl(ev_models[["months"]], ~ month_day %in% .x)
  models_wday_idx <- purrr::map_lgl(ev_models[["wdays"]], ~ wday_day %in% .x)

  if (any(models_month_idx & models_wday_idx)) {
    day_timecycle <- ev_models[["time_cycle"]][models_month_idx & models_wday_idx][[1]]
    day_models <- ev_models[["user_profiles"]][models_month_idx & models_wday_idx][[1]]
    day_n_sessions <- ev_models[["n_sessions"]][models_month_idx & models_wday_idx][[1]]

    if (nrow(day_models) == 0) {
      message(paste("Warning: no models configured for", day_timecycle, "time-cycle."))
      day_models <- NA
      day_n_sessions <- 0
    }

  } else {
    message(paste("Warning: the day", as.character(day), "is not considered by the models."))
    day_timecycle <- NA
    day_models <- NA
    day_n_sessions <- 0
  }

  list(
    time_cycle = day_timecycle,
    models = day_models,
    n_sessions = day_n_sessions
  )
}


#' Get day sessions
#'
#' @param day Date to simulate
#' @param ev_models profiles models
#' @param connection_log Logical, true if connection models have logarithmic transformations
#' @param energy_log Logical, true if energy models have logarithmic transformations
#' @param charging_powers tibble with variables `power` and `ratio`
#' The powers must be in kW and the ratios between 0 and 1.
#'
#' @return tibble
#' @keywords internal
#'
#' @importFrom dplyr %>% slice_sample
#' @importFrom rlang .data
#' @importFrom purrr pmap_dfr
#'
get_day_sessions <- function(day, ev_models, connection_log, energy_log, charging_powers) {

  day_features <- get_day_features(day, ev_models)

  if (day_features$n_sessions == 0) {
    return( NULL )
  }

  day_sessions <- pmap_dfr(
    day_features$models,
    ~ estimate_sessions(
      profile_name = ..1,
      n_sessions = ceiling(..2*day_features$n_sessions),
      power = ..3,
      connection_models = ..4,
      energy_models = ..5,
      connection_log, energy_log,
      charging_powers
    )
  ) %>%
    mutate(
      start_dt = day + convert_time_num_to_period(.data$start),
      Timecycle = day_features$time_cycle
    ) %>%
    select("Timecycle", "Profile", "start_dt", "duration", "power", "energy")

  if (nrow(day_sessions) > day_features$n_sessions) {
    day_sessions <- day_sessions %>%
      slice_sample(n = day_features$n_sessions)
  }

  return( day_sessions )
}


#' Simulation of EV sessions
#'
#' Simulate EV charging sessions given the `evmodel` object and other contextual parameters.
#'
#' @param evmodel object of class `evmodel` built with `{evprof}`
#' (see this [link](https://mcanigueral.github.io/evprof/articles/evmodel.html) for more information)
#' @param sessions_day tibble with variables `time_cycle` (names corresponding to `evmodel$models$time_cycle`) and `n_sessions` (number of daily sessions per day for each time-cycle model)
#' @param user_profiles tibble with variables `time_cycle`, `profile`, `ratio` and optionally `power`.
#' It can also be `NULL` to use the `evmodel` original user profiles distribution.
#' The powers must be in kW and the ratios between 0 and 1.
#' The user profiles with a value of `power` will be simulated with this specific charging power.
#' If `power` is `NA` then it is simulated according to the ratios of next parameter `charging_powers`.
#' @param charging_powers tibble with variables `power` and `ratio`.
#' The powers must be in kW and the ratios between 0 and 1.
#' This is used to simulate the charging power of user profiles without a specific charging power in `user_profiles` parameter.
#'
#' @param dates date sequence that will set the time frame of the simulated sessions
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map map_dfr set_names
#' @importFrom dplyr mutate any_of row_number arrange left_join filter
#' @importFrom lubridate round_date as_datetime with_tz
#' @importFrom rlang .data
#' @importFrom tidyr drop_na
#'
#' @details
#' Some adaptations have been done to the output of the Gaussian models:
#' the minimum simulated energy is considered to be 1 kWh, while the minimum
#' connection duration is 30 minutes.
#'
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # Get the example `evmodel`
#' ev_model <- evsim::california_ev_model
#'
#' # Simulate EV charging sessions, considering that the Worktime sessions
#' # during Workdays have 11 kW, while all Visit sessions charge at 3.7kW or
#' # 11kW, with a distribution of 30% and 70% respectively.
#'
#' simulate_sessions(
#'   ev_model,
#'   sessions_day = tibble(
#'     time_cycle = c("Workday", "Weekend"),
#'     n_sessions = c(15, 10)
#'   ),
#'   user_profiles = tibble(
#'     time_cycle = c("Workday", "Workday", "Weekend"),
#'     profile = c("Visit", "Worktime", "Visit"),
#'     ratio = c(0.5, 0.5, 1),
#'     power = c(NA, 11, NA)
#'   ),
#'   charging_powers = tibble(
#'     power = c(3.7, 11),
#'     ratio = c(0.3, 0.7)
#'   ),
#'   dates = seq.Date(today(), today()+days(4), length.out = 4),
#'   resolution = 15
#' )
#'
simulate_sessions <- function(evmodel, sessions_day, user_profiles, charging_powers, dates, resolution) {

  if (sum(sessions_day[["n_sessions"]]) == 0) {
    message("No EV sessions to simulate")
    return( tibble() )
  }

  if (is.null(user_profiles)) {
    user_profiles <- get_user_profiles_distribution(evmodel)
  }

  ev_models <- prepare_model(evmodel[["models"]], sessions_day, user_profiles)
  connection_log <- evmodel[['metadata']][['connection_log']]
  energy_log <- evmodel[['metadata']][['energy_log']]
  tzone_model <- evmodel[['metadata']][['tzone']]

  dates_dttm <- round_date(as_datetime(dates, tz = tzone_model), unit = 'day')

  simulated_sessions <- map_dfr(
    dates_dttm,
    ~ get_day_sessions(.x, ev_models, connection_log, energy_log, charging_powers)
  )

  if (nrow(simulated_sessions) == 0) {
    return( tibble() )
  }

  simulated_sessions <- simulated_sessions %>%
    mutate(
      ConnectionStartDateTime = round_date(.data$start_dt, unit = paste(resolution, "minutes")),
      ConnectionHours = .data$duration,
      Power = .data$power,
      Energy = .data$energy
    ) %>%
    adapt_charging_features(resolution) %>%
    drop_na() %>%
    arrange(.data$ConnectionStartDateTime) %>%
    mutate(Session = paste0('S', row_number())) %>%
    select('Session', 'Timecycle', 'Profile', 'ConnectionStartDateTime', 'ConnectionEndDateTime',
           'ChargingStartDateTime', 'ChargingEndDateTime', 'Power', 'Energy',
           'ConnectionHours', 'ChargingHours')

  return( simulated_sessions )
}
