
# Utils -------------------------------------------------------------------

#' Round to nearest interval
#'
#' @param dbl number to round
#' @param interval rounding interval
#'
#' @return numeric value
#'
round_to_interval <- function(dbl, interval) {
  round(dbl/interval)*interval
}


#' Adapt charging time and energy according to power and time resolution
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
adapt_charging_features <- function (sessions, resolution = 15) {
  sessions %>%
    mutate(
      ChargingHours = pmin(round_to_interval(.data$Energy/.data$Power, resolution/60), .data$ConnectionHours),
      Energy = round(.data$Power * .data$ChargingHours, 2),
      ChargingStartDateTime = .data$ConnectionStartDateTime,
      ChargingEndDateTime = .data$ChargingStartDateTime + convert_time_num_to_period(.data$ChargingHours)
    )
}


#' Convert numeric time value to a datetime period (hour-based)
#'
#' @param time_num Numeric time value (hour-based)
#'
#' @importFrom lubridate hours minutes
#'
convert_time_num_to_period <- function(time_num) {
  h <- time_num %/% 1
  m <- (time_num - h)*60 %/% 1
  hours(as.integer(h)) + minutes(as.integer(m))
}


#' Get charging rates distribution in percentages
#'
#' @param sessions sessions data set in standard format
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% select mutate group_by summarise between n
#' @importFrom rlang .data
#'
get_charging_powers_ratios <- function(sessions) {
  sessions %>%
    mutate(
      Power = round_to_interval(.data$Power, 0.5),
      power = ifelse(
        between(.data$Power, 0, 4),
        3.7,
        ifelse(
          between(.data$Power, 4.5, 8),
          7.4,
          ifelse(
            between(.data$Power, 8.5, 12),
            11,
            ifelse(
              between(.data$Power, 12.5, 24),
              22,
              NA
            )
          )
        )
      )
    ) %>%
    group_by(.data$power) %>%
    summarise(n = n()) %>%
    mutate(ratio = .data$n/sum(.data$n)) %>%
    select(.data$power, .data$ratio)
}



# Simulate sessions -------------------------------------------------------

#' Estimate sessions energy values
#'
#' @param n number of sessions
#' @param mu means of univariate GMM
#' @param sigma covariance matrix of univariate GMM
#' @param log Logical, true if models have logarithmic transformation
#'
#' @return numeric vector
#'
#' @importFrom stats rnorm
#'
estimate_energy <- function(n, mu, sigma, log) {
  energy <- rnorm(n, mu, sigma)
  if (log) energy <- exp(energy)
  return( energy )
}

#' Estimate energy given energy models tibble
#'
#' @param power_vct numeric vector of power values of simulated sessions
#' @param energy_models energy models tibble
#' @param energy_log Logical, true if models have logarithmic transformation
#'
#' @return list of numeric vectors
#'
#' @importFrom purrr pmap map_lgl
#' @importFrom tibble tibble
#'
get_estimated_energy <- function(power_vct, energy_models, energy_log) {
  n <- length(power_vct)
  energy_from_all_powers <- list()

  if (!("charging_rate" %in% colnames(energy_models))) {
    energy_models <- tibble(
      charging_rate = "Unknown",
      energy_models = list(energy_models)
    )
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
#' @param n number of sessions
#' @param mu means of bivariate GMM
#' @param sigma covariance matrix of bivariate GMM
#' @param log Logical, true if models have logarithmic transformation
#'
#' @return vector of numeric values
#'
#' @importFrom MASS mvrnorm
#'
estimate_connection <- function(n, mu, sigma, log) {
  ev_connections <- as.data.frame(matrix(mvrnorm(n = n, mu = mu, Sigma = sigma), ncol = 2))
  if (log) ev_connections <- exp(ev_connections)
  return( ev_connections )
}


#' Get estimated profiles
#'
#' @param n number of sessions
#' @param profile_models models of the profile
#' @param log Logical, true if models have logarithmic transformation
#'
#' @return list with sessions connection values
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
#' @param profile_name profile name
#' @param n_sessions total number of sessions per day
#' @param connection_models bivariate GMM of the profile
#' @param energy_models univariate GMM of the profile
#' @param connection_log Logical, true if connection models have logarithmic transformations
#' @param energy_log Logical, true if energy models have logarithmic transformations
#' @param charging_powers tibble with variables `power` and `ratio`
#' The powers must be in kW and the ratios between 0 and 1.
#'
#' @return tibble
#'
#' @importFrom dplyr tibble bind_rows slice_sample sample_frac mutate select everything
#' @importFrom purrr simplify
#' @importFrom tidyr fill
#'
estimate_sessions <- function(profile_name, n_sessions, connection_models, energy_models, connection_log, energy_log, charging_powers) {

  if (n_sessions == 0) {
    return( NULL )
  }

  ev_sessions <- tibble()
  n_sessions_objective <- n_sessions - nrow(ev_sessions)

  while (n_sessions_objective > 0) {

    estimated_connections <- do.call(
      rbind,
      get_estimated_connections(n_sessions_objective, connection_models, connection_log)
    )

    estimated_power <- sample_frac(
      charging_powers['power'], size = n_sessions_objective,
      prob = charging_powers$ratio, replace = T
    )[["power"]]

    estimated_energy <- get_estimated_energy(estimated_power, energy_models, energy_log)

    estimated_sessions <- tibble(
      start = round(estimated_connections[[1]], 2),
      duration = round(estimated_connections[[2]], 2),
      power = estimated_power[1:nrow(estimated_connections)],
      energy = round(estimated_energy[1:nrow(estimated_connections)], 2)
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
    select("Profile", everything())

  return( ev_sessions )
}


get_day_features <- function(day, ev_models) {
  month_day <- lubridate::month(day)
  wday_day <- lubridate::wday(day, week_start = 1)

  models_month_idx <- purrr::map_lgl(ev_models[["months"]], ~ month_day %in% .x)
  models_wday_idx <- purrr::map_lgl(ev_models[["wdays"]], ~ wday_day %in% .x)

  day_models <- ev_models[["user_profiles"]][models_month_idx & models_wday_idx][[1]]
  day_n_sessions <- ev_models[["n_sessions"]][models_month_idx & models_wday_idx][[1]]

  if (is.na(day_n_sessions) | is.null(day_n_sessions) | is.nan(day_n_sessions)) {
    day_n_sessions <- 0
  }

  list(
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
    day_features$model,
    ~ estimate_sessions(..1, ceiling(..2*day_features$n_sessions), ..3, ..4, connection_log, energy_log, charging_powers)
  ) %>%
    mutate(start_dt = day + convert_time_num_to_period(.data$start)) %>%
    select(- .data$start)

  if (nrow(day_sessions) > day_features$n_sessions) {
    day_sessions <- day_sessions %>%
      slice_sample(n = day_features$n_sessions)
  }

  return( day_sessions )
}


#' Simulate sessions given the `evmodel` object and a datetime sequence
#'
#' @param evmodel object of type `evmodel` (see this [link](https://mcanigueral.github.io/evprof/articles/evmodel.html) for more information)
#' @param sessions_day tibble with variables `time_cycle` (names corresponding to `evmodel$models$time_cycle`) and `n_sessions` (number of daily sessions per day for each time-cycle model)
#' @param charging_powers tibble with variables `power` and `ratio`
#' The powers must be in kW and the ratios between 0 and 1.
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
#' @details The steps for simulating the sessions are:
#'
#' 1. Simulate connection start/duration and energy with GMM
#'
#' 2. Approximate connection start and duration according to time resolution and add connection end
#'
#' 3. Assign a charging power to every sessions
#'
#' 4. Approximate the charging hours, energy and charging end according to time resolution, the power and the connection duration
#'
simulate_sessions <- function(evmodel, sessions_day, charging_powers, dates, resolution) {

  if (sum(sessions_day[["n_sessions"]]) == 0) {
    message("No EV sessions to simulate")
    return( tibble() )
  }

  ev_models <- evmodel[["models"]]
  connection_log <- evmodel[['metadata']][['connection_log']]
  energy_log <- evmodel[['metadata']][['energy_log']]
  tzone_model <- evmodel[['metadata']][['tzone']]

  if (!("charging_rate" %in% colnames(ev_models$user_profiles[[1]]$energy_models[[1]]))) {
    message("Warning: old format of EV models")
  }

  dates_dttm <- round_date(with_tz(as_datetime(dates), tzone = tzone_model), unit = 'day')
  ev_models <- left_join(ev_models, sessions_day, by = 'time_cycle')

  simulated_sessions <- map_dfr(
    dates_dttm,
    ~ get_day_sessions(.x, ev_models, connection_log, energy_log, charging_powers)
  )

  simulated_sessions <- simulated_sessions %>%
    mutate(
      ConnectionStartDateTime = round_date(.data$start_dt, unit = paste(resolution, "minutes")),
      ConnectionHours = round_to_interval(.data$duration, 1/60), # Rounded to 1-minute resolution
      ConnectionEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ConnectionHours),
      Power = .data$power,
      Energy = .data$energy
    ) %>%
    adapt_charging_features(resolution) %>%
    drop_na() %>%
    arrange(.data$ConnectionStartDateTime) %>%
    mutate(Session = paste0('S', row_number())) %>%
    select('Session', 'Profile', 'ConnectionStartDateTime', 'ConnectionEndDateTime',
           'ChargingStartDateTime', 'ChargingEndDateTime', 'Power', 'Energy',
           'ConnectionHours', 'ChargingHours')

  return( simulated_sessions )
}
