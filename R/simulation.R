
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

#' Change charging features with new charging power distribution
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param power_rates numeric vector with different charging power rates
#' @param power_prob numeric vector with the probability of the charging power rates with the corresponding order
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
add_charging_features <- function(sessions, power_rates, power_prob, resolution = 15) {
  # Charging power
  if (length(power_rates) > 1) {
    sessions$Power <- sample(power_rates, size = nrow(sessions), prob = power_prob, replace = T)
  } else {
    sessions$Power <- rep(power_rates, nrow(sessions))
  }
  sessions %>%
    mutate(
      ChargingHours = pmin(
        # round_to_interval(.data$Energy/.data$Power, resolution/60),
        .data$Energy/.data$Power,
        .data$ConnectionHours
      ), # Limit ChargingHours by ConnectionHours
      Energy = .data$Power * .data$ChargingHours, # Energy must change if ChargingHours was limited by ConnectionHours
      ChargingStartDateTime = .data$ConnectionStartDateTime,
      ChargingEndDateTime = .data$ChargingStartDateTime + convert_time_num_to_period(.data$ChargingHours)
    )
}


#' Adapt charging features according to existing charging powers
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
      Energy = .data$Power * .data$ChargingHours,
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
  # After applying clusters' ratios we may have `n = 0`
  # However we should simulate at least 1 sessions since for lower values
  # of `n_sessions_day` and multiple clusters, we would never have positive
  # values of `n`
  if (n == 0) n = 1
  energy <- rnorm(n, mu, sigma)
  while (any(energy <= 0)) {
    energy[energy <= 0] <- rnorm(sum(energy <= 0), mu, sigma)
  }
  if (log) energy <- exp(energy)
  return( energy )
}

#' Estimate energy given energy models tibble
#'
#' @param n number of sessions
#' @param energy_models energy models tibble
#' @param log Logical, true if models have logarithmic transformation
#'
#' @return list of numeric vectors
#'
#' @importFrom purrr pmap
#'
get_estimated_energy <- function(n, energy_models, log) {
  return(pmap(
    energy_models,
    ~ estimate_energy(round(n*..3), ..1, ..2, log)
  ))
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
  # After applying clusters' ratios we may have `n = 0`
  # However we should simulate at least 1 sessions since for lower values
  # of `n_sessions_day` and multiple clusters, we would never have positive
  # values of `n`
  if (n == 0) n = 1
  connections <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
  while (any(connections[,1] < 0)) {
    connections[connections[,1] < 0, 1] <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)[,1]
  }
  while (any(connections[,2] <= 0)) {
    connections[connections[,2] <= 0, 2] <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)[,2]
  }
  if (log) connections <- exp(connections)
  return( connections )
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
    ~ estimate_connection(round(n*..3), ..1, ..2, log)
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
#'
#' @return tibble
#'
#' @importFrom dplyr tibble bind_rows
#' @importFrom purrr simplify
#'
estimate_sessions <- function(profile_name, n_sessions, connection_models, energy_models, connection_log, energy_log) {
  ev_sessions <- tibble()
  n_sessions_objective <- n_sessions - nrow(ev_sessions)

  while (n_sessions_objective > 0) {
    estimated_connections <- do.call(
      rbind,
      get_estimated_connections(n_sessions_objective, connection_models, connection_log)
    )
    estimated_energy <- simplify(
      get_estimated_energy(n_sessions_objective, energy_models, energy_log)
    )
    estimated_sessions <- tibble(
      start = round(estimated_connections[,1], 2),
      duration = round(estimated_connections[,2], 2),
      energy = round(estimated_energy[1:nrow(estimated_connections)], 2)
    )

    ev_sessions <- bind_rows(ev_sessions, estimated_sessions)

    n_sessions_objective <- n_sessions - nrow(ev_sessions)
  }

  return(ev_sessions)
}


#' Get sessions for a specific day and profile
#'
#' @param profile_name profile name
#' @param day day as datetime with hour 00:00
#' @param ev_models tibble with profiles models according to calendar
#' @param connection_log Logical, true if connection models have logarithmic transformations
#' @param energy_log Logical, true if energy models have logarithmic transformations
#'
#' @return tibble
#'
#' @importFrom purrr map_lgl
#' @importFrom lubridate month wday
#' @importFrom dplyr %>% mutate select
#' @importFrom tidyr drop_na
#'
get_profile_day_sessions <- function(profile_name, day, ev_models, connection_log, energy_log) {

  month_day <- month(day)
  wday_day <- wday(day, week_start = 1)

  models_month_idx <- purrr::map_lgl(ev_models[["months"]], ~ month_day %in% .x)
  models_wday_idx <- purrr::map_lgl(ev_models[["wdays"]], ~ wday_day %in% .x)

  day_models <- ev_models[["user_profiles"]][models_month_idx & models_wday_idx][[1]]
  day_n_sessions <- ev_models[["n_sessions"]][models_month_idx & models_wday_idx][[1]]

  if (!(profile_name %in% day_models[["profile"]])) {
    return( NULL )
  }

  profile_idx <- which(day_models[["profile"]] == profile_name)
  profile_n_sessions <- round(day_n_sessions*day_models[["ratio"]][[profile_idx]])

  if (profile_n_sessions == 0) {
    return( NULL )
  }

  estimated_sessions <- estimate_sessions(
    profile_name,
    profile_n_sessions,
    connection_models = day_models[["connection_models"]][[profile_idx]],
    energy_models = day_models[["energy_models"]][[profile_idx]],
    connection_log, energy_log
  ) %>%
    mutate(start_dt = day + convert_time_num_to_period(.data$start)) %>%
    select(- "start")

  return( estimated_sessions )
}

#' Get profile sessions
#'
#' @param profile_name profile name
#' @param dates datetime vector with dates to simualte (datetime values with hour set to 00:00)
#' @param ev_models profiles models
#' @param connection_log Logical, true if connection models have logarithmic transformations
#' @param energy_log Logical, true if energy models have logarithmic transformations
#'
#' @return tibble
#'
#' @importFrom purrr map_dfr
#'
get_profile_sessions <- function(profile_name, dates, ev_models, connection_log, energy_log) {
  map_dfr(dates, ~get_profile_day_sessions(profile_name, .x, ev_models, connection_log, energy_log))
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

  dates_dttm <- round_date(with_tz(as_datetime(dates), tzone = tzone_model), unit = 'day')

  ev_models <- left_join(ev_models, sessions_day, by = 'time_cycle')

  # Obtain sessions from all profiles in models
  profiles <- unique(unlist(map(ev_models[["user_profiles"]], ~ .x[["profile"]])))

  simulated_sessions <- map_dfr(
    set_names(profiles, profiles),
    ~get_profile_sessions(.x, dates_dttm, ev_models, connection_log, energy_log),
    .id = "Profile"
  )

  simulated_sessions <- simulated_sessions %>%
    mutate(
      ConnectionStartDateTime = round_date(.data$start_dt, unit = paste(resolution, "minutes")),
      # ConnectionHours = round_to_interval(.data$duration, resolution/60),
      ConnectionEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ConnectionHours),
      Energy = .data$energy
    ) %>%
    add_charging_features(charging_powers$power, charging_powers$ratio, resolution) %>%
    arrange(.data$ConnectionStartDateTime) %>%
    mutate(Session = paste0('S', row_number())) %>%
    select('Session', 'Profile', 'ConnectionStartDateTime', 'ConnectionEndDateTime',
           'ChargingStartDateTime', 'ChargingEndDateTime', 'Power', 'Energy',
           'ConnectionHours', 'ChargingHours')

  return( simulated_sessions )
}
