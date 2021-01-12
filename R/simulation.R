
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
  # if (n == 0) return(0)
  if (n == 0) n = 1
  energy <- rnorm(n, mu, sigma)
  if (log) energy <- exp(energy)
  # # Negative values replaced by 3 kWh
  # # Potential improvement to avoid negative variables: Log-Normal conversion
  # energy[energy <= 1] <- 3
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
  # if (n == 0) return(matrix(c(0, 0), ncol = 2))
  if (n == 0) n = 1
  connections <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
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
#' @importFrom dplyr tibble
#' @importFrom purrr simplify
#'
estimate_sessions <- function(profile_name, n_sessions, connection_models, energy_models, connection_log, energy_log) {
  estimated_connections <- do.call(
    rbind,
    get_estimated_connections(n_sessions, connection_models, connection_log)
  )
  estimated_energy <- simplify(
    get_estimated_energy(n_sessions, energy_models, energy_log)
  )
  return(tibble(
    start = round(estimated_connections[,1], 2),
    duration = round(estimated_connections[,2], 2),
    energy = round(estimated_energy[1:nrow(estimated_connections)], 2)
  ))
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

  day_models <- ev_models[["models"]][models_month_idx & models_wday_idx][[1]]
  day_n_sessions <- ev_models[["n_sessions"]][models_month_idx & models_wday_idx][[1]]

  if (!(profile_name %in% day_models[["profile"]])) {
    return( NULL )
  }

  profile_idx <- which(day_models[["profile"]] == profile_name)
  profile_n_sessions <- round(day_n_sessions*day_models[["ratio"]][[profile_idx]])

  if (profile_n_sessions == 0) {
    return( NULL )
  }

  estimate_sessions(
    profile_name,
    profile_n_sessions,
    connection_models = day_models[["connection_models"]][[profile_idx]],
    energy_models = day_models[["energy_models"]][[profile_idx]],
    connection_log, energy_log
  ) %>%
    mutate("start_dt" = day + convert_time_num_to_period(.data$start)) %>%
    select(- "start") %>%
    drop_na()
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


#' Simualte sessions given datetime sequence and models
#'
#' @param evmodel object of type `evmodel` (see this [link](https://mcanigueral.github.io/evprof/articles/evmodel.html) for more information)
#' @param sessions_day tibble with variables `time_cycle` (names corresponding to `evmodel$models$time_cycle`) and `n_sessions` (number of daily sessions per day for each time-cycle model)
#' @param charging_powers charging powers proportions (tibble) with two columns: `power` and `ratio`.
#' The powers must be in kW and the ratios between 0 and 1.
#' @param dates datetime vector with dates to simualte (datetime values with hour set to 00:00)
#' @param interval_mins interval of minutes (integer) to round the sessions datetime variables
#' @param connection_log Logical, true if connection models have logarithmic transformations
#' @param energy_log Logical, true if energy models have logarithmic transformations
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map map_dfr set_names
#' @importFrom dplyr mutate any_of row_number arrange left_join
#' @importFrom rlang .data
#' @importFrom xts align.time
#'
simulate_sessions <- function(evmodel, sessions_day, charging_powers, dates, interval_mins, connection_log=TRUE, energy_log=TRUE) {
  ev_models <- evmodel[["models"]]
  ev_models <- left_join(ev_models, sessions_day, by = 'time_cycle')

  # Obtain sessions from all profiles in models
  profiles <- unique(unlist(map(ev_models[["user_profiles"]], ~ .x[["profile"]])))

  sessions_estimated <- map_dfr(
    set_names(profiles, profiles),
    ~get_profile_sessions(.x, dates, ev_models, connection_log, energy_log),
    .id = "Profile"
  )

  # Standardize the variables
  sessions_estimated <- sessions_estimated %>%
    mutate(
      ConnectionStartDateTime = xts::align.time(.data$start_dt, n=60*interval_mins),
      ConnectionHours = round_to_interval(.data$duration, interval_mins/60),
      Power = sample(charging_powers[["power"]], size = nrow(sessions_estimated), prob = charging_powers[["ratio"]], replace = T),
      Energy = round_to_interval(.data$energy, .data$Power*interval_mins/60)
    )

  # Limit energy charged according to power
  limit_idx <- sessions_estimated$Energy > sessions_estimated$Power*sessions_estimated$ConnectionHours
  sessions_estimated[limit_idx, "Energy"] <-
    sessions_estimated[limit_idx, "Power"]*sessions_estimated[limit_idx, "ConnectionHours"]

  # Increase energy resulting in 0kWh due to power round
  e0_idx <- sessions_estimated$Energy <= 0
  sessions_estimated[e0_idx, "Energy"] <- sessions_estimated[e0_idx, "Power"]*interval_mins/60

  # Calculate charging time according to power and energy
  sessions_estimated <- sessions_estimated %>%
    mutate(
      ChargingHours = .data$Energy/.data$Power,
      ConnectionEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ConnectionHours),
      ChargingStartDateTime = .data$ConnectionStartDateTime,
      ChargingEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ChargingHours),
      FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
    ) %>%
    arrange(.data$ConnectionStartDateTime) %>%
    mutate(Session = paste0('S', row_number())) %>%
    select('Profile', 'Session', 'ConnectionStartDateTime', 'ConnectionEndDateTime',
           'ChargingStartDateTime', 'ChargingEndDateTime', 'Power', 'Energy',
           'ConnectionHours', 'FlexibilityHours', 'ChargingHours')

  return( sessions_estimated )
}
