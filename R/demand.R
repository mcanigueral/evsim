
# Utils -------------------------------------------------------------------

#' Change charging features with new charging power distribution
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param power_rates numeric vector with different charging power rates
#' @param power_prob numeric vector with the probability of the charging power rates with the corresponding order
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
change_charging_features <- function(sessions, power_rates, power_prob) {
  # Charging power
  if (length(power_rates) > 1) {
    sessions[['Power']] <- sample(power_rates, size = nrow(sessions), prob = power_prob, replace = T)
  } else {
    sessions[['Power']] <- rep(power_rates, nrow(sessions))
  }
  sessions %>%
    mutate(
      ChargingHours = pmin(.data$Energy/.data$Power, .data$ConnectionHours), # Limit ChargingHours by ConnectionHours
      Energy = .data$Power * .data$ChargingHours, # Energy must change if ChargingHours was limited by ConnectionHours
      ChargingStartDateTime = .data$ConnectionStartDateTime,
      ChargingEndDateTime = .data$ChargingStartDateTime + convert_time_num_to_period(.data$ChargingHours)
    )
}

#' Approximate sessions to perfect power steps
#'
#' @param sessions tibble, sessions data set
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables
#' @param power_interval numeric, interval of power approximation, in kW
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr mutate %>%
#' @importFrom rlang .data
#' @importFrom lubridate round_date
#'
approximate_sessions <- function(sessions, resolution = 15, power_interval = 0.01) {
  sessions %>%
    mutate(
      ConnectionStartDateTime = round_date(.data$ConnectionStartDateTime, paste(resolution, "minutes")),
      ConnectionEndDateTime = round_date(.data$ConnectionEndDateTime, paste(resolution, "minutes")),
      ChargingStartDateTime = round_date(.data$ChargingStartDateTime, paste(resolution, "minutes")),
      ChargingEndDateTime = round_date(.data$ChargingEndDateTime, paste(resolution, "minutes")),
      Power = round_to_interval(.data$Power, power_interval),
      ConnectionHours = as.numeric(.data$ConnectionEndDateTime - .data$ConnectionStartDateTime, units='hours'),
      ChargingHours = as.numeric(.data$ChargingEndDateTime - .data$ChargingStartDateTime, units='hours'),
      Energy = .data$Power*.data$ChargingHours
    )
}



# Demand ------------------------------------------------------------------

#' Obtain timeseries demand from sessions dataset
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the datetime variable of the returned time-series data frame
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables. If `dttm_seq` is defined this parameter is ignored.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr left_join tibble sym mutate_if
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom lubridate floor_date days is.timepoint
#'
#' @details This function is only valid if charging start/end times of sessions are aligned to a specific time-interval.
#' For this purpose use `approximate_sessions` function.
#'
get_demand <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15) {
  if (is.null(dttm_seq)) {
    dttm_seq <- seq.POSIXt(
      from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
      to = floor_date(max(sessions$ConnectionEndDateTime), 'day')+days(1),
      by = paste(resolution, 'min')
    )
  } else {
    resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
  }
  sessions_aligned <- sessions %>%
    mutate_if(is.timepoint, floor_date, paste(resolution, 'min'))

  demand <- left_join(
    tibble(datetime = dttm_seq),
    map_dfr(dttm_seq, ~ get_interval_demand(sessions_aligned, .x, by)) %>%
      pivot_wider(names_from = !!sym(by), values_from = .data$Power, values_fill = 0),
    by = 'datetime'
  )
  return( replace(demand, is.na(demand), 0) )
}


get_interval_demand <- function(sessions, timeslot, by) {
  sessions %>%
    dplyr::filter(.data$ChargingStartDateTime <= timeslot, timeslot < .data$ChargingEndDateTime) %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(Power = sum(.data$Power)) %>%
    dplyr::mutate(datetime = timeslot)
}

