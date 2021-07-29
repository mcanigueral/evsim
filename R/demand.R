
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

