
# Demand ------------------------------------------------------------------

#' Obtain time-series demand from sessions data set
#'
#' @param sessions tibble, sessions data set in standard format defined by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the datetime variable of the returned time-series data frame
#' @param by character, being 'Profile', 'Session' or 'ChargingStation', existing in the `sessions` data frame.
#' When `by='Profile'` each column corresponds to an EV user profile.
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables. If `dttm_seq` is defined this parameter is ignored.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr left_join tibble sym mutate_if as_tibble
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom lubridate floor_date days
#' @importFrom dtplyr lazy_dt
#' @importFrom data.table as.data.table
#'
get_demand <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15) {

  if (nrow(sessions) == 0) {
    if (is.null(dttm_seq)) {
      message("Must provide sessions or dttm_seq parameter")
      return( NULL )
    } else {
      return( tibble(datetime = dttm_seq, demand = 0) )
    }
  } else {
    if (is.null(dttm_seq)) {
      dttm_seq <- seq.POSIXt(
        from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
        to = floor_date(max(sessions$ConnectionEndDateTime), 'day')+days(1),
        by = paste(resolution, 'min')
      )
    } else {
      resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
    }
  }

  sessions_dt <- lazy_dt(sessions)

  demand <- as_tibble(left_join(
    lazy_dt(tibble(datetime = dttm_seq)),
    lazy_dt(map_dfr(dttm_seq, ~ get_interval_demand(sessions_dt, .x, by, resolution)) %>%
              pivot_wider(names_from = !!sym(by), values_from = .data$demand, values_fill = 0)),
    by = 'datetime'
  ))
  return( replace(demand, is.na(demand), 0) )
}

get_interval_demand <- function(sessions, timeslot, by, resolution) {
  sessions %>%
    dplyr::filter(
      (.data$ChargingStartDateTime <= timeslot & timeslot < .data$ChargingEndDateTime) |
        (.data$ChargingStartDateTime == timeslot & timeslot == .data$ChargingEndDateTime)
    ) %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(demand = sum(
      pmin(as.numeric(.data$ChargingEndDateTime - timeslot, unit = 'hours'), resolution/60)*.data$Power*
        60/resolution # This last term is to convert kWh to average kW
    )) %>%
    dplyr::mutate(datetime = timeslot) %>%
    dplyr::as_tibble()
}


# Occupancy ---------------------------------------------------------------

#' Get number of existing connections
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the datetime variable of the returned time-series data frame
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables. If `dttm_seq` is defined this parameter is ignored.
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dbl
#' @importFrom dplyr tibble
#' @importFrom rlang .data
#' @importFrom lubridate floor_date days
#' @importFrom dtplyr lazy_dt
#'
get_n_connections <- function(sessions, dttm_seq = NULL, resolution = 15) {

  if (nrow(sessions) == 0) {
    if (is.null(dttm_seq)) {
      message("Must provide sessions or dttm_seq parameter")
      return( NULL )
    } else {
      return( tibble(datetime = dttm_seq, n_connections = 0) )
    }
  } else {
    if (is.null(dttm_seq)) {
      dttm_seq <- seq.POSIXt(
        from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
        to = floor_date(max(sessions$ConnectionEndDateTime), 'day')+days(1),
        by = paste(resolution, 'min')
      )
    } else {
      resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
    }
  }

  sessions_dt <- dtplyr::lazy_dt(sessions)

  tibble(
    datetime = dttm_seq,
    n_connections = map_dbl(dttm_seq, ~ get_interval_n_connections(sessions_dt, .x))
  )
}

get_interval_n_connections <- function(sessions, timeslot) {
  nrow(
    sessions %>%
      dplyr::filter(
        (.data$ConnectionStartDateTime <= timeslot & timeslot < .data$ConnectionEndDateTime) |
          (.data$ConnectionStartDateTime == timeslot & timeslot == .data$ConnectionEndDateTime)
      ) %>%
      dplyr::as_tibble()
  )
}



