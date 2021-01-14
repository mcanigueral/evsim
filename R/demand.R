
# Demand ------------------------------------------------------------------

#' Obtain demand from a starting dttm value and certain duration interval
#'
#' @param sessions sessions data set in standard format
#' @param dttm_start datetime value starting the demand calculation
#' @param interval_mins numeric, duration (in minutes) of the demand interval
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#'
#' @return tibble
#'
#' @importFrom dplyr %>% filter group_by summarise mutate sym
#' @importFrom tidyr spread
#' @importFrom lubridate minutes
#' @importFrom rlang .data
#'
get_interval_demand <- function(sessions, dttm_start, interval_mins, by = c("Profile", "Session")) {
  sessions %>%
    filter(
      .data$ConnectionStartDateTime <= dttm_start & .data$ChargingEndDateTime >= (dttm_start + minutes(interval_mins))
    ) %>%
    group_by(!!sym(by)) %>%
    summarise(Power = sum(.data$Power)) %>%
    mutate(datetime = dttm_start) %>%
    spread(!!sym(by), .data$Power)
}


#' Obtain timeseries demand from sessions dataset
#'
#' @param sessions sessions data set in standard format
#' @param dttm_seq vector with a sequence of datetime values
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% left_join select everything rename mutate_if
#' @importFrom lubridate is.timepoint
#' @importFrom xts align.time
#' @importFrom purrr map2_dfr
#'
get_demand <- function(sessions, dttm_seq, by = "Profile") {
  interval_mins <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)

  sessions <- sessions %>%
    mutate_if(is.timepoint, xts::align.time, interval_mins*60)

  demand <- tibble(datetime = dttm_seq) %>%
    left_join(
      map_dfr(dttm_seq, ~get_interval_demand(sessions, .x, interval_mins, by)),
      by = "datetime"
    )

  demand <- replace(demand, is.na(demand), 0)

  return( demand )
}


