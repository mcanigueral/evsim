
# Demand ------------------------------------------------------------------

#' Obtain demand from a starting dttm value and certain duration interval
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param timeslot datetime, time slot of the requested demand
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#'
#' @return tibble
#'
#' @importFrom dplyr %>% filter group_by summarise mutate sym
#' @importFrom tidyr spread
#' @importFrom rlang .data
#'
#' @details This function is only valid if charging start/end times of sessions are aligned to a specific time-interval
#'
get_interval_demand <- function(sessions, timeslot, by = c("Profile", "Session")) {
  sessions %>%
    filter(.data$ChargingStartDateTime <= timeslot, timeslot < .data$ChargingEndDateTime) %>%
    group_by(!!sym(by)) %>%
    summarise(Power = sum(.data$Power)) %>%
    spread(!!sym(by), .data$Power) %>%
    mutate(datetime = timeslot)
}


#' Obtain timeseries demand from sessions dataset
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the datetime variable of the returned time-series data frame
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% left_join select everything rename mutate_if
#' @importFrom lubridate is.timepoint round_date
#' @importFrom purrr map2_dfr
#'
get_demand <- function(sessions, dttm_seq, by = "Profile") {
  interval_mins <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)

  sessions <- sessions %>%
    mutate_if(is.timepoint, round_date, paste(interval_mins, "minutes"))

  demand <- map_dfr(dttm_seq, ~get_interval_demand(sessions, .x, by))

  demand_df <- tibble(datetime = dttm_seq) %>% left_join(demand, by = "datetime")
  demand_df <- replace(demand_df, is.na(demand_df), 0)

  return( demand_df )
}


