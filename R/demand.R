
# Demand ------------------------------------------------------------------

#' Expand sessions along time slots
#'
#' Every session in `sessions` is divided in multiple time slots
#' with the corresponding `Power` consumption, among other variables.
#'
#' @param sessions tibble, sessions data set in standard format marked by `evprof` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param resolution integer, time resolution (in minutes) of the time slots
#'
#' @importFrom dplyr  %>% mutate row_number
#' @importFrom purrr map_dfr
#'
#' @return tibble
#' @keywords internal
#'
expand_sessions <- function(sessions, resolution) {
  sessions %>%
    split(seq_len(nrow(sessions))) %>%
    map_dfr(
      ~ expand_session(.x, resolution = resolution)
    ) %>%
    mutate(
      ID = row_number()
    )
}


#' Expand a session along time slots within its connection time
#'
#' The `session` is divided in multiple time slots
#' with the corresponding `Power` consumption, among other variables.
#'
#' @param session tibble, sessions data set in standard format marked by `evprof` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param resolution integer, time resolution (in minutes) of the time slots
#'
#' @importFrom dplyr  %>% mutate tibble select all_of
#'
#' @return tibble
#' @keywords internal
#'
expand_session <- function(session, resolution) {
  session_expanded <- tibble(
    Timeslot = seq.POSIXt(
      from = session$ConnectionStartDateTime,
      length.out = ceiling(session$ConnectionHours*60/resolution),
      by = paste(resolution, "mins")
    )
  ) %>%
    mutate(
      Session = session$Session,
      Power = 0,
      NominalPower = session$Power,
      RequiredEnergy = session$Energy,
      FlexibilityHours = session$ConnectionHours - session$ChargingHours
    ) %>%
    select(all_of(c("Session", "Timeslot", "Power", "NominalPower",
                    "RequiredEnergy", "FlexibilityHours")))

  full_power_timeslots <- trunc(session$ChargingHours*60/resolution)
  full_power_energy <- session$Power*full_power_timeslots/(60/resolution)

  power_vct <- round(c(
    rep(session$Power, times = full_power_timeslots),
    (session$Energy - full_power_energy)/(resolution/60)
  ), 2)[seq_len(nrow(session_expanded))]
  session_expanded$Power <- replace(power_vct, is.na(power_vct), 0)

  return( session_expanded )
}


#' Obtain timeseries demand from sessions dataset
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param dttm_seq sequence of datetime values that will be the `datetime`
#' variable of the returned time-series data frame.
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables.
#' If `dttm_seq` is defined this parameter is ignored.
#' @param mc.cores integer, number of cores to use.
#' Must be at least one, and parallelization requires at least two cores.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble sym select_if group_by summarise arrange right_join distinct
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate floor_date days month
#' @importFrom parallel detectCores mclapply
#' @importFrom purrr list_rbind
#'
get_demand <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15, mc.cores = 1) {

  # Parameter check and definition of `dttm_seq` and `resolution`
  if (mc.cores < 1) {
    message("Parameter mc.cores must be at leas 1. Setting mc.cores = 1.")
    mc.cores <- 1
  }
  if (detectCores() <= (mc.cores)) {
    message("Parameter mc.cores too high. Setting mc.cores = 1 to avoid parallelization.")
    mc.cores <- 1
  }

  if (nrow(sessions) == 0) {
    if (is.null(dttm_seq)) {
      message("Must provide sessions or dttm_seq parameter")
      return( NULL )
    } else {
      return( tibble(datetime = dttm_seq, demand = 0) )
    }
  } else {

    sessions <- sessions %>%
      filter(.data$Power > 0) %>%
      adapt_charging_features(time_resolution = resolution)

    if (is.null(dttm_seq)) {
      dttm_seq <- seq.POSIXt(
        from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
        to = floor_date(max(sessions$ConnectionEndDateTime), 'day') + days(1),
        by = paste(resolution, 'min')
      )
    } else {
      resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
    }
  }

  # Expand sessions that are connected more than 1 time slot
  sessions_to_expand <- sessions %>%
    filter(.data$ConnectionHours > resolution/60) %>%
    mutate(Month = month(.data$ConnectionStartDateTime))

  if (nrow(sessions_to_expand) > 0) {

    # Expand sessions
    sessions_expanded <- sessions_to_expand  %>%
      split(sessions_to_expand$Month) %>%
      mclapply(
        expand_sessions, resolution = resolution, mc.cores = mc.cores
      ) %>%
      list_rbind()

    # Join all sessions together
    sessions_expanded <- sessions_expanded %>%
      bind_rows(
        sessions %>%
          filter(!(.data$Session %in% sessions_to_expand$Session)) %>%
          mutate(Timeslot = .data$ConnectionStartDateTime)
      )

  } else {
    sessions_expanded <- sessions %>%
      mutate(Timeslot = .data$ConnectionStartDateTime)
  }

  sessions_expanded <- sessions_expanded %>%
    select(any_of(c('Session', 'Timeslot', 'Power'))) %>%
    left_join(
      sessions %>%
        select('Session', !!sym(by)) %>%
        distinct(),
      by = 'Session'
    )

  # Calculate power demand by time slot and variable `by`
  demand <- sessions_expanded %>%
    group_by(!!sym(by), datetime = .data$Timeslot) %>%
    summarise(Power = sum(.data$Power)) %>%
    pivot_wider(names_from = !!sym(by), values_from = 'Power', values_fill = 0) %>%
    right_join(
      tibble(datetime = dttm_seq),
      by = 'datetime'
    ) %>%
    arrange(.data$datetime)

  return( replace(demand, is.na(demand), 0) )
}


# Occupancy ---------------------------------------------------------------

#' Obtain time series of EV connected from sessions data set
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param dttm_seq sequence of datetime values that will be the `datetime`
#' variable of the returned time-series data frame.
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables.
#' If `dttm_seq` is defined this parameter is ignored.
#' @param mc.cores integer, number of cores to use.
#' Must be at least one, and parallelization requires at least two cores.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble sym select_if group_by summarise arrange right_join distinct
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate floor_date days round_date month
#' @importFrom parallel detectCores mclapply
#' @importFrom purrr list_rbind
#'
get_n_connections <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15, mc.cores = 1) {

  # Parameter check and definition of `dttm_seq` and `resolution`
  if (mc.cores < 1) {
    message("Parameter mc.cores must be at leas 1. Setting mc.cores = 1.")
    mc.cores <- 1
  }
  if (detectCores() <= (mc.cores)) {
    message("Parameter mc.cores too high. Setting mc.cores = 1 to avoid parallelization.")
    mc.cores <- 1
  }

  if (nrow(sessions) == 0) {
    if (is.null(dttm_seq)) {
      message("Must provide sessions or dttm_seq parameter")
      return( NULL )
    } else {
      return( tibble(datetime = dttm_seq, n_connections = 0) )
    }
  } else {

    # Adapt date time variables to time resolution
    sessions <- sessions  %>%
      mutate(
        ConnectionStartDateTime = round_date(.data$ConnectionStartDateTime, paste(resolution, "mins")),
        ConnectionEndDateTime = .data$ConnectionStartDateTime + convert_time_num_to_period(.data$ConnectionHours),
        ChargingHours = round(pmin(.data$Energy/.data$Power, .data$ConnectionHours), 2),
        ChargingStartDateTime = .data$ConnectionStartDateTime,
        ChargingEndDateTime = .data$ChargingStartDateTime + convert_time_num_to_period(.data$ChargingHours)
      )

    if (is.null(dttm_seq)) {
      dttm_seq <- seq.POSIXt(
        from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
        to = floor_date(max(sessions$ConnectionEndDateTime), 'day') + days(1),
        by = paste(resolution, 'min')
      )
    } else {
      resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
    }
  }

  # Expand sessions that are connected more than 1 time slot
  sessions_to_expand <- sessions %>%
    filter(.data$ConnectionHours > resolution/60) %>%
    mutate(Month = month(.data$ConnectionStartDateTime))

  if (nrow(sessions_to_expand) > 0) {

    # Expand sessions
    sessions_expanded <- sessions_to_expand  %>%
      split(sessions_to_expand$Month) %>%
      mclapply(
        expand_sessions, resolution = resolution, mc.cores = mc.cores
      ) %>%
      list_rbind()

    # Join all sessions together
    sessions_expanded <- sessions_expanded %>%
      bind_rows(
        sessions %>%
          filter(!(.data$Session %in% sessions_to_expand$Session)) %>%
          mutate(Timeslot = .data$ConnectionStartDateTime)
      )

  } else {
    sessions_expanded <- sessions %>%
      mutate(Timeslot = .data$ConnectionStartDateTime)
  }

  sessions_expanded <- sessions_expanded %>%
    select(any_of(c('Session', 'Timeslot'))) %>%
    left_join(
      sessions %>%
        select('Session', !!sym(by)) %>%
        distinct(),
      by = 'Session'
    )

  # Calculate the number of EV connections by time slot and variable `by`
  n_connections <- sessions_expanded %>%
    group_by(!!sym(by), datetime = .data$Timeslot) %>%
    summarise(n_connections = n()) %>%
    pivot_wider(names_from = !!sym(by), values_from = 'n_connections', values_fill = 0) %>%
    right_join(
      tibble(datetime = dttm_seq),
      by = 'datetime'
    ) %>%
    arrange(.data$datetime)

  return( replace(n_connections, is.na(n_connections), 0) )
}


