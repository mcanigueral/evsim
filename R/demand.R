
# Utils -------------------------------------------------------------------

#' Interactive plot for time-series tibbles
#'
#' First column of the `df` tibble must be a `datetime` or date variable.
#' The rest of columns must be numeric of the same units. This functions makes
#' use of `dygraphs` package to generate an HTML Dygraphs plot.
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param title character, title of the plot (accepts HTML code)
#' @param xlab character, X axis label (accepts HTML code)
#' @param ylab character, Y axis label (accepts HTML code)
#' @param legend_show character, when to display the legend.
#' Specify "always" to always show the legend.
#' Specify "onmouseover" to only display it when a user mouses over the chart.
#' Specify "follow" to have the legend show as overlay to the chart which follows the mouse.
#' The default behavior is "auto", which results in "always" when more than one series
#' is plotted and "onmouseover" when only a single series is plotted.
#' @param legend_width integer, width (in pixels) of the div which shows the legend.
#' @param group character, dygraphs group to associate this plot with. The x-axis zoom level of dygraphs plots within a group is automatically synchronized.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#' @param ... extra arguments to pass to `dygraphs::dyOptions` function.
#'
#' @return dygraph
#' @export
#'
#' @importFrom dygraphs dygraph dyLegend dyOptions dyCSS
#'
#' @examples
#' suppressMessages(library(lubridate))
#' suppressMessages(library(dplyr))
#'
#' # Get demand with the complete datetime sequence from the sessions
#' sessions <- head(evsim::california_ev_sessions, 100)
#' demand <- get_demand(
#'   sessions,
#'   by = "Session",
#'   resolution = 60
#' )
#' demand %>% plot_ts()
#'
plot_ts <- function(df, title = NULL, xlab = NULL, ylab = NULL,
                    legend_show = "auto", legend_width = 250,
                    group = NULL, width = NULL, height = NULL, ...) {
  dygraph(df, main = title, xlab = xlab, ylab = ylab, group = group, width = width, height = height) %>%
    dyLegend(show = legend_show, width = legend_width, showZeroValues = FALSE) %>%
    dyOptions(retainDateWindow = TRUE, useDataTimezone = TRUE, ...) %>%
    dyCSS(system.file("www", "dystyle.css", package = "evsim"))
}


#' Is the sessions data set aligned in time?
#'
#' Checks if sessions time variables (only connection/charging start times) are
#' aligned with a specific time resolution.
#'
#' @param sessions tibble, sessions data set in standard format marked by `evprof` package
#' @param resolution integer, time resolution (in minutes) of the time slots
#'
#' @return logical
#' @keywords internal
#'
is_aligned <- function(sessions, resolution) {
  connection_start_dt <- sessions$ConnectionStartDateTime
  if (sum(unique(lubridate::minute(connection_start_dt)) %% resolution) == 0) {
    return( TRUE )
  } else {
    return( FALSE )
  }
}



# Demand ------------------------------------------------------------------

#' Expand sessions along time slots
#'
#' Every session in `sessions` is divided in multiple time slots
#' with the corresponding `Power` consumption, among other variables.
#'
#' @param sessions tibble, sessions data set in standard format marked by `evprof` package
#' @param resolution integer, time resolution (in minutes) of the time slots
#'
#' @importFrom dplyr  %>% mutate row_number
#' @importFrom purrr map_dfr
#'
#' @return tibble
#' @export
#'
#' @details
#' The `Power` value is calculated for every time slot according to the original
#' required energy. The columns `PowerNominal`, `EnergyRequired` and
#' `FlexibilityHours` correspond to the values of the original session, and not
#' to the expanded session in every time slot. The column `ID` shows the number
#' of the time slot corresponding to the original session.
#'
#'
#' @examples
#' library(dplyr)
#'
#' sessions <- head(evsim::california_ev_sessions, 10)
#' expand_sessions(
#'   sessions,
#'   resolution = 60
#' )
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
      PowerNominal = session$Power,
      EnergyRequired = session$Energy
    ) %>%
    select(all_of(c(
      "Session", "Timeslot", "Power", "PowerNominal", "EnergyRequired"
    )))

  # Power
  full_power_timeslots <- trunc(session$ChargingHours*60/resolution)
  full_power_energy <- session$Power*full_power_timeslots/(60/resolution)
  power_vct <- round(c(
    rep(session$Power, times = full_power_timeslots),
    (session$Energy - full_power_energy)/(resolution/60)
  ), 2)[seq_len(nrow(session_expanded))]
  session_expanded$Power <- replace(power_vct, is.na(power_vct), 0)

  # ConnectionHoursLeft
  session_expanded$ConnectionHoursLeft <- seq(
    from = session$ConnectionHours,
    length.out = ceiling(session$ConnectionHours*60/resolution),
    by= -resolution/60
  )

  return( session_expanded )
}


#' Time-series EV demand
#'
#' Obtain time-series of EV demand from sessions data set
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the `datetime`
#' variable of the returned time-series data frame.
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables.
#' If `dttm_seq` is defined this parameter is ignored.
#' @param mc.cores integer, number of cores to use.
#' Must be at least one, and parallelization requires at least two cores.
#'
#' @return time-series tibble with first column of type `datetime`
#' @export
#'
#' @importFrom dplyr tibble sym select_if group_by summarise arrange right_join distinct filter between
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate floor_date days month
#' @importFrom parallel detectCores mclapply
#' @importFrom purrr list_rbind
#'
#' @details
#' Note that the time resolution of variables `ConnectionStartDateTime` and
#' `ChargingStartDateTime` must coincide with
#' `resolution` parameter. For example, if a charging session in `sessions` starts charging
#' at 15:32 and `resolution = 15`, the load of this session won't be computed. To solve this,
#' the function automatically aligns charging sessions' start time according to
#' `resolution`, so following the previous example the session would start at 15:30.
#'
#'
#' @examples
#' suppressMessages(library(lubridate))
#' suppressMessages(library(dplyr))
#'
#' # Get demand with the complete datetime sequence from the sessions
#' sessions <- head(evsim::california_ev_sessions, 100)
#' demand <- get_demand(
#'   sessions,
#'   by = "Session",
#'   resolution = 60
#' )
#' demand %>% plot_ts(ylab = "EV demand (kW)")
#'
#' # Get demand with a custom datetime sequence and resolution of 15 minutes
#' sessions <- head(evsim::california_ev_sessions_profiles, 100)
#' dttm_seq <- seq.POSIXt(
#'   as_datetime(dmy(08102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
#'   as_datetime(dmy(11102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
#'   by = "15 mins"
#' )
#' demand <- get_demand(
#'   sessions,
#'   dttm_seq = dttm_seq,
#'   by = "Profile",
#'   resolution = 15
#' )
#' demand %>% plot_ts(ylab = "EV demand (kW)")
#'
get_demand <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15, mc.cores = 1) {

  if (nrow(sessions) == 0) {
    stop("Error: `sessions` can't be an empty tibble.")
  }

  demand_vars <- unique(sessions[[by]])

  # Multi-processing parameter check
  if (mc.cores > detectCores(logical = FALSE) | mc.cores < 1) {
    mc.cores <- 1
  }
  my.mclapply <- switch(
    Sys.info()[['sysname']], # check OS
    Windows = {mclapply.windows}, # case: windows
    Linux   = {mclapply}, # case: linux
    Darwin  = {mclapply} # case: mac
  )

  # Definition of `dttm_seq` and `resolution`
  if (is.null(dttm_seq)) {
    dttm_seq <- seq.POSIXt(
      from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
      to = floor_date(max(sessions$ConnectionEndDateTime), 'day') + days(1),
      by = paste(resolution, 'min')
    )
  } else {
    resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
    sessions <- sessions %>%
      filter(
        between(.data$ChargingStartDateTime, dttm_seq[1], dttm_seq[length(dttm_seq)])
      )
  }

  if (nrow(sessions) == 0) {

    demand <- tibble(datetime = dttm_seq)

  } else {

    # Remove sessions that are not consuming in certain time slots
    sessions <- sessions %>%
      filter(.data$Power > 0)

    # Align time variables to current time resolution
    if (!is_aligned(sessions, resolution)) {
      message(paste0("Warning: charging sessions are aligned to ", resolution, "-minute resolution."))
      sessions <- sessions %>%
        adapt_charging_features(time_resolution = resolution)
    }

    # Expand sessions that are connected more than 1 time slot
    sessions_to_expand <- sessions %>%
      filter(.data$ConnectionHours > resolution/60) %>%
      mutate(Month = month(.data$ConnectionStartDateTime))

    if (nrow(sessions_to_expand) > 0) {

      # Expand sessions
      if (mc.cores == 1) {
        sessions_expanded <- sessions_to_expand %>%
          expand_sessions(resolution = resolution)
      } else {
        sessions_expanded <- sessions_to_expand  %>%
          split(sessions_to_expand$Month) %>%
          my.mclapply(
            expand_sessions, resolution = resolution, mc.cores = mc.cores
          ) %>%
          list_rbind()
      }

      # Join all sessions together
      sessions_expanded <- sessions_expanded %>%
        bind_rows(
          sessions %>%
            filter(.data$ConnectionHours <= resolution/60) %>%
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
      arrange(factor(!!sym(by), levels = unique(sessions[[by]]))) %>%
      pivot_wider(names_from = !!sym(by), values_from = 'Power', values_fill = 0) %>%
      right_join(
        tibble(datetime = dttm_seq),
        by = 'datetime'
      ) %>%
      arrange(.data$datetime)
  }

  # Check if some `by` variable is not in the tibble, then add zeros
  demand_vars_to_add <- setdiff(demand_vars, colnames(demand))
  if (length(demand_vars_to_add) > 0) {
    demand[demand_vars_to_add] <- 0
  }

  # Fill gaps
  demand <- replace(demand, is.na(demand), 0)

  return( demand )
}


# Occupancy ---------------------------------------------------------------

#' Time-series EV occupancy
#'
#' Obtain time-series of simultaneously connected EVs from sessions data set
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the `datetime`
#' variable of the returned time-series data frame.
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param resolution integer, time resolution (in minutes) of the sessions datetime variables.
#' If `dttm_seq` is defined this parameter is ignored.
#' @param mc.cores integer, number of cores to use.
#' Must be at least one, and parallelization requires at least two cores.
#'
#' @return time-series tibble with first column of type `datetime`
#' @export
#'
#' @importFrom dplyr tibble sym select_if group_by summarise arrange right_join distinct filter between
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate floor_date days round_date month
#' @importFrom parallel detectCores mclapply
#' @importFrom purrr list_rbind
#'
#' @details
#' Note that the time resolution of variable `ConnectionStartDateTime` must coincide with
#' `resolution` parameter. For example, if a charging session in `sessions` starts charging
#' at 15:32 and `resolution = 15`, the load of this session won't be computed. To solve this,
#' the function automatically aligns charging sessions' start time according to
#' `resolution`, so following the previous example the session would start at 15:30.
#'
#' @examples
#' library(lubridate)
#' library(dplyr)
#'
#' # Get occupancy with the complete datetime sequence from the sessions
#' sessions <- head(evsim::california_ev_sessions, 100)
#' connections <- get_occupancy(
#'   sessions,
#'   by = "ChargingStation",
#'   resolution = 60
#' )
#' connections %>%
#'   plot_ts(ylab = "Vehicles connected", legend_show = "onmouseover")
#'
#' # Get occupancy with a custom datetime sequence and resolution of 15 minutes
#' sessions <- head(evsim::california_ev_sessions_profiles, 100)
#' dttm_seq <- seq.POSIXt(
#'   as_datetime(dmy(08102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
#'   as_datetime(dmy(11102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
#'   by = "15 mins"
#' )
#' connections <- get_occupancy(
#'   sessions,
#'   dttm_seq = dttm_seq,
#'   by = "Profile"
#' )
#' connections %>%
#'   plot_ts(ylab = "Vehicles connected", legend_show = "onmouseover")
#'
get_occupancy <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15, mc.cores = 1) {

  if (nrow(sessions) == 0) {
    stop("Error: `sessions` can't be an empty tibble.")
  }

  occupancy_vars <- unique(sessions[[by]])

  # Multi-processing parameter check
  if (mc.cores > detectCores(logical = FALSE) | mc.cores < 1) {
    mc.cores <- 1
  }
  my.mclapply <- switch(
    Sys.info()[['sysname']], # check OS
    Windows = {mclapply.windows}, # case: windows
    Linux   = {mclapply}, # case: linux
    Darwin  = {mclapply} # case: mac
  )

  # Definition of `dttm_seq` and `resolution`
  if (is.null(dttm_seq)) {
    dttm_seq <- seq.POSIXt(
      from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
      to = floor_date(max(sessions$ConnectionEndDateTime), 'day') + days(1),
      by = paste(resolution, 'min')
    )
  } else {
    resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
    sessions <- sessions %>%
      filter(
        between(.data$ConnectionStartDateTime, dttm_seq[1], dttm_seq[length(dttm_seq)])
      )
  }

  if (nrow(sessions) == 0) {

    n_connections <- tibble(datetime = dttm_seq)

  } else {

    # Align time variables to current time resolution
    if (!is_aligned(sessions, resolution)) {
      message(paste0("Warning: charging sessions are aligned to ", resolution, "-minute resolution."))
      sessions <- sessions %>%
        adapt_charging_features(time_resolution = resolution)
    }

    # Expand sessions that are connected more than 1 time slot
    sessions_to_expand <- sessions %>%
      filter(.data$ConnectionHours > resolution/60) %>%
      mutate(Month = month(.data$ConnectionStartDateTime))

    if (nrow(sessions_to_expand) > 0) {

      # Expand sessions
      if (mc.cores == 1) {
        sessions_expanded <- sessions_to_expand %>%
          expand_sessions(resolution = resolution)
      } else {
        sessions_expanded <- sessions_to_expand  %>%
          split(sessions_to_expand$Month) %>%
          my.mclapply(
            expand_sessions, resolution = resolution, mc.cores = mc.cores
          ) %>%
          list_rbind()
      }

      # Join all sessions together
      sessions_expanded <- sessions_expanded %>%
        bind_rows(
          sessions %>%
            filter(.data$ConnectionHours <= resolution/60) %>%
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
      arrange(factor(!!sym(by), levels = unique(sessions[[by]]))) %>%
      pivot_wider(names_from = !!sym(by), values_from = 'n_connections', values_fill = 0) %>%
      right_join(
        tibble(datetime = dttm_seq),
        by = 'datetime'
      ) %>%
      arrange(.data$datetime)
  }

  # Check if some `by` variable is not in the tibble, then add zeros
  occupancy_vars_to_add <- setdiff(occupancy_vars, colnames(n_connections))
  if (length(occupancy_vars_to_add) > 0) {
    n_connections[occupancy_vars_to_add] <- 0
  }

  # Fill gaps
  n_connections <- replace(n_connections, is.na(n_connections), 0)

  return( n_connections )
}


