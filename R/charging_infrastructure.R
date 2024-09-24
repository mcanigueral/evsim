

#' Assign a charging station to EV charging sessions
#'
#' Variable `ChargingStation` and `Socket`will be assigned to the `sessions`
#' tibble with a name pattern being: `names_prefix` + "CHS" + number
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param resolution integer, time resolution in minutes
#' @param min_stations integer, minimum number of charging stations to consider
#' @param n_sockets integer, number of sockets per charging station
#' @param names_prefix character, prefix of the charging station names (optional)
#' @param duration_th integer between 0 and 100, minimum share of time (in percentage)
#' of the "occupancy duration curve" (see function `plot_occupancy_duration_curve`).
#' This is used to avoid sizing a charging infrastructure to host for example
#' 100 vehicles when only 5% of time there are more than 80 vehicles connected.
#' Then, setting `duration_th = 5` will ensure that we don't over-size
#' the charging infrastructure for the 100 vehicles.
#' It is recommended to find this value through multiple iterations.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% tibble mutate select all_of row_number filter group_by summarise
#' @importFrom tidyr separate drop_na
#' @importFrom lubridate as_datetime tz
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#'
#' @examples
#' # Assign a `ChargingStation` to every session according to the occupancy
#' sessions_infrastructure <- add_charging_infrastructure(
#'   sessions = head(evsim::california_ev_sessions, 50),
#'   resolution = 60
#' )
#' print(unique(sessions_infrastructure$ChargingStation))
#'
#' # Now without considering the occupancy values that only represent
#' # a 10% of the time
#' sessions_infrastructure <- add_charging_infrastructure(
#'   sessions = head(evsim::california_ev_sessions, 50),
#'   resolution = 60, duration_th = 10
#' )
#' print(unique(sessions_infrastructure$ChargingStation))
#'
#'
add_charging_infrastructure <- function(sessions, resolution = 15, min_stations = 0, n_sockets = 2, names_prefix = NULL, duration_th = 0) {

  # How many charging stations (of two sockets) do we need?
  connections <- sessions %>%
    mutate(Profile = "n_connections") %>%
    get_occupancy(resolution = resolution, by = "Profile", mc.cores = 1)

  connections_pct <- get_occupancy_curve_data(connections$n_connections)

  n_connections_required <- connections_pct %>%
    filter(.data$pct >= duration_th)

  n_required_stations <- ceiling(max(n_connections_required$n_connections)/n_sockets)
  if (n_required_stations < min_stations) {
    n_required_stations <- min_stations
  }

  # Name the charging stations and the corresponding sockets
  if (is.null(names_prefix)) {
    names_prefix <- ""
  }
  new_stations_names <- paste(names_prefix, 1:n_required_stations, sep = "CHS")
  socket_names <- paste(new_stations_names, rep(seq_len(n_sockets), each = n_required_stations), sep = "-")

  # Iterate over all time slots assigning every session to a charging socket
  free_sockets <- socket_names
  sessions_socket <- sessions %>% mutate(idx = row_number())
  sessions_socket$ChargingSocket <- NA
  dttm_seq <- sort(unique(c(sessions$ConnectionStartDateTime, sessions$ConnectionEndDateTime)))
  for (dttm in dttm_seq) {
    # dttm <- connections$datetime[60]
    # Add the sockets of ending sessions to the vector of free sockets
    ending_sessions <- sessions_socket %>%
      filter(
        .data$ConnectionEndDateTime == dttm
      ) %>%
      drop_na()
    if (nrow(ending_sessions) > 0) {
      free_sockets <- c(free_sockets, ending_sessions$ChargingSocket)
    }

    # Assign to the starting sessions the free sockets
    starting_sessions <- sessions_socket %>%
      filter(
        .data$ConnectionStartDateTime == dttm
      )
    if (nrow(starting_sessions) > 0) {
      n_free_sockets <- length(free_sockets)
      if (n_free_sockets == 0) {
        next
      }
      n_assigned_sockets <- pmin(nrow(starting_sessions), n_free_sockets)
      sessions_socket$ChargingSocket[
        starting_sessions$idx[1:n_assigned_sockets]
      ] <- free_sockets[1:n_assigned_sockets]

      # Remove the assigned sockets from the vector of free sockets
      if (n_assigned_sockets < n_free_sockets) {
        free_sockets <- free_sockets[(n_assigned_sockets+1):n_free_sockets]
      } else {
        free_sockets <- c()
      }
    }
  }

  sessions_socket <- sessions_socket %>%
    drop_na("ChargingSocket") %>%
    separate("ChargingSocket", into = c("ChargingStation", "Socket"), sep = "-")

  message(paste("Discarded", round((1-nrow(sessions_socket)/nrow(sessions))*100, 2), "% of sessions due to infrastructure"))

  return(sessions_socket)
}





#' Plot the occupancy duration curve
#'
#' This term is based on the "load duration curve" and is useful to see the
#' behavior of occupancy over the time in your charging installation.
#' The steeper the curve, the shorter the duration that higher number of connections
#' are sustained. Conversely, the flatter the curve, the longer the duration that
#' higher number of connections are sustained.
#' This information is crucial for various purposes, such as infrastructure planning,
#' capacity sizing, and resource allocation.
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
#' @return ggplot
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom purrr map set_names list_rbind
#'
#' @examples
#' library(dplyr)
#'
#' sessions <- head(evsim::california_ev_sessions_profiles, 100)
#' plot_occupancy_duration_curve(
#'   sessions,
#'   by = "Profile",
#'   resolution = 15
#' )
#'
plot_occupancy_duration_curve <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15, mc.cores = 1) {
  connections <- get_occupancy(
    sessions,
    dttm_seq = dttm_seq,
    by = by,
    resolution = resolution,
    mc.cores = mc.cores
  )

  connections_curves <- map(
    unique(sessions[[by]]) %>% set_names(),
    ~ get_occupancy_curve_data(connections[[.x]])
  ) %>%
    list_rbind(names_to = by)

  connections_curves %>%
    ggplot(aes(x = .data$pct, y = .data$n_connections, color = .data[[by]], group = .data[[by]])) +
    geom_line() +
    labs(x = "Share of time (%)", y = "Vehicles connected",
         title = "Occupancy duration curve")
}


get_occupancy_curve_data <- function(vct) {
  dplyr::tibble(
    n_connections = seq(min(vct), max(vct)),
    pct = round(purrr::map_dbl(.data$n_connections, ~ sum(vct >= .x)/length(vct)*100), 2)
  ) %>%
    dplyr::group_by(.data$pct) %>%
    dplyr::summarise(n_connections = min(.data$n_connections)) # Just one value of kW per percentage
}


