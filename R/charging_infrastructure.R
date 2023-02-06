

#' Assign charging station to charging sessions data set
#'
#' Variable `ChargingStation` and `Socket`will be assigned to the `sessions`
#' tibble with a name pattern being: `names_prefix` + "CHS" + number
#'
#' @param sessions tibble, charging sessions data set
#' @param resolution integer, time resolution in minutes
#' @param min_stations integer, minimum number of charging stations to consider
#' @param names_prefix character, prefix of the charging station names (optional)
#' @param connections_th integer, minimum percentage of time intervals than a
#' certain number of vehicles have been connected.
#' This is used to avoid sizing a charging infrastructure to host for example
#' 100 vehicles when only 5% of time there are more than 80 vehicles connected.
#' Then, setting `connections_th = 5` will ensure that we don't over-size
#' the charging infrastructure. It is recommended to find this value through
#' multiple iterations.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% tibble mutate select all_of row_number filter group_by summarise
#' @importFrom tidyr separate
#' @importFrom lubridate as_datetime tz
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#'
add_charging_infrastructure <- function(sessions, resolution, min_stations = 0, names_prefix = NULL, connections_th = 10) {

  # How many charging stations (of two sockets) do we need?
  connections <- get_n_connections(sessions, resolution = resolution)
  connections_pct <- tibble(
    n_connections = seq(1, max(connections$n_connections))
  ) %>%
    mutate(
      n = map_dbl(.data$n_connections, ~ sum(connections$n_connections >= .x)),
      pct = n/nrow(connections)*100
    )
  n_connections_required <- connections_pct %>%
    filter(.data$pct > connections_th)

  n_required_stations <- ceiling(max(n_connections_required$n_connections)/2)
  if (n_required_stations < min_stations) {
    n_required_stations <- min_stations
  }

  # Name the charging stations and the corresponding sockets
  if (is.null(names_prefix)) {
    names_prefix <- ""
  }
  new_stations_names <- paste(names_prefix, 1:n_required_stations, sep = "CHS")
  socket_names <- paste(new_stations_names, rep(c(1, 2), each = n_required_stations), sep = "-")

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
    drop_na(.data$ChargingSocket) %>%
    separate(.data$ChargingSocket, into = c("ChargingStation", "Socket"), sep = "-")

  message(paste("Discarded", round((1-nrow(sessions_socket)/nrow(sessions))*100, 2), "% of sessions due to infrastructure"))

  return(sessions_socket)
}

