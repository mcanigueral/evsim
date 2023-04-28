
#' `print` method for EV model object of `evprof` class
#'
#' @param x  `evprof` model object
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @keywords internal
#'
print.evmodel <- function(x, ...) {
  m <- x$models
  cat('EV sessions model of class "evprof", created on', as.character(x$metadata$creation), '\n')
  cat('Timezone of the model:', x$metadata$tzone, '\n')
  cat('The Gaussian Mixture Models of EV user profiles are built in:\n')
  cat('  - Connection Models:', if (x$metadata$connection_log) "logarithmic" else "natural", 'scale\n')
  cat('  - Energy Models:', if (x$metadata$energy_log) "logarithmic" else "natural", 'scale\n')
  cat('\nModel composed by', nrow(m), 'time-cycles:\n')
  for (n in 1:nrow(m)) {
    cat(
      '  ', n, '. ', m[['time_cycle']][n], ':',
      '\n     Months = ', if (length(m[['months']][[n]]) == 1) m[['months']][[n]][1] else
        paste0(m[['months']][[n]][1], '-', m[['months']][[n]][length(m[['months']][[n]])]),
      ', Week days = ', if (length(m[['wdays']][[n]]) == 1) m[['wdays']][[n]][1] else
        paste0(m[['wdays']][[n]][1], '-', m[['wdays']][[n]][length(m[['wdays']])]),
      '\n     User profiles = ', paste(m[['user_profiles']][[n]][['profile']], collapse = ", "),
      '\n', sep = ''
    )
  }
}



# Get user profiles distribution ------------------------------------------

#' Get the user profiles distribution from the original data set
#' used to build the model
#'
#' @param evmodel object of class `evprof`
#'
#' @importFrom purrr map_dfr set_names
#' @importFrom dplyr %>% select any_of
#'
get_user_profiles_distribution <- function(evmodel) {
  evmodel$models$user_profiles %>%
    set_names(evmodel$models$time_cycle) %>%
    map_dfr(
      ~ .x %>%
        select(any_of(c('profile', 'ratio', 'power'))),
      .id = 'time_cycle'
    )
}


# Modify the models -------------------------------------------------------

#' Prepare the `evmodel` object ready for the simulation
#'
#' The ratios and default charging power for every user profile,
#' and the sessions per day for every time cycle are included.
#'
#' @param evmodel object of class `evprof`
#' @param sessions_day tibble with variables `time_cycle` (names corresponding to `evmodel$models$time_cycle`) and `n_sessions` (number of daily sessions per day for each time-cycle model)
#' @param user_profiles tibble with variables `time_cycle`, `user_profile`, `ratio` and optionally `power`.
#' The powers must be in kW and the ratios between 0 and 1.
#' The user profiles with a value of `power` will be simulated with this specific charging power.
#' If `power` is `NA` then it is simulated according to the ratios of parameter `charging_powers` in function. `simulate_sessions`.
#'
#' @details If any user profile is not in the `new_ratios` data frame, its corresponding ratio in the `evmodel` object is updated with a `0`
#'
#' @return the updated `evmodel` object
#' @export
#'
#' @importFrom dplyr left_join select %>%
#' @importFrom tidyr nest
#'
prepare_model <- function(evmodel, sessions_day, user_profiles) {

  if (!('power' %in% colnames(user_profiles))) {
    user_profiles['power'] <- NA
  }

  ev_model <- user_profiles %>%
    select('time_cycle', 'profile', 'ratio', 'power') %>%
    nest(.by = 'time_cycle', .key = 'user_profiles') %>%
    left_join(
      select(evmodel, 'time_cycle', 'months', 'wdays'),
      by = 'time_cycle'
    ) %>%
    left_join(
      sessions_day,
      by = 'time_cycle'
    ) %>%
    select('time_cycle', 'months', 'wdays', 'user_profiles', 'n_sessions')

  for (m in 1:nrow(ev_model)) {
    time_cycle_name <- ev_model$time_cycle[[m]]
    if (!(time_cycle_name %in% evmodel$time_cycle)) {
      message(paste("Error: Time cycle", time_cycle_name, "does not exist"))
      return(NULL)
    }
    evmodel_idx <- which(time_cycle_name == evmodel$time_cycle)

    gmm <- left_join(
      ev_model$user_profiles[[m]],
      evmodel$user_profiles[[evmodel_idx]] %>%
        select('profile', 'connection_models', 'energy_models'),
      by = 'profile'
    )

    ev_model[["user_profiles"]][[m]] <- gmm
  }

  return(ev_model)
}



