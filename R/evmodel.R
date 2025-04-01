# Read EV models ---------------------------------------------------------

#' Read EV model
#'
#' Read an EV model JSON file and convert it to object of class `evmodel`
#'
#' @param file path to the JSON file
#'
#' @returns object of class `evmodel`
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom dplyr as_tibble
#'
#' @examples
#' ev_model <- california_ev_model # Model of example
#'
#' save_ev_model(ev_model, file = file.path(tempdir(), "evmodel.json"))
#'
#' read_ev_model(file = file.path(tempdir(), "evmodel.json"))
#'
read_ev_model <- function(file) {
  evmodel <- jsonlite::fromJSON(file)
  class(evmodel) <- "evmodel"
  evmodel$models <- dplyr::as_tibble(evmodel$models)
  evmodel$models$user_profiles <- purrr::map(
    evmodel$models$user_profiles, tidy_models
  )
  return(evmodel)
}

lst_df_to_tbl <- function(df_lst) {
  purrr::map(df_lst, as_tibble)
}

tidy_models <- function(user_models_df) {
  user_models_df <- as_tibble(user_models_df)
  user_models_df$connection_models <- lst_df_to_tbl(user_models_df$connection_models)
  user_models_df$energy_models <- purrr::map(
    user_models_df$energy_models,
    ~ .x %>%
      as_tibble() %>%
      mutate(energy_models = lst_df_to_tbl(energy_models))
  )
  user_models_df
}




#' `print` method for `evmodel` object class
#'
#' @param x  `evmodel` object
#' @param ... further arguments passed to or from other methods.
#'
#' @returns nothing but prints information about the `evmodel` object
#' @export
#' @keywords internal
#'
#' @examples
#' print(california_ev_model)
#'
#'
print.evmodel <- function(x, ...) {
  m <- x$models
  cat('EV sessions model of class "evmodel", created on', as.character(x$metadata$creation), '\n')
  cat('Timezone of the model:', x$metadata$tzone, '\n')
  cat('The Gaussian Mixture Models of EV user profiles are built in:\n')
  cat('  - Connection Models:', if (x$metadata$connection_log) "logarithmic" else "natural", 'scale\n')
  cat('  - Energy Models:', if (x$metadata$energy_log) "logarithmic" else "natural", 'scale\n')
  cat('\nModel composed by', nrow(m), 'time-cycles:\n')
  for (n in seq_len(nrow(m))) {
    cat(
      '  ', n, '. ', m[['time_cycle']][n], ':',
      '\n     Months = ', if (length(m[['months']][[n]]) == 1) m[['months']][[n]][1] else
        paste0(m[['months']][[n]][1], '-', m[['months']][[n]][length(m[['months']][[n]])]),
      ', Week days = ', if (length(m[['wdays']][[n]]) == 1) m[['wdays']][[n]][1] else
        paste0(m[['wdays']][[n]][1], '-', m[['wdays']][[n]][length(m[['wdays']][[n]])]),
      '\n     User profiles = ', paste(m[['user_profiles']][[n]][['profile']], collapse = ", "),
      '\n', sep = ''
    )
  }
}


#' User profiles distribution
#'
#' Get the user profiles distribution from the original data set
#' used to build the model
#'
#' @param evmodel object of class `evmodel`
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dfr set_names
#' @importFrom dplyr %>% select any_of
#'
#' @examples
#' get_user_profiles_distribution(evsim::california_ev_model)
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

#' Prepare the models from the `evmodel` object ready for the simulation
#'
#' The ratios and default charging power for every user profile,
#' and the sessions per day for every time cycle are included.
#'
#' @param ev_models tibble with models from an `evmodel` object
#' @param sessions_day tibble with variables `time_cycle` (names corresponding to `evmodel$models$time_cycle`) and `n_sessions` (number of daily sessions per day for each time-cycle model)
#' @param user_profiles tibble with variables `time_cycle`, `user_profile`, `ratio` and optionally `power`.
#' The powers must be in kW and the ratios between 0 and 1.
#' The user profiles with a value of `power` will be simulated with this specific charging power.
#' If `power` is `NA` then it is simulated according to the ratios of parameter `charging_powers` in function. `simulate_sessions`.
#'
#' @return tibble
#' @keywords internal
#'
#' @importFrom dplyr left_join select %>%
#' @importFrom tidyr nest
#'
prepare_model <- function(ev_models, sessions_day, user_profiles) {

  if (!('power' %in% colnames(user_profiles))) {
    user_profiles['power'] <- NA
  }

  ev_model <- user_profiles %>%
    select('time_cycle', 'profile', 'ratio', 'power') %>%
    nest(.by = 'time_cycle', .key = 'user_profiles') %>%
    left_join(
      select(ev_models, 'time_cycle', 'months', 'wdays'),
      by = 'time_cycle'
    ) %>%
    left_join(
      sessions_day,
      by = 'time_cycle'
    ) %>%
    select('time_cycle', 'months', 'wdays', 'user_profiles', 'n_sessions')

  for (m in seq_len(nrow(ev_model))) {
    time_cycle_name <- ev_model$time_cycle[[m]]
    if (!(time_cycle_name %in% ev_models$time_cycle)) {
      message(paste("Error: Time cycle", time_cycle_name, "does not exist"))
      return(NULL)
    }
    evmodel_idx <- which(time_cycle_name == ev_models$time_cycle)

    gmm <- left_join(
      ev_model$user_profiles[[m]],
      ev_models$user_profiles[[evmodel_idx]] %>%
        select('profile', 'connection_models', 'energy_models'),
      by = 'profile'
    )

    ev_model[["user_profiles"]][[m]] <- gmm
  }

  return(ev_model)
}



# Get parameters from model ----------------------------------------------------------

#' Get `evmodel` parameters in a list of summary tables
#'
#' Every time cycle is an element of the returned list, containing a table with
#' a user profile in every row and the mean and standard deviation values of the
#' GMM variables (connection duration, connection start time and energy).
#' If the energy models were built by charging rate, the average `mean` and `sd`
#' are provided without taking into account different charging rates (this
#' information is lost in this summary).
#'
#' @param evmodel object of class `evmodel`
#'
#' @return list
#' @export
#'
#' @importFrom purrr map
#' @importFrom stats median
#'
#' @examples
#' get_evmodel_summary(evsim::california_ev_model)
#'
get_evmodel_summary <- function(evmodel) {
  evmodel_parameters <- get_evmodel_parameters(evmodel)
  map(
    evmodel_parameters, get_time_cycle_summary
  )
}

get_time_cycle_summary <- function(time_cycle_parameters) {
  time_cycle_parameters %>%
    purrr::map(get_user_profile_summary) %>%
    purrr::list_rbind(names_to = "profile")
}

get_user_profile_summary <- function(user_profile_parameters) {
  connection_summary <- user_profile_parameters$connection_models %>%
    dplyr::mutate(
      start_mean = .data$start_mean*.data$ratio,
      start_sd = .data$start_sd*.data$ratio,
      duration_mean = .data$duration_mean*.data$ratio,
      duration_sd = .data$duration_sd*.data$ratio
    ) %>%
    dplyr::select(c("start_mean", "start_sd", "duration_mean", "duration_sd")) %>%
    dplyr::summarise_all(sum)
  energy_summary <- user_profile_parameters$energy_models %>%
    dplyr::mutate(
      energy_mean = .data$energy_mean*.data$ratio,
      energy_sd = .data$energy_sd*.data$ratio
    ) %>%
    dplyr::select("energy_mean", "energy_sd") %>%
    dplyr::summarise_all(sum)
  dplyr::tibble(
    ratio = user_profile_parameters$ratio
  ) %>%
    dplyr::mutate(
      connection_summary, energy_summary
    )
}


#' Get `evmodel` parameters in a list
#'
#' Every time cycle is an element of the returned list, containing a list with
#' the user profile as elements, each one containing the ratio and the
#' corresponding tables with the statistic parameters of connection and
#' energy GMM.
#'
#' @param evmodel object of class `evmodel`
#'
#' @return list
#' @export
#'
#' @importFrom purrr set_names map
#'
#' @examples
#' get_evmodel_parameters(evsim::california_ev_model)
#'
get_evmodel_parameters <- function(evmodel) {
  evmodel$models$user_profiles %>%
    set_names(evmodel$models$time_cycle) %>%
    map(
      get_time_cycle_parameters,
      connection_log = evmodel$metadata$connection_log,
      energy_log = evmodel$metadata$energy_log
    )
}

get_time_cycle_parameters <- function(time_cycle_model, connection_log, energy_log) {
  time_cycle_model <- time_cycle_model %>%
    dplyr::mutate(
      connection_parameters = purrr::map(
        .data$connection_models, get_connection_model_parameters, log = connection_log
      ),
      energy_parameters = purrr::map(
        .data$energy_models, get_energy_model_parameters, log = energy_log
      )
    )

  purrr::set_names(
    seq_len(nrow(time_cycle_model)),
    time_cycle_model$profile
  ) %>%
    purrr::map(
      ~ list(
        ratio = time_cycle_model$ratio[.x],
        connection_models = time_cycle_model$connection_parameters[[.x]],
        energy_models = time_cycle_model$energy_parameters[[.x]]
      )
    )
}

get_connection_model_parameters <- function(user_profile_models, log) {
  if (log) {
    func_conv <- exp
  } else {
    func_conv <- function(x){x}
  }
  purrr::pmap(
    user_profile_models,
    ~ dplyr::tibble(
      start_mean = func_conv(..1[1]),
      start_sd = func_conv(..2[1, 1]),
      duration_mean = func_conv(..1[2]),
      duration_sd = func_conv(..2[2, 2]),
      ratio = ..3
    )
  ) %>%
    purrr::list_rbind()
}

get_energy_model_parameters <- function(user_profile_models, log) {
  user_profile_models %>%
    dplyr::mutate(
      purrr::map(
        .data$energy_models,
        ~ get_power_energy_model_parameters(.x, log)
      ) %>%
        purrr::list_rbind()
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "charging_rate", "energy_mean", "energy_sd", "ratio"
    )))
}

get_power_energy_model_parameters <- function(user_profile_models_power, log) {
  if (log) {
    func_conv <- exp
  } else {
    func_conv <- function(x){x}
  }
  purrr::pmap(
    user_profile_models_power,
    ~ tibble(
      energy_mean = func_conv(..1)*..3,
      energy_sd = func_conv(..2)*..3,
    )
  )  %>%
    purrr::list_rbind() %>%
    dplyr::summarise_all(sum)
}









# Create model from parameters --------------------------------------------

#' Connection GMM
#'
#' Get connection Gaussian Mixture Models from parameters
#'
#' @param time_cycle_parameters tibble with Gaussian Mixture Models parameters.
#' This tibble must have the following columns: `profile`,	`ratio` (in %),	`start_mean` (in hours),
#' 	`start_sd` (in hours),	`duration_mean` (in hours),	`duration_sd` (in hours),	`energy_mean` (in kWh),	`energy_sd` (in kWh).
#' @param connection_log logical, true if connection models have logarithmic transformations
#'
#' @return connection GMM tibble
#' @keywords internal
#'
#' @importFrom dplyr tibble mutate select %>%
#' @importFrom purrr pmap
#'
get_connection_models_from_parameters <- function(time_cycle_parameters, connection_log = FALSE) {
  if (connection_log) {
    func_conv <- log
  } else {
    func_conv <- function(x){x}
  }

  time_cycle_parameters %>%
    mutate(
      ratio = .data$ratio,
      connection_models = pmap(
        list(.data$start_mean, .data$start_sd, .data$duration_mean, .data$duration_sd),
        ~ tibble(
          mu = list(c(
            func_conv(..1),
            func_conv(..3)
          )),
          sigma = list(cov(tibble(
            start = func_conv(rnorm(10000, ..1, ..2)),
            duration = func_conv(rnorm(10000, ..3, ..4))
          ))),
          ratio = 1
        )
      )
    ) %>%
    select("profile", "ratio", "connection_models")
}

#' Energy GMM
#'
#' Get energy Gaussian Mixture Models from parameters
#'
#' @param time_cycle_parameters tibble with Gaussian Mixture Models parameters.
#' This tibble must have the following columns: `profile`,	`ratio` (in %),	`start_mean` (in hours),
#' 	`start_sd` (in hours),	`duration_mean` (in hours),	`duration_sd` (in hours),	`energy_mean` (in kWh),	`energy_sd` (in kWh).
#' @param energy_log logical, true if connection models have logarithmic transformations
#'
#' @return energy GMM tibble
#' @keywords internal
#'
#' @importFrom dplyr tibble mutate select %>%
#' @importFrom purrr pmap
#'
get_energy_models_from_parameters <- function(time_cycle_parameters, energy_log =  FALSE) {
  time_cycle_parameters %>%
    mutate(
      energy_models = pmap(
        list(.data$energy_mean, .data$energy_sd),
        ~ tibble(
          charging_rate = "Unknown",
          ratio = 1,
          energy_models = list(tibble(
            mu = ifelse(energy_log, log(..1), ..1),
            sigma = ifelse(energy_log, sd(log(rnorm(10000, ..1, ..2))), ..2),
            ratio = 1
          ))
        )
      )
    ) %>%
    select("profile", "energy_models")
}


#' Create the custom EV model
#'
#' Get the EV model object of class `evmodel`
#'
#' @param names character vector with the given names of each time-cycle model
#' @param months_lst list of integer vectors with the corresponding months of the year for each time-cycle model
#' @param wdays_lst list of integer vectors with the corresponding days of the week for each time-cycle model (week start = 1)
#' @param parameters_lst list of tibbles corresponding to the GMM parameters of every time-cycle model
#' @param connection_log logical, true if connection models have logarithmic transformations
#' @param energy_log logical, true if energy models have logarithmic transformations
#' @param data_tz character, time zone of the original data (necessary to properly simulate new sessions)
#'
#' @returns object of class `evmodel`
#' @export
#'
#' @importFrom purrr map map2
#' @importFrom dplyr tibble left_join select mutate %>%
#'
#' @examples
#'
#' # For workdays time cycle
#' workdays_parameters <- dplyr::tibble(
#'   profile = c("Worktime", "Visit"),
#'   ratio = c(80, 20),
#'   start_mean = c(9, 11),
#'   start_sd = c(1, 4),
#'   duration_mean = c(8, 4),
#'   duration_sd = c(0.5, 2),
#'   energy_mean = c(15, 6),
#'   energy_sd = c(4, 3)
#' )
#'
#' # For weekends time cycle
#' weekends_parameters <- dplyr::tibble(
#'   profile = "Visit",
#'   ratio = 100,
#'   start_mean = 12,
#'   start_sd = 4,
#'   duration_mean = 3,
#'   duration_sd = 2,
#'   energy_mean = 4,
#'   energy_sd = 4
#' )
#'
#' parameters_lst <- list(workdays_parameters, weekends_parameters)
#'
#' # Get the whole model
#' ev_model <- get_custom_ev_model(
#'   names = c("Workdays", "Weekends"),
#'   months_lst = list(1:12, 1:12),
#'   wdays_lst = list(1:5, 6:7),
#'   parameters_lst = parameters_lst,
#'   connection_log = FALSE,
#'   energy_log = FALSE,
#'   data_tz = "Europe/Amsterdam"
#' )
#'
#'
get_custom_ev_model <- function(names, months_lst = list(1:12, 1:12), wdays_lst = list(1:5, 6:7),
                                parameters_lst, connection_log, energy_log, data_tz) {

  connection_GMM <- map(
    parameters_lst, get_connection_models_from_parameters, connection_log
  )
  energy_GMM <- map(
    parameters_lst, get_energy_models_from_parameters, energy_log
  )

  GMM <- map2(
    connection_GMM, energy_GMM,
    ~ left_join(.x, .y, by = 'profile')
  )

  ev_model <- list(
    metadata = list(
      creation = Sys.Date(),
      connection_log = connection_log,
      energy_log = energy_log,
      tzone = data_tz
    ),
    models = tibble(
      time_cycle = names,
      months = months_lst,
      wdays = wdays_lst,
      user_profiles = GMM
    )
  )
  class(ev_model) <- "evmodel"
  return( ev_model )
}


#' Save the EV model
#'
#' Save the EV model object of class `evmodel` to a JSON file
#'
#' @param evmodel object of class `evmodel`
#' @param file character string with the path or name of the file
#'
#' @returns nothing but saves the `evmodel` object in a JSON file
#' @export
#'
#' @examples
#' ev_model <- california_ev_model # Model of example
#'
#' save_ev_model(ev_model, file = file.path(tempdir(), "evmodel.json"))
#'
save_ev_model <- function(evmodel, file) {
  evmodel_lst <- list(
    metadata = evmodel$metadata,
    models = evmodel$models
  )
  ev_models_json <- jsonlite::toJSON(evmodel_lst)
  if (grepl(".json", file)) {
    write(ev_models_json, file = file)
  } else {
    write(ev_models_json, file = paste0(file, ".json"))
  }
}

