
#' `print` method for `evmodel` object class
#'
#' @param x  `evmodel` object
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#'
print.evmodel <- function(x, ...) {
  m <- x$models
  cat('EV sessions model of class "evprof", created on', as.character(x$metadata$creation), '\n')
  cat('The Gaussian Mixture Models of EV user profiles are built in:\n')
  cat('  - Connection Models:', if (x$metadata$connection_log) "logarithmic" else "natural", 'scale\n')
  cat('  - Energy Models:', if (x$metadata$energy_log) "logarithmic" else "natural", 'scale\n')
  cat('\nModel composed by', nrow(m), 'time-cycle models:\n')
  for (n in 1:nrow(m)) {
    cat(
      '  ', n, '. ', m[['time_cycle']][n], ':',
      '\n     Months = ', if (length(m[['months']][[n]]) == 1) m[['months']][[n]][1] else
        paste0(m[['months']][[n]][1], '-', m[['months']][[n]][length(m[['months']])]),
      ', Week days = ', if (length(m[['wdays']][[n]]) == 1) m[['wdays']][[n]][1] else
        paste0(m[['wdays']][[n]][1], '-', m[['wdays']][[n]][length(m[['wdays']])]),
      '\n     User profiles = ', paste(m[['user_profiles']][[n]][['profile']], collapse = ", "),
      '\n', sep = ''
    )
  }
}


# Modify the models -------------------------------------------------------

#' Update the ratios of the user profiles
#'
#' @param evmodel object of class `evmodel`
#' @param new_ratios tibble with columns: `model_name`, `profile`, `profile_ratio.`
#' It must have all profiles from every model, including the ones with `profile_ratio = 0`.
#' The ratios must be between 0 and 1.
#' @param discard If TRUE, profiles with `profile_ratio == 0` will be discarded from the `evmodel` object
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dbl
#'
update_profiles_ratios <- function(evmodel, new_ratios, discard=FALSE) {

  for (m in 1:nrow(evmodel)) {
    model <- evmodel[["models"]][[m]]
    model_name <- evmodel[["time_cycle"]][[m]]
    model[["ratio"]] <- map_dbl(
      model[["profile"]],
      ~ new_ratios[["ratio"]][ (new_ratios[["time_cycle"]] == model_name) & (new_ratios[["profile"]] == .x) ]
    )

    if (discard) {
      model <- model[model[["ratio"]] > 0, ]
    }

    evmodel[["models"]][[m]] <- model
  }

  return(evmodel)
}


