
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


# Modify the models -------------------------------------------------------

#' Update the ratios of the user profiles
#'
#' @param evmodel object of class `evmodel`
#' @param new_ratios tibble with columns: `time_cycle`, `profile`, `ratio`
#' It must have all profiles from every model, including the ones with `ratio = 0`.
#' The ratios must be between 0 and 1.
#' @param discard If TRUE, profiles with `ratio == 0` will be discarded from the `evmodel` object
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dbl
#'
update_profiles_ratios <- function(evmodel, new_ratios, discard=FALSE) {
  ev_model <- evmodel[['models']]
  for (m in 1:nrow(ev_model)) {

    time_cycle <- ev_model[["time_cycle"]][[m]]
    if (!(time_cycle %in% new_ratios[["time_cycle"]])) next

    gmm <- ev_model[["user_profiles"]][[m]]
    gmm[["ratio"]] <- map_dbl(
      gmm[["profile"]],
      ~ new_ratios[["ratio"]][ (new_ratios[["time_cycle"]] == time_cycle) & (new_ratios[["profile"]] == .x) ]
    )

    if (discard) {
      gmm <- gmm[gmm[["ratio"]] > 0, ]
    }

    ev_model[["user_profiles"]][[m]] <- gmm
  }

  evmodel[['models']] <- ev_model
  return(evmodel)
}


