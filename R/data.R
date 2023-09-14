

#' Names of standard features of a sessions dataset
#'
#' A vector with the standard names of sessions features
#' for this package functions.
#'
#' @format A vector
#' @keywords internal
#'
"sessions_feature_names"



#' EV charging sessions example
#'
#' Example of an charging sessions data set ready to use by `{evprof}` functions.
#' It is the open source data set downloaded from the
#' [ACN-Data website](https://ev.caltech.edu/dataset), transformed according
#' to the standard names defined by `evprof`
#' (see [this](https://mcanigueral.github.io/evprof/articles/sessions-format.html) article).
#' More information about the analysis of this data set in the {evprof} website:
#' <https://mcanigueral.github.io/evprof/articles/california.html>
#'
#' @format ## `california_ev_sessions`
#' A `tibble` object with standard variable names defined by `evprof`
#' @source <https://ev.caltech.edu/dataset>
#' @keywords internal
#'
"california_ev_sessions"


#' Clustered EV charging sessions example
#'
#' Example of an charging sessions data set that has been clustered by `{evprof}` functions.
#' (see [this article](https://mcanigueral.github.io/evprof/articles/california.html#sessions-classification-into-user-profiles)).
#'
#' @format ## `california_ev_sessions_profiles`
#' A `tibble` object with standard variable names defined by `evprof`
#' @source <https://ev.caltech.edu/dataset>
#' @keywords internal
#'
"california_ev_sessions_profiles"


#' EV model example
#'
#' Example of an `evmodel` object created with `{evprof}` for testing purposes.
#' It has been created using an Open source data set of EV charging sessions
#' provided by [ACN](https://acnportal.readthedocs.io/en/latest/).
#' More information about the development of the model in the {evprof} website:
#' <https://mcanigueral.github.io/evprof/articles/california.html>
#'
#' @format ## `california_ev_model`
#' An `evmodel` object.
#' \describe{
#'   \item{metadata}{Information about the characteristics of the model}
#'   \item{model}{Gaussian Mixture Models for connection times and energy}
#' }
#' @source <https://mcanigueral.github.io/evprof/articles/california.html>
#' @keywords internal
#'
"california_ev_model"



