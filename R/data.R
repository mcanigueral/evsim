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
