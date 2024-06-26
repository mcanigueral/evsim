% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evmodel.R
\name{get_connection_models_from_parameters}
\alias{get_connection_models_from_parameters}
\title{Connection GMM}
\usage{
get_connection_models_from_parameters(
  time_cycle_parameters,
  connection_log = FALSE
)
}
\arguments{
\item{time_cycle_parameters}{tibble with Gaussian Mixture Models parameters.
This tibble must have the following columns: \code{profile},    \code{ratio} (in \%),    \code{start_mean} (in hours),
\code{start_sd} (in hours),    \code{duration_mean} (in hours),    \code{duration_sd} (in hours),    \code{energy_mean} (in kWh),    \code{energy_sd} (in kWh).}

\item{connection_log}{logical, true if connection models have logarithmic transformations}
}
\value{
connection GMM tibble
}
\description{
Get connection Gaussian Mixture Models from parameters
}
\keyword{internal}
