% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{get_estimated_energy}
\alias{get_estimated_energy}
\title{Estimate energy given energy models tibble}
\usage{
get_estimated_energy(n, energy_models, log)
}
\arguments{
\item{n}{number of sessions}

\item{energy_models}{energy models tibble}

\item{log}{Logical, true if models have logarithmic transformation}
}
\value{
list of numeric vectors
}
\description{
Estimate energy given energy models tibble
}
