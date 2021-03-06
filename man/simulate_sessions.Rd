% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{simulate_sessions}
\alias{simulate_sessions}
\title{Simulate sessions given the \code{evmodel} object and a datetime sequence}
\usage{
simulate_sessions(evmodel, sessions_day, charging_powers, dates, resolution)
}
\arguments{
\item{evmodel}{object of type \code{evmodel} (see this \href{https://mcanigueral.github.io/evprof/articles/evmodel.html}{link} for more information)}

\item{sessions_day}{tibble with variables \code{time_cycle} (names corresponding to \code{evmodel$models$time_cycle}) and \code{n_sessions} (number of daily sessions per day for each time-cycle model)}

\item{charging_powers}{tibble with variables \code{power} and \code{ratio}
The powers must be in kW and the ratios between 0 and 1.}

\item{dates}{date sequence that will set the time frame of the simulated sessions}

\item{resolution}{integer, time resolution (in minutes) of the sessions datetime variables}
}
\value{
tibble
}
\description{
Simulate sessions given the \code{evmodel} object and a datetime sequence
}
