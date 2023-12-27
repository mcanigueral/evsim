library(testthat)
library(evsim)
library(dplyr)
library(lubridate)

# Get the example `evmodel` and `sessions` included in the package
sessions <- evsim::california_ev_sessions %>%
  filter(year(ConnectionStartDateTime) == 2018, month(ConnectionStartDateTime) == 10)
ev_model <- evsim::california_ev_model


test_that("simulation of EV sessions works", {
  simulated_sessions <- simulate_sessions(
    ev_model,
    sessions_day = tibble(
      time_cycle = c("Workday", "Weekend"),
      n_sessions = c(15, 10)
    ),
    user_profiles = tibble(
      time_cycle = c("Workday", "Workday", "Weekend"),
      profile = c("Visit", "Worktime", "Visit"),
      ratio = c(0.5, 0.5, 1)
    ),
    charging_powers = tibble(
      power = c(3.7, 11),
      ratio = c(0.3, 0.7)
    ),
    dates = seq.Date(today(), today()+days(4), length.out = 4),
    resolution = 15
  )
  expect_true(nrow(simulated_sessions) > 0)
})

test_that("simulation of EV sessions works with specific charging power", {
  simulated_sessions <- simulate_sessions(
    ev_model,
    sessions_day = tibble(
      time_cycle = c("Workday", "Weekend"),
      n_sessions = c(15, 10)
    ),
    user_profiles = tibble(
      time_cycle = c("Workday", "Workday", "Weekend"),
      profile = c("Visit", "Worktime", "Visit"),
      ratio = c(0.5, 0.5, 1),
      power = c(3.7, 11, 3.7)
    ),
    charging_powers = tibble(
      power = c(3.7, 11),
      ratio = c(0.3, 0.7)
    ),
    dates = seq.Date(today(), today()+days(4), length.out = 4),
    resolution = 15
  )
  expect_true(nrow(simulated_sessions) > 0)
})

test_that("charging rates are calculated from sessions",  {
  charging_rates <- sessions %>%
    get_charging_rates_distribution()
  expect_true(nrow(charging_rates) > 1)
})

test_that("simulation works with energy models of specific charging rates", {
  # Modify to assume that energy models from Worktime sessions are for 11 kW
  ev_model$models$user_profiles[[1]]$energy_models[[2]]$charging_rate <- 11
  simulated_sessions <- simulate_sessions(
    ev_model,
    sessions_day = tibble(
      time_cycle = c("Workday", "Weekend"),
      n_sessions = c(15, 10)
    ),
    user_profiles = tibble(
      time_cycle = c("Workday", "Workday", "Weekend"),
      profile = c("Visit", "Worktime", "Visit"),
      ratio = c(0.5, 0.5, 1),
      power = c(3.7, 11, 3.7)
    ),
    charging_powers = tibble(
      power = c(3.7, 11),
      ratio = c(0.3, 0.7)
    ),
    dates = seq.Date(today(), today()+days(4), length.out = 4),
    resolution = 15
  )
  expect_true(nrow(simulated_sessions) > 0)
})



