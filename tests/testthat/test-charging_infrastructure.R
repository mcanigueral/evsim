library(testthat)
library(evsim)
library(dplyr)
library(lubridate)

# Get the example `evmodel` and `sessions` included in the package
sessions <- evsim::california_ev_sessions %>%
  filter(year(ConnectionStartDateTime) == 2018, month(ConnectionStartDateTime) == 10)

test_that("plot of occupancy duration curve works", {
  expect_true(ggplot2::is.ggplot(
    plot_occupancy_duration_curve(
      sessions %>% mutate(Profile = "all"),
      by = "Profile",
      resolution = 15
    )
  ))
})

test_that("charging infrastructure sizing works", {
  sessions_infrastructure <- add_charging_infrastructure(sessions, duration_th = 0)
  expect_true(length(unique(sessions_infrastructure$ChargingStation)) > 1)
})
