library(testthat)
library(evsim)
library(dplyr)
library(lubridate)


# Get the example `evmodel` and `sessions` included in the package
resolution <- 15
sessions <- evsim::california_ev_sessions_profiles %>%
  filter(year(ConnectionStartDateTime) == 2018, month(ConnectionStartDateTime) == 10) %>%
  adapt_charging_features(time_resolution = resolution)


test_that("the number of connections is calculated properly by Session", {
  n_connections <- sessions %>%
    get_occupancy(by = "Session", resolution = resolution)
  expect_true(all(sessions$Session %in% names(n_connections)))
})

test_that("the number of connections is calculated properly by Profile", {
  n_connections <- sessions %>%
    get_occupancy(by = "Profile", resolution = resolution)
  expect_true(all(unique(sessions$Profile) %in% names(n_connections)))
})

test_that("the number of connections is calculated properly with custom datetime sequence", {
  dttm_seq <- seq.POSIXt(
    as_datetime(dmy(01102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    as_datetime(dmy(07102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    by = "15 mins"
  )
  n_connections <- sessions %>%
    get_occupancy(by = "Profile", resolution = resolution, dttm_seq = dttm_seq)
  expect_true(nrow(n_connections) == length(dttm_seq))
})

test_that("the number of connections calculation is skipped if there are no sessions nor datetime sequence", {
  expect_error(
    sessions %>%
      mutate(Profile = "All") %>%
      filter(Profile == "all") %>%
      get_occupancy(by = "Profile", resolution = resolution, dttm_seq = NULL)
  )
})

test_that("zeros are returned when no sessions are connected in datetime_seq", {
  n_connections <- sessions %>%
    mutate(Profile = "All") %>%
    get_occupancy(by = "Profile", resolution = resolution, dttm_seq = NULL)
  dttm_seq_2 <- n_connections$datetime + years(1)
  n_connections2 <- sessions %>%
    mutate(Profile = "All") %>%
    get_occupancy(by = "Profile", resolution = resolution, dttm_seq = dttm_seq_2)
  expect_equal(sum(n_connections2$All), 0)
})
