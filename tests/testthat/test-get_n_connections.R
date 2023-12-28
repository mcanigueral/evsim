library(testthat)
library(evsim)
library(dplyr)
library(lubridate)

# Get the example `evmodel` and `sessions` included in the package
sessions <- evsim::california_ev_sessions %>%
  filter(year(ConnectionStartDateTime) == 2018, month(ConnectionStartDateTime) == 10)


test_that("the number of connections is calculated properly by Session", {
  n_connections <- sessions %>%
    get_n_connections(by = "Session", resolution = 15)
  expect_true(any(names(n_connections) %in% sessions$Session))
})

test_that("the number of connections is calculated properly by Profile", {
  n_connections <- sessions %>%
    mutate(Profile = "All") %>%
    get_n_connections(by = "Profile", resolution = 15)
  expect_true("All" %in% names(n_connections))
})

test_that("the number of connections is calculated properly with custom datetime sequence", {
  dttm_seq <- seq.POSIXt(
    as_datetime(dmy(01102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    as_datetime(dmy(07102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    by = "15 mins"
  )
  n_connections <- sessions %>%
    mutate(Profile = "All") %>%
    get_n_connections(by = "Profile", resolution = 15, dttm_seq = dttm_seq)
  expect_true(nrow(n_connections) == length(dttm_seq))
})

test_that("the number of connections calculation is skipped if there are no sessions nor datetime sequence", {
  n_connections <- sessions %>%
    mutate(Profile = "All") %>%
    filter(Profile == "all") %>%
    get_n_connections(by = "Profile", resolution = 15, dttm_seq = NULL)
  expect_true(is.null(n_connections))
})
