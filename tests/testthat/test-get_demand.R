library(testthat)
library(evsim)
library(dplyr)
library(lubridate)

# Get the example `evmodel` and `sessions` included in the package
sessions <- evsim::california_ev_sessions %>%
  filter(year(ConnectionStartDateTime) == 2018, month(ConnectionStartDateTime) == 10)


test_that("demand is calcualted properly by Session", {
  demand <- sessions %>%
    get_demand(by = "Session", resolution = 15)
  expect_true(any(names(demand) %in% sessions$Session))
})

test_that("demand is calcualted properly by Profile", {
  demand <- sessions %>%
    mutate(Profile = "All") %>%
    get_demand(by = "Profile", resolution = 15)
  expect_true("All" %in% names(demand))
})

test_that("demand is calcualted properly with custom datetime sequence", {
  dttm_seq <- seq.POSIXt(
    as_datetime(dmy(01102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    as_datetime(dmy(07102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    by = "15 mins"
  )
  demand <- sessions %>%
    mutate(Profile = "All") %>%
    get_demand(by = "Profile", resolution = 15, dttm_seq = dttm_seq)
  expect_true(nrow(demand) == length(dttm_seq))
})
