library(testthat)
library(evsim)
library(dplyr)
library(lubridate)


# Get the example `evmodel` and `sessions` included in the package
resolution <- 15
sessions <- evsim::california_ev_sessions_profiles %>%
  filter(year(ConnectionStartDateTime) == 2018, month(ConnectionStartDateTime) == 10) %>%
  adapt_charging_features(time_resolution = resolution)


test_that("demand is calculated properly by Session", {
  demand <- sessions %>%
    get_demand(by = "Session", resolution = resolution)
  expect_true(all(sessions$Session %in% names(demand)))
  expect_equal(round(sum(rowSums(demand[-1]))*resolution/60), round(sum(sessions$Energy)))
})

test_that("demand is calculated properly by Profile", {
  demand <- sessions %>%
    get_demand(by = "Profile", resolution = resolution)
  expect_true(all(unique(sessions$Profile) %in% names(demand)))
  expect_equal(round(sum(rowSums(demand[-1]))*resolution/60), round(sum(sessions$Energy)))
})

test_that("demand is calculated properly with custom datetime sequence", {
  dttm_seq <- seq.POSIXt(
    as_datetime(dmy(08102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    as_datetime(dmy(15102018)) %>% force_tz(tz(sessions$ConnectionStartDateTime)),
    by = "15 mins"
  )
  demand <- sessions %>%
    get_demand(by = "Profile", resolution = resolution, dttm_seq = dttm_seq)
  expect_true(nrow(demand) == length(dttm_seq))
})

test_that("demand calculation is skipped if there are no sessions", {
  expect_error(
    sessions %>%
      mutate(Profile = "All") %>%
      filter(Profile == "all") %>%
      get_demand(by = "Profile", resolution = resolution, dttm_seq = NULL)
  )
})


test_that("zeros are returned when no sessions have demand in datetime_seq", {
  demand <- sessions %>%
    mutate(Profile = "All") %>%
    get_demand(by = "Profile", resolution = resolution, dttm_seq = NULL)
  dttm_seq_2 <- demand$datetime + years(1)
  demand2 <- sessions %>%
    mutate(Profile = "All") %>%
    get_demand(by = "Profile", resolution = resolution, dttm_seq = dttm_seq_2)
  expect_equal(sum(demand2$All), 0)
})
