library(testthat)
library(evsim)
library(dplyr)
library(lubridate)

# Get the example `evmodel` and `sessions` included in the package
ev_model <- evsim::california_ev_model
temp_dir <- tempdir()

test_that("Model is created correctly from parameters", {
  # For workdays time cycle
  workdays_parameters <- dplyr::tibble(
    profile = c("Worktime", "Visit"),
    ratio = c(80, 20),
    start_mean = c(9, 11),
    start_sd = c(1, 4),
    duration_mean = c(8, 4),
    duration_sd = c(0.5, 2),
    energy_mean = c(15, 6),
    energy_sd = c(4, 3)
  )

  # For weekends time cycle
  weekends_parameters <- dplyr::tibble(
    profile = "Visit",
    ratio = 100,
    start_mean = 12,
    start_sd = 4,
    duration_mean = 3,
    duration_sd = 2,
    energy_mean = 4,
    energy_sd = 4
  )


  # Get the whole model
  ev_model <- get_ev_model(
    names = c("Workdays", "Weekends"),
    months_lst = list(1:12, 1:12),
    wdays_lst = list(1:5, 6:7),
    connection_GMM = purrr::map(
      list(Workdays = workdays_parameters, Weekends = weekends_parameters),
      ~ get_connection_models_from_parameters(.x)
    ),
    energy_GMM = purrr::map(
      list(Workdays = workdays_parameters, Weekends = weekends_parameters),
      ~ get_energy_models_from_parameters(.x)
    ),
    connection_log = FALSE,
    energy_log = FALSE,
    data_tz = "Europe/Amsterdam"
  )

  expect_equal(class(ev_model), "evmodel")
})

test_that("Model is printed correctly",  {
  print(ev_model)
})

test_that("Model file is saved correctly",  {
  temp_model_file <- file.path(temp_dir, "model.json")
  save_ev_model(ev_model, file = temp_model_file)
  expect_true(file.exists(temp_model_file))
})

test_that("Model file is read correctly",  {
  temp_model_file <- file.path(temp_dir, "model.json")
  ev_model <- read_ev_model(file = temp_model_file)
  expect_equal(class(ev_model), "evmodel")
})

test_that("User profiles distribution is printed correctly", {
  upd <- get_user_profiles_distribution(ev_model)
  expect_equal(c("time_cycle", "profile", "ratio"), colnames(upd))
})
