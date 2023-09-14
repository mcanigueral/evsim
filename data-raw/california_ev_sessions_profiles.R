
california_ev_sessions_profiles <- readRDS(
  "data-raw/california_sessions_profiles.RDS"
)
usethis::use_data(california_ev_sessions_profiles, overwrite = TRUE)
