
california_ev_sessions <- readRDS(
  "data-raw/california_sessions.RDS"
)
usethis::use_data(california_ev_sessions, overwrite = TRUE)
