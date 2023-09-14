
california_ev_model <- evprof::read_ev_model(
  'data-raw/california_evmodel.json'
)

usethis::use_data(california_ev_model, overwrite = TRUE)
