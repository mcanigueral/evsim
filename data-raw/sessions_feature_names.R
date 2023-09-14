
sessions_feature_names <- c(
  "Session",
  "ConnectionStartDateTime",
  "ConnectionEndDateTime",
  "ChargingStartDateTime",
  "ChargingEndDateTime",
  "Power",
  "Energy",
  "ConnectionHours",
  "ChargingHours",
  "FlexibilityHours",
  "ChargingStation",
  "Socket"
)

usethis::use_data(sessions_feature_names, overwrite = TRUE)
