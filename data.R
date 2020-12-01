data <- readRDS("shelters.rds")

data$occupancy_date <- as.Date(data$occupancy_date)

