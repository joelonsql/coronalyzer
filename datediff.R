library(tidyverse)

first_date <- as.Date("2020-03-11")
last_date  <- as.Date("2020-04-06")
deaths_20200402 <- c(0,0,1,1,2,2,1,6,7,9,8,11, 8,16,22,27,31,26,25,26,26,13, 5, 0,0,  0, 0)
deaths_20200403 <- c(1,0,1,1,2,2,1,6,7,9,8,11, 9,16,22,27,31,29,27,30,33,23,23, 2,0,  0, 0)
deaths_20200404 <- c(1,0,1,1,2,2,1,6,7,9,8,11, 9,16,23,27,31,29,28,30,36,25,36,18,1,  0, 0)
deaths_20200405 <- c(1,0,1,1,2,2,1,6,7,9,8,11, 9,16,24,27,32,29,29,30,36,31,43,22,6,  1, 0)
deaths_20200406 <- c(1,0,1,1,2,2,1,6,7,9,8,11,10,16,24,28,33,29,31,32,36,35,47,34,17,23,13)

data <- data.frame(deaths = integer(), death_date = as.Date(as.character()), report_date = as.Date(as.character())) %>%
  add_row(
    deaths = deaths_20200402,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date(NA), length(deaths_20200402))
  ) %>%
  add_row(
    deaths = deaths_20200403 - deaths_20200402,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-03"), length(deaths_20200403))
  ) %>%
  add_row(
    deaths = deaths_20200404 - deaths_20200403,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-04"), length(deaths_20200404))
  ) %>%
  add_row(
    deaths = deaths_20200405 - deaths_20200404,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-05"), length(deaths_20200405))
  ) %>%
  add_row(
    deaths = deaths_20200406 - deaths_20200405,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-06"), length(deaths_20200406))
  )

data$report_date <- as.factor(data$report_date)

ggplot(data, aes(x=death_date)) +
  geom_col(aes(y=deaths, fill=report_date)) +
  theme_minimal() +
  labs(x = "Datum avliden", fill = "Rapportdatum", y = "Antal avlidna") +
  ggtitle("Folkhälsomyndigheten - Covid19 Historik Excel - Avlidna per dag")

