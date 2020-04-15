library(tidyverse)

first_date <- as.Date("2020-03-11")
last_date  <- as.Date("2020-04-15")
deaths_20200402 <- c(0,0,1,1,2,2,1,6,7, 9,8,11, 8,16,22,27,31,26,25,26,26,13, 5, 0,0,  0, 0, 0, 0, 0, 0,0)
deaths_20200403 <- c(1,0,1,1,2,2,1,6,7, 9,8,11, 9,16,22,27,31,29,27,30,33,23,23, 2,0,  0, 0, 0, 0, 0, 0,0)
deaths_20200404 <- c(1,0,1,1,2,2,1,6,7, 9,8,11, 9,16,23,27,31,29,28,30,36,25,36,18,1,  0, 0, 0, 0, 0, 0,0)
deaths_20200405 <- c(1,0,1,1,2,2,1,6,7, 9,8,11, 9,16,24,27,32,29,29,30,36,31,43,22,6,  1, 0, 0, 0, 0, 0,0)
deaths_20200406 <- c(1,0,1,1,2,2,1,6,7, 9,8,11,10,16,24,28,33,29,31,32,36,35,47,34,17,23,13, 0, 0, 0, 0,0)
deaths_20200407 <- c(1,0,1,1,2,2,1,6,7, 9,8,11,11,17,24,30,33,31,32,38,37,40,55,49,40,49,37, 2, 0, 0, 0,0)
deaths_20200408 <- c(1,0,1,1,2,2,2,6,7,10,7,11,11,18,25,29,33,31,34,38,36,42,59,54,48,58,55,36, 6, 0, 0,0)
deaths_20200409 <- c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,45,65,58,54,67,66,53,47, 3, 0,0)
deaths_20200410 <- c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,47,67,64,57,75,74,60,67,20, 3,0)
deaths_20200411 <- c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,47,67,65,57,75,74,60,70,23,13,0)
deaths_20200413 <- c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,47,67,65,57,75,74,60,70,26,17,14, 9, 2,13,0)
deaths_20200414 <- c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,30,32,35,38,42,43,48,69,68,59,76,71,65,77,43,31,26,33,21, 5,0)
deaths_20200415 <- c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,30,32,35,38,42,43,49,68,69,60,78,82,70,90,55,52,50,54,45,31,6)

stable_date <- as.Date("2020-04-02") # first_date + max(which(!is.na(match(deaths_20200415 - deaths_20200414,0)))) - 1

data <- data.frame(deaths = integer(), death_date = as.Date(as.character()), report_date = as.Date(as.character())) %>%
  add_row(
    deaths = deaths_20200413,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-13"), length(deaths_20200413))
  ) %>%
  add_row(
    deaths = deaths_20200414 - deaths_20200413,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-14"), length(deaths_20200414))
  ) %>%
  add_row(
    deaths = deaths_20200415 - deaths_20200414,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-15"), length(deaths_20200415))
  )

data$report_date <- as.factor(data$report_date)
ggplot(data, aes(x=death_date)) +
  geom_col(aes(y=deaths, fill=report_date), position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  labs(x = "Datum avliden", fill = "Rapportdatum", y = "Antal avlidna") +
  ggtitle("Folkhälsomyndigheten - Covid19 Historik Excel - Avlidna per dag") +
  geom_vline(aes(xintercept = stable_date)) +
  scale_y_continuous(breaks = seq(0,100,by=10))
