library(tidyverse)

age_groups <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-90","90+")

deaths_20200402 <- c(0,0,2,0,1,12,18, 76,119,54)
deaths_20200403 <- c(0,0,2,0,1,13,23, 89,144,61)
deaths_20200404 <- c(0,0,3,0,1,15,26,101,157,70)
deaths_20200405 <- c(0,0,3,1,1,17,27,106,170,76)
deaths_20200406 <- c(0,0,3,1,1,22,32,126,196,96)

data <- data.frame(deaths = integer(), report_date = as.Date(as.character()), age_group = as.character()) %>%
  add_row(
    deaths = deaths_20200402,
    age_group = age_groups,
    report_date = NA
  ) %>%
  add_row(
    deaths = deaths_20200403 - deaths_20200402,
    age_group = age_groups,
    report_date = rep(as.Date("2020-04-03"), length(deaths_20200403))
  ) %>%
  add_row(
    deaths = deaths_20200404 - deaths_20200403,
    age_group = age_groups,
    report_date = rep(as.Date("2020-04-04"), length(deaths_20200404))
  ) %>%
  add_row(
    deaths = deaths_20200405 - deaths_20200404,
    age_group = age_groups,
    report_date = rep(as.Date("2020-04-05"), length(deaths_20200405))
  ) %>%
  add_row(
    deaths = deaths_20200406 - deaths_20200405,
    age_group = age_groups,
    report_date = rep(as.Date("2020-04-06"), length(deaths_20200406))
  )

# data$report_date <- as.factor(data$report_date)

ggplot(data, aes(x=report_date)) +
  geom_col(aes(y=deaths, fill=age_group)) +
  theme_minimal() +
  labs(x = "Rapportdatum", fill = "Åldersgrupp", y = "Antal avlidna") +
  ggtitle("Folkhälsomyndigheten - Covid19 Historik Excel - Totalt antal per åldersgrupp")

