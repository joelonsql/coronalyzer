library(tidyverse)

first_date <- as.Date("2020-03-11")
last_date  <- as.Date("2020-04-11")
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

stable_date <- as.Date("2020-04-07") # first_date + max(which(!is.na(match(deaths_20200411 - deaths_20200410,0)))) - 1

data <- data.frame(deaths = integer(), death_date = as.Date(as.character()), report_date = as.Date(as.character())) %>%
  add_row(
    deaths = deaths_20200402,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-02"), length(deaths_20200402))
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
  ) %>%
  add_row(
    deaths = deaths_20200407 - deaths_20200406,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-07"), length(deaths_20200407))
  ) %>%
  add_row(
    deaths = deaths_20200408 - deaths_20200407,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-08"), length(deaths_20200408))
  ) %>%
  add_row(
    deaths = deaths_20200409 - deaths_20200408,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-09"), length(deaths_20200409))
  ) %>%
  add_row(
    deaths = deaths_20200410 - deaths_20200409,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-10"), length(deaths_20200410))
  ) %>%
  add_row(
    deaths = deaths_20200411 - deaths_20200410,
    death_date = seq(first_date, last_date, by = "day"),
    report_date = rep(as.Date("2020-04-11"), length(deaths_20200411))
  )

before_texts <- c(
  "Tegnell presentation 2020-04-06 i SVT: \"Fallen ligger på knappt 30 om dan\"\n\n",
  "Tegnell presentation 2020-04-07 i SVT: \"Vi ligger på ett snitt på 40 fall per dygn.\"\n\n",
  "Tegnell presentation 2020-04-08 i SVT: \"Nu ligger vi på 45 eller högre.\"\n\n"
)

after_texts <- c(
  "Tegnell presentation 2020-04-06 i SVT: \"Fallen ligger på knappt 30 om dan\"\n\nEftersläpningseffekten per 2020-04-10",
  "Tegnell presentation 2020-04-07 i SVT: \"Vi ligger på ett snitt på 40 fall per dygn.\"\n\nEftersläpningseffekten per 2020-04-10",
  "Tegnell presentation 2020-04-08 i SVT: \"Nu ligger vi på 45 eller högre.\"\n\nEftersläpningseffekten per 2020-04-10"
)

for (i in 1:3) {

last_report_date <- as.Date("2020-04-05") + i

before <- data %>% filter(death_date <= last_report_date & report_date <= last_report_date)
after <- data %>% filter(death_date <= last_report_date)

before$report_date <- as.factor(before$report_date)
after$report_date <- as.factor(after$report_date)

pal <- c("#ad64a0",rep("#55aaa0",9))

after_plot <- ggplot(after, aes(x=death_date)) +
  geom_col(aes(y=deaths, fill=report_date), position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  scale_fill_manual(values=pal) +
  labs(x = "Avlidendatum", y = "Antal nya dödsfall") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d-%b") +
  ggtitle("Antal avlidna per dag", subtitle = after_texts[i]) +
  guides(fill = FALSE) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 15)
  )

gpb <- ggplot_build(after_plot)

before_plot <- ggplot(before, aes(x=death_date)) +
  geom_col(aes(y=deaths, fill=report_date), position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  scale_fill_manual(values=pal) +
  labs(x = "Avlidendatum", y = "Antal nya dödsfall") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d-%b") +
  ggtitle("Antal avlidna per dag", subtitle = before_texts[i]) +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(
    gpb$layout$panel_scales_y[[1]]$range$range[1],
    gpb$layout$panel_scales_y[[1]]$range$range[2]
  )) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 15)
  )

print(before_plot)
print(after_plot)

ggsave(paste("fhm_",i,"_0.pdf",sep=""), before_plot)
ggsave(paste("fhm_",i,"_1.pdf",sep=""), after_plot)

}

data$report_date <- as.factor(data$report_date)
ggplot(data, aes(x=death_date)) +
  geom_col(aes(y=deaths, fill=report_date), position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  labs(x = "Datum avliden", fill = "Rapportdatum", y = "Antal avlidna") +
  ggtitle("Folkhälsomyndigheten - Covid19 Historik Excel - Avlidna per dag") +
  geom_vline(aes(xintercept = stable_date))

