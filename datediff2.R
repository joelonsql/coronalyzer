library(tidyverse)
library(plotly)

first_date <- as.Date("2020-03-11")
last_date  <- as.Date("2020-04-15")
n_days <- as.integer(last_date - first_date)
data <- rbind(
  data.frame(
    report_date = as.Date("2020-04-02"),
    death_date = seq(first_date,first_date+22,by=1),
    deaths = c(0,0,1,1,2,2,1,6,7, 9,8,11,8,16,22,27,31,26,25,26,26,13,5)
  ),
  data.frame(
    report_date = as.Date("2020-04-03"),
    death_date = seq(first_date,first_date+23,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,9,8,11,9,16,22,27,31,29,27,30,33,23,23,2)
  ),
  data.frame(
    report_date = as.Date("2020-04-04"),
    death_date = seq(first_date,first_date+24,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7, 9,8,11,9,16,23,27,31,29,28,30,36,25,36,18,1)
  ),
  data.frame(
    report_date = as.Date("2020-04-05"),
    death_date = seq(first_date,first_date+25,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,9,8,11,9,16,24,27,32,29,29,30,36,31,43,22,6,1)
  ),
  data.frame(
    report_date = as.Date("2020-04-06"),
    death_date = seq(first_date,first_date+26,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,9,8,11,10,16,24,28,33,29,31,32,36,35,47,34,17,23,13)
  ),
  data.frame(
    report_date = as.Date("2020-04-07"),
    death_date = seq(first_date,first_date+27,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,9,8,11,11,17,24,30,33,31,32,38,37,40,55,49,40,49,37,2)
  ),
  data.frame(
    report_date = as.Date("2020-04-08"),
    death_date = seq(first_date,first_date+28,by=1),
    deaths = c(1,0,1,1,2,2,2,6,7,10,7,11,11,18,25,29,33,31,34,38,36,42,59,54,48,58,55,36,6)
  ),
  data.frame(
    report_date = as.Date("2020-04-09"),
    death_date = seq(first_date,first_date+29,by=1),
    deaths = c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,45,65,58,54,67,66,53,47,3)
  ),
  data.frame(
    report_date = as.Date("2020-04-10"),
    death_date = seq(first_date,first_date+30,by=1),
    deaths = c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,47,67,64,57,75,74,60,67,20,3)
  ),
  data.frame(
    report_date = as.Date("2020-04-11"),
    death_date = seq(first_date,first_date+31,by=1),
    deaths = c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,47,67,65,57,75,74,60,70,23,13,0)
  ),
  data.frame(
    report_date = as.Date("2020-04-12"),
    death_date = seq(first_date,first_date+32,by=1),
    deaths = c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,47,67,65,57,75,74,60,70,24,14,8,2)
  ),
  data.frame(
    report_date = as.Date("2020-04-13"),
    death_date = seq(first_date,first_date+33,by=1),
    deaths = c(1,0,1,1,2,2,2,6,7,10,7,12,11,20,25,30,32,34,37,41,42,47,67,65,57,75,74,60,70,26,17,14,9,2)
  ),
  data.frame(
    report_date = as.Date("2020-04-14"),
    death_date = seq(first_date,first_date+34,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,30,32,35,38,42,43,48,69,68,59,76,71,65,77,43,31,26,33,21,5)
  ),
  data.frame(
    report_date = as.Date("2020-04-15"),
    death_date = seq(first_date,first_date+35,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,30,32,35,38,42,43,49,68,69,60,78,82,70,90,55,52,50,54,45,31,6)
  ),
  data.frame(
    report_date = as.Date("2020-04-16"),
    death_date = seq(first_date,first_date+36,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,30,32,35,38,42,43,50,68,71,61,79,85,75,97,63,62,61,62,55,49,41,10)
  ),
  data.frame(
    report_date = as.Date("2020-04-17"),
    death_date = seq(first_date,first_date+37,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,29,32,35,38,43,44,50,67,75,66,79,87,76,99,66,62,63,67,56,56,45,38,4)
  ),
  data.frame(
    report_date = as.Date("2020-04-18"),
    death_date = seq(first_date,first_date+38,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,29,32,35,38,44,45,50,67,78,68,81,88,77,101,73,73,73,76,62,60,55,59,20,2)
  ),
  data.frame(
    report_date = as.Date("2020-04-19"),
    death_date = seq(first_date,first_date+39,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,29,32,35,38,44,45,51,67,79,68,81,90,78,102,75,75,74,79,63,60,56,61,23,9,1)
  ),
  data.frame(
    report_date = as.Date("2020-04-20"),
    death_date = seq(first_date,first_date+40,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,29,32,35,38,44,45,51,67,79,68,81,90,78,102,76,75,74,79,63,60,57,63,30,19,17,2)
  ),
  data.frame(
    report_date = as.Date("2020-04-21"),
    death_date = seq(first_date,first_date+41,by=1),
    deaths = c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,29,32,35,39,44,45,52,67,81,69,82,90,81,106,79,78,84,86,72,67,77,78,49,51,43,21,3)
  )
)

data <- data %>%
  group_by(death_date) %>%
  mutate(new_deaths = deaths - coalesce(lag(deaths, order_by = report_date),0))

data$lag_effect <- as.numeric(data$report_date - data$death_date)

min_date <- min(data$report_date)
max_date <- max(data$report_date)

data <- data %>% mutate(lag_effect = if_else(report_date > min_date, lag_effect, 0))

ggplot(data %>% filter(new_deaths > 0 & report_date > min_date)) +
  geom_point(aes(x=death_date, y=report_date, size=new_deaths, color=lag_effect)) +
  theme_minimal() +
  labs(x = "Avliden_datum", color = "Eftersläpning", y = "Rapportdatum", size="Nya dödsfall") +
  ggtitle("Folkhälsomyndigheten - Covid19 Historik Excel - Avlidna per dag") +
  scale_color_gradientn(colours = terrain.colors(10)) +
  scale_y_date(breaks = "1 day")

data$report_date <- as.factor(data$report_date)

ggplot(data, aes(x=death_date)) +
  geom_line(aes(y=deaths, color=report_date)) +
  theme_minimal() +
  ggtitle("Folkhälsomyndigheten - Covid19 - Avlidna per dag") +
  labs(x = "Datum avliden", color = "Rapportdatum", y = "Antal avlidna")

data$lag_effect <- if_else(data$lag_effect < 7, data$lag_effect, 7)
data$lag_effect <-  as.factor(data$lag_effect)

plot <- ggplot(data, aes(x=death_date)) +
  geom_col(aes(y=new_deaths, fill=lag_effect), position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  labs(x = "Datum avliden", fill = "Eftersläpning", y = "Antal avlidna") +
  ggtitle("Folkhälsomyndigheten - Covid19 - Avlidna per dag") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-06")), aes(y=30, label="6/4: Fallen ligger på knappt 30 om dan."), hjust = "inward") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-07")), aes(y=40, label="7/4: Vi ligger på ett snitt på 40 fall per dygn."), hjust = "inward") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-08")), aes(y=45, label="8/4: Nu ligger vi på 45 eller högre."), hjust = "inward") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-20")), aes(y=60, label="20/4: Vi ligger i snitt på 60 fall om dagen."), hjust = "inward") +
  scale_y_continuous(breaks = seq(0,100,by=10))

plot

ggplotly(plot)





