library(readxl)
library(tidyverse)
library(data.table)

fix <- function(data, year) {
  data %>%
    filter(`DagMånad` != "Okänd dödsdag") %>%
    filter(`DagMånad` != "29 februari") %>%
    pivot_longer(matches("^[KM] "), names_to = "age", values_to = "deaths") %>%
    mutate(date = parse_date(paste(`DagMånad`,"2020"),"%d %B %Y",locale=locale("sv"))) %>%
#    filter(date < Sys.Date() - 7) %>%
    mutate(year = year)
}

url <- "https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/"

xls_file <- tempfile()
utils::download.file(url, xls_file, mode = "wb")

xls2019 <- read_excel(xls_file, skip = 7, sheet = "Tabell 2", range = cell_limits(c(8,1),c(NA,10))) %>%
  dplyr::select(-`2019`)

xls2020 <- cbind(
  xls2019$DagMånad,
  read_excel(xls_file, skip = 7, sheet = "Tabell 2", range = cell_limits(c(8,12),c(NA,19)))
) %>%
  rename(`DagMånad`=`xls2019$DagMånad`)

data <- rbind(
  fix(xls2019, 2019),
  fix(xls2020, 2020)
)

data$year <- as.factor(data$year)

# Use 2019 as base line
stats <- data %>%
  filter(year == 2019) %>%
  group_by(age, year) %>%
  summarise(sd = sd(deaths), mean = mean(deaths)) %>%
  dplyr::select(-year)

data <- data %>%
  group_by(age, year) %>%
  arrange(date) %>%
  mutate(
    roll_deaths = frollapply(deaths,60,mean),
    cummax      = cummax(deaths)
  ) %>%
  ungroup() %>%
  inner_join(stats, by = c("age"))

daily_max <- max(data$deaths)

ggplot(data, aes(x=date)) +
  geom_point(aes(y=deaths, color=year, alpha=0.5)) +
  geom_line(aes(y=roll_deaths, color=year), linetype = 2) +
  geom_line(aes(y=cummax, color=year)) +
  geom_ribbon(aes(ymin=mean-sd*3,ymax=mean+sd*3,fill="μ ± 3σ (99.73%)"),alpha=0.2) +
  geom_ribbon(aes(ymin=mean-sd*4,ymax=mean+sd*4,fill="μ ± 4σ (99.99%)"),alpha=0.2) +
  geom_ribbon(aes(ymin=mean-sd*5,ymax=mean+sd*5,fill="μ ± 5σ (99.9999%)"),alpha=0.2) +
  labs(x = "Date", y = "Antal dödsfall", fill = "Konfidensintervall (2019)") +
  facet_wrap(~age, nrow = 2) +
  guides(alpha = FALSE) +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0,daily_max,by=10))

#ggplot(data) +
#  geom_boxplot(aes(x=year, y=deaths, fill=month.abb[month(date)])) +
#  facet_wrap(~age) +
#  theme_minimal()

#ggplot(data, aes(date, deaths, color=year)) +
#  geom_point() +
#  geom_smooth(se = FALSE, method = lm) +
#  facet_wrap(~age) +
#  theme_minimal()

