library(readxl)
library(tidyverse)
library(data.table)
library(plotly)

url <- "https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/"

xls_file <- tempfile()
utils::download.file(url, xls_file, mode = "wb")

data <- read_excel(xls_file, skip = 7, sheet = "Tabell 1", range = cell_limits(c(7,1),c(NA,7))) %>%
  filter(`DagMånad` != "Okänd dödsdag") %>%
  filter(`DagMånad` != "29 februari") %>%
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "deaths") %>%
  mutate(date = parse_date(paste(`DagMånad`,"2020"),"%d %B %Y",locale=locale("sv"))) %>%
  dplyr::select(-`DagMånad`) %>%
  filter(date < Sys.Date()-14 | year < 2020) # SCB: "Statistik för två veckor tillbaka i tiden väntas inte förändras i någon större utsträckning"

plot <- ggplot(data, aes(x=date, y=deaths, color=year)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  ggtitle("Daily deaths registered in Sweden", subtitle="Source: https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/") +
  labs(y = "Daily deaths registered", x = "Day")

plot

ggplotly(plot)

data <- data %>%
  filter(date >= as.Date("2020-03-15") & date < Sys.Date()-14) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(cumsum_deaths = cumsum(deaths))

plot <- ggplot(data, aes(x=date, y=cumsum_deaths, color=year)) +
  geom_line() +
  theme_minimal() +
  ggtitle("Deaths registered in Sweden", subtitle="Source: https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/") +
  labs(y = "Cumulative sum of daily deaths registered", x = "Day")

plot

ggplotly(plot)
