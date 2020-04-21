library(readxl)
library(tidyverse)
library(data.table)
library(plotly)

url <- "https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/"

xls_file <- tempfile()
utils::download.file(url, xls_file, mode = "wb")

data <- read_excel(xls_file, skip = 9, sheet = "Tabell 3", range = cell_limits(c(9,1),c(NA,3))) %>%
  filter(`DagMånad` != "Okänd dödsdag") %>%
  filter(`DagMånad` != "Samtliga döda") %>%
  filter(`DagMånad` != "29 februari") %>%
  mutate(date = parse_date(paste(`DagMånad`,"2020"),"%d %B %Y",locale=locale("sv"))) %>%
  mutate(year = `Ar`) %>%
  mutate(deaths = `1`) %>%
  dplyr::select(-`DagMånad`) %>%
  dplyr::select(-`Ar`) %>%
  dplyr::select(-`1`) %>%
  filter(date < Sys.Date()-14 | year < 2020) # SCB: "Statistik för två veckor tillbaka i tiden väntas inte förändras i någon större utsträckning"

data$year <- as.factor(data$year)

plot <- ggplot(data, aes(x=date, y=deaths, color=year)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  ggtitle("Daily deaths registered in Stockholm/Sweden", subtitle="Source: https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/") +
  labs(y = "Daily deaths registered", x = "Day")

plot

# ggplotly(plot)
