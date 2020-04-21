library(DBI)
library(readxl)
library(tidyverse)
library(data.table)
library(scales)

url <- "https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/"

xls_file <- tempfile()
utils::download.file(url, xls_file, mode = "wb")

data1 <- read_excel(xls_file, skip = 7, sheet = "Tabell 1", range = cell_limits(c(7,1),c(NA,7))) %>%
  filter(`DagMånad` != "Okänd dödsdag") %>%
  filter(`DagMånad` != "29 februari") %>%
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "deaths") %>%
  mutate(date = parse_date(paste(`DagMånad`,"2020"),"%d %B %Y",locale=locale("sv"))) %>%
  dplyr::select(-`DagMånad`) %>%
  filter(date < Sys.Date()-14 | year < 2020) %>%
  filter(year >= 2019)

db <- dbConnect(RPostgres::Postgres(), dbname = "joel", host = "localhost", port = 5432, user = "joel")

query <- "
select
  left(date_trunc('year',deathdate)::text,4) as year,
  format('2020-%s-%s',extract(month from deathdate), extract(day from deathdate))::date AS date,
--  deathdate::date AS date,
  sum(count) as deaths
from sdbagg
where deathdate >= '1917-01-01'
and deathdate < '1919-01-01'
and (extract(month from deathdate) <> 2 or extract(day from deathdate) <> 29)
group by 1,2
order by 1,2
"

data2 <- dbGetQuery(db, query)

data2$deaths <- data2$deaths * 1.1

data <- rbind(data1, data2)

data$year <- as.factor(data$year)

plot <- ggplot(data, aes(x=date, y=deaths, color=year)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  ggtitle("Daily deaths registered in Sweden", subtitle="Source: Sveriges dödsregister") +
  labs(y = "Daily deaths registered", x = "Day")
#  scale_y_continuous(expand = c(0, 0), limits = c(0, 13e-05), labels = comma)

plot

dbDisconnect(db)
