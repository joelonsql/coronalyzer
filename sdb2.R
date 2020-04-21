library(DBI)
library(tidyverse)

db <- dbConnect(RPostgres::Postgres(), dbname = "joel", host = "localhost", port = 5432, user = "joel")

query <- "
select
  left(date_trunc('year',deathdate)::text,4) as year,
  format('1918-%s-%s',extract(month from deathdate), extract(day from deathdate))::date AS date,
--  deathdate::date AS date,
  sum(count) as deaths
from sdbagg
where deathdate >= '1914-01-01'
and deathdate < '1921-01-01'
and (extract(month from deathdate) <> 2 or extract(day from deathdate) <> 29)
group by 1,2
order by 1,2
"

data <- dbGetQuery(db, query)

data$year <- as.factor(data$year)

plot <- ggplot(data, aes(x=date, y=deaths, color=year)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  ggtitle("Daily deaths registered in Sweden before and during Spanish Flu", subtitle="Source: Sveriges dödsregister") +
  labs(y = "Daily deaths registered", x = "Day")

plot

dbDisconnect(db)
