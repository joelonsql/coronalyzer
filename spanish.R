library(DBI)
library(tidyverse)
library(plotly)

db <- dbConnect(RPostgres::Postgres(), dbname = "joel", host = "localhost", port = 5432, user = "joel")

query <- "
select
  deathdate,
--  avg(count) over (order by deathdate ROWS BETWEEN 30 PRECEDING AND CURRENT ROW) as count
  sum(count) over (order by deathdate) as count
from (
  select
    deathdate,
    sum(count-1.5384615384615385) as count
  from sdbagg
  where deathdate > '1918-07-01'
  and deathdate < '1919-03-01'
  and gender = 'M'
  and age between 21 and 28
  group by 1
  order by 1
) as x
"

data <- dbGetQuery(db, query)

ggplot(data, aes(x=deathdate, y=count)) +
  geom_line()
#  scale_y_log10()

dbDisconnect(db)
