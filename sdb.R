library(DBI)
library(tidyverse)
library(plotly)

db <- dbConnect(RPostgres::Postgres(), dbname = "joel", host = "localhost", port = 5432, user = "joel")

query <- "
select
  date_trunc('year',deathdate)::date as deathdate,
  age,
  gender,
  sum(count) as count
from sdbagg
where deathdate > '1890-01-01'
group by 1,2,3
order by 1,2,3
"

data <- dbGetQuery(db, query)

plot_ly(data, alpha = 0.5) %>%
  add_lines(
    x = ~age,
    y = ~count,
    frame = ~deathdate,
    color = ~factor(gender),
    line = list(simplify = FALSE)
  ) %>%
  layout(
    yaxis = list(title = "Antal döda"),
    title="Dödsfall i Sverige per ålder per år per kön"
  ) %>%
  animation_opts(50, redraw = FALSE) %>%
  animation_button(
    x = 0, xanchor = "left", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix="", font = list(size = 50, color="red"))
  )

dbDisconnect(db)
