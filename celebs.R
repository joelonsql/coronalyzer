library(DBI)
library(tidyverse)
library(plotly)

db <- dbConnect(RPostgres::Postgres(), dbname = "ccms", host = "localhost", port = 5432, user = "joel")

query <- "
select
  date as x,
  count(*)::numeric as y
from ccms.celebs
group by 1
order by 1
"

data <- dbGetQuery(db, query)

ggplot(data, aes(x=x)) +
  geom_col(aes(y=y), position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  labs(x = "Date", y = "Notable people") +
  ggtitle("List of deaths due to coronavirus disease 2019", subtitle="https://en.wikipedia.org/wiki/List_of_deaths_due_to_coronavirus_disease_2019")

query <- "
select age::numeric as x from ccms.celebs
"

data <- dbGetQuery(db, query)

ggplot(data, aes(x=x)) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Age") +
  ggtitle("List of deaths due to coronavirus disease 2019", subtitle="https://en.wikipedia.org/wiki/List_of_deaths_due_to_coronavirus_disease_2019")

query <- "
select date as x, age::numeric as y from ccms.celebs
"

data <- dbGetQuery(db, query)

ggplot(data, aes(x=x,y=y)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Date", y = "Age") +
  ggtitle("List of deaths due to coronavirus disease 2019", subtitle="https://en.wikipedia.org/wiki/List_of_deaths_due_to_coronavirus_disease_2019")

dbDisconnect(db)



