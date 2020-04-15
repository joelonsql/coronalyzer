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

path <- "/Users/joel/src/coronalyzer/Folkhalsomyndigheten_Covid19/"

data <- NULL

for (i in 0:9) {
  date <- as.Date("2020-04-02")+i
  xls <- read_excel(paste0(path,date,".xlsx"), sheet = 6) %>%
    filter(Åldersgrupp != "Uppgift saknas")
  xls$date <- date
  data <- rbind(data, xls)
}

ggplot(data, aes(x=date, y=Totalt_antal_fall, color=Åldersgrupp)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.95) +
  theme_minimal() +
  ggtitle("FHM Excel - Total antal per åldersgrupp")


ggplot(data, aes(x=date, y=Totalt_antal_intensivvårdade, color=Åldersgrupp)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.95) +
  theme_minimal() +
  ggtitle("FHM Excel - Total antal per åldersgrupp")


ggplot(data, aes(x=date, y=Totalt_antal_avlidna, color=Åldersgrupp)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.95) +
  theme_minimal() +
  ggtitle("FHM Excel - Total antal per åldersgrupp")


ggplot(data, aes(x=date, y=Totalt_antal_intensivvårdade / Totalt_antal_fall, color=Åldersgrupp)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.95) +
  theme_minimal() +
  ggtitle("FHM Excel - Total antal per åldersgrupp", subtitle = "Totalt_antal_intensivvårdade / Totalt_antal_fall")

ggplot(data, aes(x=date, y=Totalt_antal_avlidna / Totalt_antal_fall, color=Åldersgrupp)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.95) +
  theme_minimal() +
  geom_text(nudge_y=0.005, aes(y=Totalt_antal_avlidna / Totalt_antal_fall, label = scales::percent(Totalt_antal_avlidna / Totalt_antal_fall)), show.legend = FALSE, check_overlap = TRUE) +
  ggtitle("FHM Excel - Total antal per åldersgrupp", subtitle = "Totalt_antal_avlidna / Totalt_antal_fall")


ggplot(data, aes(x=date)) +
  geom_line(aes(y=Totalt_antal_fall, color="Totalt_antal_fall")) +
  geom_line(aes(y=Totalt_antal_intensivvårdade, color="Totalt_antal_intensivvårdade")) +
  geom_line(aes(y=Totalt_antal_avlidna, color="Totalt_antal_avlidna")) +
  theme_minimal() +
  scale_y_log10() +
  ggtitle("FHM Excel - Total antal per åldersgrupp") +
  facet_wrap(~Åldersgrupp)


ggplot(data, aes(x=date, y=Totalt_antal_intensivvårdade / Totalt_antal_fall)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.95) +
  theme_minimal() +
  ggtitle("FHM Excel - Total antal per åldersgrupp", subtitle = "Totalt_antal_intensivvårdade / Totalt_antal_fall") +
  facet_wrap(~Åldersgrupp)

ggplot(data %>%
         group_by(Åldersgrupp) %>%
         arrange(Åldersgrupp,date) %>%
         mutate(nyafall=c(0,diff(Totalt_antal_fall))),
       aes(x=date, y=nyafall, color=Åldersgrupp)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  theme_minimal() +
  labs(y = "Nya fall per dag") +
  ggtitle("FHM Excel - Total antal per åldersgrupp")






