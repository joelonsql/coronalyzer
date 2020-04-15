library(readxl)
library(tidyverse)
library(data.table)

data <- read_csv("deaths.csv")

ggplot(data, aes(x=week, y=deaths)) +
  geom_line(aes(color="2020"), size=1) +
  geom_ribbon(aes(ymin=min, ymax=max, fill="2015-19 Range"), alpha=0.5) +
  theme_minimal() +
  scale_fill_manual(values=c("orange")) +
  scale_color_manual(values=c("blue")) +
  ggtitle("Weekly deaths registered in Sweden", subtitle="Source: SCB, week ending 5th April 2020") +
  labs(y = "Weekly deaths registered", x = "Day")
  


