library(tidyverse)

data <- read_csv("training_data.csv")

model <- lm(share ~ day * daytype, data)
summary(model)

graph <- rbind(
  data.frame(day=0:7, daytype="Holiday"),
  data.frame(day=0:7, daytype="Weekday")
)
graph$share <- predict(model, graph)

ggplot(data, aes(x=day, y=share, color=daytype)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method="lm") +
  scale_x_continuous(breaks=0:7) +
  scale_y_continuous(breaks=seq(0.1,1.0,by=0.1), labels=scales::percent)

ggplot(graph, aes(x=day, y=share, color=daytype)) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(breaks=0:7) +
  scale_y_continuous(breaks=seq(0.1,1.0,by=0.1), labels=scales::percent)
