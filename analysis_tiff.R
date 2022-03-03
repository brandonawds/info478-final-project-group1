library(dplyr)
library(tidyverse)

mental_health_data <- read.csv("mental_health_data.csv")

anxiety_disorder <- mental_health_data %>%
  filter(Indicator == "Symptoms of Anxiety Disorder" & Phase == "1"  & Time.Period =="1" & Group == "By State") %>%
  select(State, Value) %>%
  arrange(Value)

anxiety_disorder_histogam <- hist(anxiety_disorder$Value, main="Indicators of Anxiety Disorder in US", xlab = "Indicators of Anxiety Disorder")

