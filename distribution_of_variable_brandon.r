library(dplyr)
library(tidyverse)

mental_health_data <- read.csv("mental_health_data.csv")

depressive_disorder <- mental_health_data %>%
  filter(Indicator == "Symptoms of Depressive Disorder" & Phase == "1" & Time.Period == "1" & Group == "By State") %>%
  select(State, Value) %>%
  arrange(Value)

bins <- seq(0, 40, by=5)
depressive_disorder_indicators <- depressive_disorder$Value
depressive_bins <- cut(depressive_disorder_indicators, bins)

indicators_histogram <- hist(depressive_disorder_indicators, breaks=bins, main="Indicators of Depressive Disorder in the US", xlab="Indicators of Depressive Disorders")
