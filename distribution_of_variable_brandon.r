library(dplyr)
library(tidyverse)

mental_health_data <- read.csv("mental_health_data.csv")

depressive_disorder <- mental_health_data %>%
  filter(Indicator == "Symptoms of Depressive Disorder" & Phase == "1" & Time.Period == "1" & Group == "By State") %>%
  select(State, Value) %>%
  arrange(Value)

depressive_disorder_indicators <- depressive_disorder$Value

hist(depressive_disorder_indicators,main="Indicators of Depressive Disorder in the US", xlab="Indicators of Depressive Disorders")
