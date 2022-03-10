library(dplyr)
library(tidyverse)
library(ggplot2)

mental_health_data <- read.csv("mental_health_data.csv")

# Final Deliverable 


disability_depression <- mental_health_data %>%
  filter(Indicator == "Symptoms of Depressive Disorder" & Group == "By Disability status") %>%
  select(Subgroup, Value, Time.Period.Label) %>%
  filter(Value != "NA") %>%
  filter(Subgroup =="With disability")

disability_depression_plot <- ggplot(data=disability_depression, aes(x=Time.Period.Label, y=Value, group=1)) + geom_line(color="Blue") + geom_point() + xlab("Time Period") + ylab("Percentage of Population w/ Indicators of Depression") + 
  ggtitle("Indicators of Depression for Disabled People") + theme(axis.text = element_text(size = 8))

without_disability_depression <- mental_health_data %>%
  filter(Indicator == "Symptoms of Depressive Disorder" & Group == "By Disability status") %>%
  select(Subgroup, Value, Time.Period.Label) %>%
  filter(Value != "NA") %>%
  filter(Subgroup =="Without disability")

without_disability_depression_plot <- ggplot(data=without_disability_depression, aes(x=Time.Period.Label, y=Value, group=1)) + geom_line(color="red") + geom_point() + xlab("Time Period") + ylab("Percentage of Population w/ Indicators of Depression") +
  ggtitle("Indicators of Depression for Non-Disabled People") + theme(axis.text = element_text(size = 8))

both_disability_depression_plot <- ggplot() + geom_line(data = disability_depression, aes(x=Time.Period.Label, y=Value, group=1), color = "Blue") +
  geom_line(data=without_disability_depression, aes(x=Time.Period.Label, y=Value, group=1), color = "Red") + 
  xlab("Time Period") +
  ylab("Percentage of Population w/ Indicators of Depression") +
  scale_color_manual(name = "color", 
                     values = c("blue"="blue"))


disability_anxiety <- mental_health_data %>%
  filter(Indicator == "Symptoms of Anxiety Disorder" & Group == "By Disability status") %>%
  select(Subgroup, Value, Time.Period.Label) %>%
  filter(Value != "NA") %>%
  filter(Subgroup =="With disability")

disability_anxiety_plot <- ggplot(data=disability_anxiety, aes(x=Time.Period.Label, y=Value, group=1)) + geom_line(color="Blue") + geom_point() + xlab("Time Period") + ylab("Percentage of Population w/ Indicators of Anxiety") + 
  ggtitle("Indicators of Anxiety for Disabled People") + theme(axis.text = element_text(size = 8))

without_disability_anxiety <- mental_health_data %>%
  filter(Indicator == "Symptoms of Anxiety Disorder" & Group == "By Disability status") %>%
  select(Subgroup, Value, Time.Period.Label) %>%
  filter(Value != "NA") %>%
  filter(Subgroup =="Without disability")

without_disability_anxiety_plot <- ggplot(data=without_disability_anxiety, aes(x=Time.Period.Label, y=Value, group=1)) + geom_line(color="red") + geom_point() + xlab("Time Period") + ylab("Percentage of Population w/ Indicators of Anxiety") +
  ggtitle("Indicators of Anxiety for Non-Disabled People") + theme(axis.text = element_text(size = 8))

both_disability_anxiety_plot <- ggplot() + geom_line(data = disability_anxiety, aes(x=Time.Period.Label, y=Value, group=1), color = "Blue") +
  geom_line(data=without_disability_anxiety, aes(x=Time.Period.Label, y=Value, group=1), color = "Red") + 
  xlab("Time Period") +
  ylab("Percentage of Population w/ Indicators of Anxiety") +
  scale_color_manual(name = "color", 
                     values = c("blue"="blue"))