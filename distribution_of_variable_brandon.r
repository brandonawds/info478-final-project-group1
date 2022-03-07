library(dplyr)
library(tidyverse)
library(ggplot2)

mental_health_data <- read.csv("mental_health_data.csv")

depressive_disorder <- mental_health_data %>%
  filter(Indicator == "Symptoms of Depressive Disorder" & Phase == "1" & Time.Period == "1" & Group == "By State") %>%
  select(State, Value) %>%
  arrange(Value)

depressive_disorder_indicators <- depressive_disorder$Value

depression_by_state <- hist(depressive_disorder_indicators,main="Indicators of Depressive Disorder in the US", xlab="Indicators of Depressive Disorders")

# Meena's Code

my_data <- read.csv("Raw_data_A dataset for emotional reactions and family resilience during COVID-19 isolation period among Indonesian families.csv")

my_data_filtered <- my_data %>% select(family_COVID19_status, sadness_scored, happiness_scored)

filtered_status1 <- my_data_filtered %>% filter(family_COVID19_status == "1")
filtered_status2 <- my_data_filtered %>% filter(family_COVID19_status == "2")
filtered_status3 <- my_data_filtered %>% filter(family_COVID19_status == "3")

avgSad1 <- mean(filtered_status1$sadness_scored)
avgHappy1 <- mean(filtered_status1$happiness_scored)

avgSad2 <- mean(filtered_status2$sadness_scored)
avgHappy2 <- mean(filtered_status2$happiness_scored)

avgSad3 <- mean(filtered_status3$sadness_scored)
avgHappy3 <- mean(filtered_status3$happiness_scored)

finalTable <- matrix(c(avgHappy1,avgSad1,avgHappy2,avgSad2,avgHappy3,avgSad3),ncol = 3, nrow = 2, byrow = TRUE)
rownames(finalTable) <- c("1","2")
colnames(finalTable) <- c("Home Isolation", "Under Monitoring", "Hospitalized")

covid_emotions_plot <- barplot(finalTable, main="Covid19 Status Vs. Sad and Happy Rates (1-5 scale)",
                               xlab="COVID19 Quarantine Status", col=c("darkblue","chocolate3"),
                               legend = (c("Sad","Happy")), beside=TRUE, ylim = c(0,5))
# Fatin Code
p <- read.csv("mental_health_data.csv")

phaseone <- p[p$Phase == '1',]

phasetwo <- p[p$Phase == '2',]

phasethree <- p[p$Phase == '3 (Jan 6 � Mar 29)',]


phaseonemean <- mean(phaseone$Value)
phasetwomean <- mean(phasetwo$Value)
phasethreemean <- mean(phasethree$Value)

c1 <- c(phaseonemean, phasetwomean, phasethreemean)
c2 <- c("Phase 1", "Phase 2", "Phase 3")

means <- data.frame(c1, c2)

g <- ggplot(data=means, aes(x=c2, y=c1)) +
  geom_bar(stat="identity", width = 0.5, fill="steelblue") + theme_minimal()

