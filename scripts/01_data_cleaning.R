# Data cleaning script

library(tidyverse)

data <- read.csv("data/judgments.csv")

data_clean <- data %>%
  mutate(text = tolower(text))

write.csv(data_clean, "data/judgments_clean.csv")
