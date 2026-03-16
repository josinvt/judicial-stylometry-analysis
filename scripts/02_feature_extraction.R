library(tidytext)
library(dplyr)

data <- read.csv("data/judgments_clean.csv")

tokens <- data %>%
  unnest_tokens(word, text)

word_counts <- tokens %>%
  count(word, sort = TRUE)

print(head(word_counts))
