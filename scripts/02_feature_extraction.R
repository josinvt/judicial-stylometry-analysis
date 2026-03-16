library(tidytext)

tokens <- judgments %>%
  unnest_tokens(word, text)

word_counts <- tokens %>%
  count(word, sort = TRUE)
