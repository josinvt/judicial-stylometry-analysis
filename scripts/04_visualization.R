library(tidyverse)
library(tidytext)

data <- read.csv("data/judgments_clean.csv")

tokens <- data %>%
  unnest_tokens(word, text)

word_freq <- tokens %>%
  count(category, word, sort = TRUE)

top_words <- word_freq %>%
  group_by(category) %>%
  slice_max(n, n = 10)

plot <- ggplot(top_words,
               aes(x = reorder(word, n), y = n, fill = category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~category, scales = "free") +
  coord_flip() +
  theme_minimal()

ggsave("results/top_words_plot.png", plot)
