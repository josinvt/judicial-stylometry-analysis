# ============================================================
# Visualization Script
# Stylometric Analysis of German Court Judgments
# Script: 04_visualization.R
# Author: Josefina Vogt
# ============================================================

# ---------- 1 Load packages ----------
required_packages <- c(
  "tidyverse",
  "tidytext",
  "tokenizers",
  "stringr",
  "ggplot2"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ---------- 2 Create results folder ----------
if (!dir.exists("results")) {
  dir.create("results")
}

# ---------- 3 Load dataset ----------
data <- read.csv("data/judgments_clean.csv", stringsAsFactors = FALSE)

# ---------- 4 Tokenize words ----------
tokens <- data %>%
  unnest_tokens(word, text)

# ---------- 5 Word frequency by category ----------
word_freq <- tokens %>%
  count(category, word, sort = TRUE)

top_words <- word_freq %>%
  group_by(category) %>%
  slice_max(n, n = 15)

# ---------- 6 Plot: Top Words ----------
plot_words <- ggplot(top_words,
                     aes(x = reorder(word, n), y = n, fill = category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~category, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most Frequent Words in Civil vs Criminal Judgments",
    x = "Word",
    y = "Frequency"
  )

ggsave(
  "results/top_words_plot.png",
  plot_words,
  width = 8,
  height = 6
)

# ---------- 7 Sentence length analysis ----------
sentence_data <- data %>%
  mutate(sentences = tokenize_sentences(text)) %>%
  unnest(sentences) %>%
  mutate(length = str_count(sentences, "\\S+"))

# ---------- 8 Plot: Sentence length ----------
plot_sentences <- ggplot(sentence_data,
                         aes(x = category, y = length, fill = category)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Sentence Length Distribution in Court Judgments",
    x = "Legal Domain",
    y = "Words per Sentence"
  )

ggsave(
  "results/sentence_length_plot.png",
  plot_sentences,
  width = 8,
  height = 6
)

# ---------- 9 Word frequency comparison ----------
word_compare <- tokens %>%
  count(category, word) %>%
  group_by(word) %>%
  filter(sum(n) > 50)

plot_compare <- ggplot(word_compare,
                       aes(x = word, y = n, fill = category)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "Word Frequency Comparison Between Legal Domains",
    x = "Words",
    y = "Frequency"
  )

ggsave(
  "results/word_frequency_comparison.png",
  plot_compare,
  width = 8,
  height = 6
)

# ---------- 10 Print confirmation ----------
print("Visualizations successfully created and saved in /results folder")
