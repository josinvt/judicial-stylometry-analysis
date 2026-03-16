library(quanteda)
library(ggplot2)

data <- read.csv("data/judgments_clean.csv")

corp <- corpus(data$text)

tokens <- tokens(corp, remove_punct = TRUE)

dfm_matrix <- dfm(tokens)

topfeat <- topfeatures(dfm_matrix, 100)

dfm_reduced <- dfm_select(dfm_matrix, pattern = names(topfeat))

matrix <- convert(dfm_reduced, to = "matrix")

pca <- prcomp(matrix)

pca_df <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  category = data$category
)

plot <- ggplot(pca_df, aes(PC1, PC2, color = category)) +
  geom_point(size = 3) +
  theme_minimal()

ggsave("results/pca_plot.png", plot)
