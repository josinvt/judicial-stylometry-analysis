# ============================================================
# Stylometric Analysis of German Court Judgments
# Script: 03_analysis.R
# Author: Josefina Vogt
# Course: Digital Humanities
# ============================================================

# ---------- 1. Load required packages ----------
required_packages <- c(
  "tidyverse",
  "quanteda",
  "caret",
  "ggplot2"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ---------- 2. Create results directory if missing ----------
if (!dir.exists("results")) {
  dir.create("results")
}

# ---------- 3. Load cleaned dataset ----------
data <- read.csv("data/judgments_clean.csv", stringsAsFactors = FALSE)

# Check required columns
if (!all(c("text", "category") %in% colnames(data))) {
  stop("Dataset must contain 'text' and 'category' columns.")
}

# Convert category to factor
data$category <- as.factor(data$category)

# ---------- 4. Create corpus ----------
corp <- corpus(data$text)

# ---------- 5. Tokenization ----------
tokens <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE
)

# ---------- 6. Create document-feature matrix ----------
dfm_matrix <- dfm(tokens)

# Keep top 200 most frequent features
top_features <- topfeatures(dfm_matrix, 200)
dfm_reduced <- dfm_select(dfm_matrix, pattern = names(top_features))

# Convert to matrix
feature_matrix <- convert(dfm_reduced, to = "matrix")

# ---------- 7. Principal Component Analysis ----------
pca <- prcomp(feature_matrix, scale. = TRUE)

pca_df <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  category = data$category
)

# ---------- 8. Plot PCA ----------
p <- ggplot(pca_df, aes(PC1, PC2, color = category)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Stylometric PCA of Civil vs Criminal Judgments",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Legal Domain"
  )

ggsave("results/pca_plot.png", plot = p, width = 8, height = 6)

# ---------- 9. Train/Test Split ----------
set.seed(123)

train_index <- createDataPartition(data$category, p = 0.8, list = FALSE)

train_data <- feature_matrix[train_index, ]
test_data <- feature_matrix[-train_index, ]

train_labels <- data$category[train_index]
test_labels <- data$category[-train_index]

# ---------- 10. Train classification model ----------
model <- train(
  x = train_data,
  y = train_labels,
  method = "glm",
  family = "binomial"
)

# ---------- 11. Predict ----------
predictions <- predict(model, test_data)

# ---------- 12. Confusion Matrix ----------
cm <- confusionMatrix(predictions, test_labels)

# Save confusion matrix
write.csv(
  as.data.frame(cm$table),
  "results/confusion_matrix.csv"
)

# Save accuracy
accuracy <- cm$overall["Accuracy"]

writeLines(
  paste("Classification Accuracy:", round(accuracy, 3)),
  "results/accuracy.txt"
)

# ---------- 13. Print results ----------
print(cm)
print(paste("Accuracy:", round(accuracy, 3)))

# ============================================================
# End of script
# ============================================================
