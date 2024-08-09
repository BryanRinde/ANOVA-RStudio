# Load necessary libraries
library(dplyr)
library(ggplot2)
library(broom)
library(ggsignif)

# Manually input the data
data <- data.frame(
  Category = factor(rep(c("A", "B", "C", "D"), each = 2)),
  Measurement = factor(rep(c("1.1", "1.2"), times = 4)),
  Value = c(1, 2, 3, 4)
)

# Calculate means and standard errors
summary_data <- data %>%
  group_by(Category, Measurement) %>%
  summarise(
    Mean = mean(Value),
    SE = sd(Value) / sqrt(n()),  # Standard Error
    .groups = 'drop'
  )

# Perform ANOVA for each Measurement type
anova_results <- summary_data %>%
  group_by(Measurement) %>%
  do({
    aov_result <- aov(Mean ~ Category, data = .)
    tidy(aov_result)
  }) %>%
  filter(term != "(Intercept)") %>%
  mutate(Category = gsub("Category", "", term)) %>%
  filter(Category != "A") %>%
  rename(p.value = p.value) %>%
  select(Measurement, Category, p.value)

# Print ANOVA results to verify
print(anova_results)

# Merge p-values with summary_data
summary_data <- summary_data %>%
  left_join(anova_results, by = c("Measurement", "Category"))

# Print merged summary_data to check
print(summary_data)

# Create the bar chart with error bars
p <- ggplot(summary_data, aes(x = Category, y = Mean, fill = Measurement)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(0.9),
                width = 0.25) +
  labs(title = "Comparison of 1.1 and 1.2 by Category",
       x = "Category",
       y = "Mean Value ± SE") +
  theme_minimal()

# Add significance annotations
p + geom_signif(
  comparisons = list(
    c("B", "A"),
    c("C", "A"),
    c("D", "DA")
  ),
  map_signif_level = TRUE,
  annotations = summary_data$p.value[summary_data$Category != "A"],
  tip_length = 0.01
)

