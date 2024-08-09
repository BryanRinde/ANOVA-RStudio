# Load necessary libraries
library(dplyr)
library(ggplot2)
library(broom)
library(ggsignif)

# Manually input the data
data <- data.frame(
  Category = factor(rep(c("DMSO", "FADS1", "FADS2", "FADS1+2"), each = 2)),
  Measurement = factor(rep(c("MGV/%Blue", "%Red/%Blue"), times = 4)),
  Value = c(2.52, 1.45, 2.88, 1.56, 2.44, 1.43, 2.26, 1.57,
            2.88, 1.54, 2.27, 1.36, 2.77, 1.55, 1.69, 0.89,
            2.09, 0.93, 1.94, 0.87, 1.15, 0.50, 1.90, 0.89,
            1.57, 0.72, 1.43, 0.60, 1.24, 0.50, 2.05, 0.85)
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
  filter(Category != "DMSO") %>%
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
  labs(title = "Comparison of MGV/%Blue and %Red/%Blue by Category",
       x = "Category",
       y = "Mean Value ± SE") +
  theme_minimal()

# Add significance annotations
p + geom_signif(
  comparisons = list(
    c("FADS1", "DMSO"),
    c("FADS2", "DMSO"),
    c("FADS1+2", "DMSO")
  ),
  map_signif_level = TRUE,
  annotations = summary_data$p.value[summary_data$Category != "DMSO"],
  tip_length = 0.01
)

