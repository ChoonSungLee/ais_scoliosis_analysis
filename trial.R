# pedicle_width_concave_mm: Pedicle width on the concave side of the PT curve.
# Histogram of pedicle width difference distribution with Bayesian interpretation
library(ggplot2)

# Create histogram with Bayesian perspective
ggplot(ais_data, aes(x = pedicle_width_difference_mm)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(pedicle_width_difference_mm, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(pedicle_width_difference_mm, na.rm = TRUE)), color = "green", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of Pedicle Width Difference in AIS Patients",
    subtitle = "Red: Mean (Prior specification) | Green: Median (Robust estimate)",
    x = "Pedicle Width Difference (mm)",
    y = "Frequency",
    caption = "Bayesian interpretation: This empirical distribution informs prior specification"
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 10, color = "gray40"))

# Summary statistics for Bayesian prior elicitation
cat("Summary statistics for Bayesian prior specification:\n")
cat("Mean:", mean(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")
cat("SD:", sd(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")
cat("Median:", median(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")
cat("IQR:", IQR(ais_data$pedicle_width_difference_mm, na.rm = TRUE), "\n")