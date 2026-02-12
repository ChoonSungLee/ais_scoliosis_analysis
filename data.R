# data.R
library(here)
library(tidyverse)

# 1. 데이터 불러오기 (선생님의 실제 파일명으로 수정하세요)
# 만약 파일명이 ais_data.csv가 아니라면 아래 이름을 바꾸시면 됩니다.
raw_data <- read.csv(here("ais_data.csv")) 

# 2. 데이터 가공 (T2, T3, T4의 차이를 계산)
# 선생님 논문의 핵심인 '오목측 - 볼록측' 차이를 만듭니다.
processed_data <- raw_data %>%
  mutate(
    # 보통 우측이 오목(Concave)이므로 우측 - 좌측을 계산합니다.
    width_diff = pedicle_width_right_mm - pedicle_width_left_mm,
    # T1~T6를 숫자로 변환 (T1=1, T2=2...)
    level_numeric = as.integer(factor(vertebra_level, levels = c("T1", "T2", "T3", "T4", "T5", "T6")))
  )

<<<<<<< HEAD
# --- Derived Variables (for modeling) ---
# From the above, the following variables can be derived in R before passing to Stan:
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
# pedicle_width_convex_mm: Pedicle width on the convex side of the PT curve.
# pedicle_width_difference_mm: pedicle_width_concave_mm - pedicle_width_convex_mm. This is likely the primary outcome variable.
# vertebra_level_numeric: Numeric representation of vertebra_level (1 for T1, 2 for T2, ..., 6 for T6).
=======
# 3. Stan에게 넘겨줄 데이터 꾸러미 만들기 (가장 중요!)
stan_data <- list(
  N = nrow(processed_data),
  vertebra_level_numeric = processed_data$level_numeric,
  pedicle_width_diff = processed_data$width_diff
)
>>>>>>> 28649e4a6055446ceac0a0ad45b3955146b5be19

print("데이터 준비 완료! 이제 Stan 모델을 실행할 수 있습니다.")