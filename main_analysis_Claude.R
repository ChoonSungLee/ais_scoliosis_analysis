# main_analysis.R (실행 및 시각화 - 그룹 비교 강화 버전)
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)
library(here)

# 멀티코어 지원
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ═══════════════════════════════════════════════
# 1. 데이터 준비
# ═══════════════════════════════════════════════
source(here("data.R"))

# ═══════════════════════════════════════════════
# 2. Stan 모델 적합
# ═══════════════════════════════════════════════
fit <- stan(
  file   = here("simple_model.stan"),
  data   = stan_data,
  iter   = 2000,
  chains = 4,
  cores  = 4
)

# ═══════════════════════════════════════════════
# 3. 기본 수렴 진단
# ═══════════════════════════════════════════════
cat("\n=== 수렴 진단: mu & sigma ===\n")
print(fit, pars = c("mu", "sigma"))

# R-hat 경고 확인 (모두 < 1.05이면 수렴 양호)
summary_fit <- summary(fit)$summary
rhat_vals <- summary_fit[grep("mu\\[", rownames(summary_fit)), "Rhat"]
cat("\nR-hat 범위 (1.0에 가까울수록 수렴 양호):",
    round(min(rhat_vals), 3), "~", round(max(rhat_vals), 3), "\n")

# ═══════════════════════════════════════════════
# 4. 핵심 결과 추출 함수
# ═══════════════════════════════════════════════
levels_label <- c("T1","T2","T3","T4","T5","T6")
sides_label  <- c("Concave","Convex")
groups_label <- c("non-sPT","sPT")

# mu 평균 정리 (그룹 x 레벨 x 측면)
extract_mu_table <- function(summary_fit) {
  mu_rows <- summary_fit[grep("^mu\\[", rownames(summary_fit)), ]
  # mu[g][k,s] 파싱
  parsed <- rownames(mu_rows) %>%
    str_match("mu\\[(\\d)\\]\\[(\\d),(\\d)\\]") %>%
    as.data.frame() %>%
    setNames(c("param","g","k","s")) %>%
    mutate(
      Group = groups_label[as.integer(g)],
      Level = levels_label[as.integer(k)],
      Side  = sides_label[as.integer(s)],
      Mean  = round(mu_rows[,"mean"], 2),
      SD    = round(mu_rows[,"sd"],   2),
      CrI_lo = round(mu_rows[,"2.5%"], 2),
      CrI_hi = round(mu_rows[,"97.5%"], 2)
    ) %>%
    select(Group, Level, Side, Mean, SD, CrI_lo, CrI_hi)
  return(parsed)
}

mu_table <- extract_mu_table(summary_fit)

# ═══════════════════════════════════════════════
# 5. 결과 출력: 논문 Table 2 대응 (Concave만)
# ═══════════════════════════════════════════════
cat("\n=== 사후 평균 넓이 — Concave 측 비교 (논문 Table 2 베이즈 버전) ===\n")
mu_table %>%
  filter(Side == "Concave") %>%
  arrange(Level, Group) %>%
  print()

# ═══════════════════════════════════════════════
# 6. 핵심 베이즈 결론 #1: 그룹 간 차이 (Credible Interval)
# ═══════════════════════════════════════════════
cat("\n=== 핵심 결론 #1: non-sPT − sPT 차이 (Concave 측) ===\n")
cat("양수 = non-sPT가 더 넓음 (sPT의 협착이 더 심함)\n\n")

diff_rows <- summary_fit[grep("diff_nonsPT_minus_sPT", rownames(summary_fit)), ]
diff_parsed <- rownames(diff_rows) %>%
  str_match("\\[(\\d),(\\d)\\]") %>%
  as.data.frame() %>%
  setNames(c("param","k","s")) %>%
  mutate(
    Level  = levels_label[as.integer(k)],
    Side   = sides_label[as.integer(s)],
    Mean   = round(diff_rows[,"mean"],  2),
    CrI_lo = round(diff_rows[,"2.5%"], 2),
    CrI_hi = round(diff_rows[,"97.5%"],2)
  ) %>%
  filter(Side == "Concave") %>%
  select(Level, Mean, CrI_lo, CrI_hi) %>%
  mutate(
    Interpretation = case_when(
      CrI_lo > 0 ~ "★ 0 미포함 → sPT가 의미있게 더 좁음",
      CrI_hi < 0 ~ "non-sPT가 더 좁음",
      TRUE        ~ "차이 불명확 (0 포함)"
    )
  )
print(diff_parsed)

# ═══════════════════════════════════════════════
# 7. 핵심 베이즈 결론 #2: 확률적 우위
# ═══════════════════════════════════════════════
cat("\n=== 핵심 결론 #2: P(non-sPT > sPT) — Concave 측 ===\n")
cat("이 확률이 높을수록 sPT의 협착이 더 심하다는 근거가 강함\n\n")

prob_rows <- summary_fit[grep("prob_nonsPT_wider", rownames(summary_fit)), ]
prob_parsed <- rownames(prob_rows) %>%
  str_match("\\[(\\d),(\\d)\\]") %>%
  as.data.frame() %>%
  setNames(c("param","k","s")) %>%
  mutate(
    Level = levels_label[as.integer(k)],
    Side  = sides_label[as.integer(s)],
    Prob  = round(prob_rows[,"mean"], 3)
  ) %>%
  filter(Side == "Concave") %>%
  select(Level, Prob) %>%
  mutate(
    Interpretation = case_when(
      Prob >= 0.975 ~ "강력한 근거",
      Prob >= 0.90  ~ "상당한 근거",
      Prob >= 0.75  ~ "중등도 근거",
      TRUE          ~ "근거 미약"
    )
  )
print(prob_parsed)

# ═══════════════════════════════════════════════
# 8. 핵심 베이즈 결론 #3: 임상 안전 확률 (3mm 미만)
# ═══════════════════════════════════════════════
cat("\n=== 핵심 결론 #3: P(width < 3mm) — 나사 삽입 위험 확률 ===\n")

narrow_nonsPT <- summary_fit[grep("prob_narrow_nonsPT", rownames(summary_fit)), "mean"]
narrow_sPT    <- summary_fit[grep("prob_narrow_sPT",    rownames(summary_fit)), "mean"]

narrow_df <- expand.grid(k=1:6, s=1:2) %>%
  mutate(
    Level = levels_label[k],
    Side  = sides_label[s],
    P_narrow_nonsPT = round(narrow_nonsPT, 3),
    P_narrow_sPT    = round(narrow_sPT,    3)
  ) %>%
  filter(Side == "Concave") %>%
  select(Level, P_narrow_nonsPT, P_narrow_sPT)

print(narrow_df)

# ═══════════════════════════════════════════════
# 9. 시각화 #1: Concave 측 그룹 비교 밀도 그래프 (논문 핵심)
# ═══════════════════════════════════════════════
posterior <- as.matrix(fit)

# T1-T6 Concave 측 파라미터 이름 추출
# mu[1][k,1] = non-sPT Concave, mu[2][k,1] = sPT Concave
target_cols_nonsPT <- paste0("mu[1][", 1:6, ",1]")
target_cols_sPT    <- paste0("mu[2][", 1:6, ",1]")
target_labels_nonsPT <- paste0(levels_label, "_Concave_nonsPT")
target_labels_sPT    <- paste0(levels_label, "_Concave_sPT")

all_targets <- c(target_cols_nonsPT, target_cols_sPT)
all_labels  <- c(target_labels_nonsPT, target_labels_sPT)

col_idx <- match(all_targets, colnames(posterior))
colnames(posterior)[col_idx] <- all_labels

p1 <- mcmc_areas(
  posterior,
  pars  = all_labels,
  prob  = 0.95
) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 3.1, y = 12.5, label = "3mm 기준선",
           color = "red", hjust = 0, size = 3.5) +
  scale_y_discrete(
    limits = rev(all_labels),
    labels = rev(all_labels)
  ) +
  ggtitle("Concave 측 척추경 넓이: sPT vs non-sPT (사후분포, 95% CrI)") +
  xlab("척추경 넓이 (mm)") +
  theme_minimal(base_size = 12)

print(p1)
ggsave(here("fig1_concave_comparison.png"), p1, width=10, height=8, dpi=150)

# ═══════════════════════════════════════════════
# 10. 시각화 #2: T2-T4 집중 비교 (논문 Figure 2 베이즈 버전)
# ═══════════════════════════════════════════════
key_levels <- c("T2","T3","T4")
key_nonsPT <- paste0(key_levels, "_Concave_nonsPT")
key_sPT    <- paste0(key_levels, "_Concave_sPT")
key_pars   <- c(rbind(key_nonsPT, key_sPT))  # 교차 배치

p2 <- mcmc_areas(
  posterior,
  pars = key_pars,
  prob = 0.95
) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = 2, linetype = "dotted", color = "orange", linewidth = 0.8) +
  annotate("text", x = 3.05, y = 6.5, label = "3mm", color = "red", size = 3.5) +
  annotate("text", x = 2.05, y = 6.5, label = "2mm", color = "orange", size = 3.5) +
  ggtitle("T2-T4 Concave: sPT vs non-sPT — 논문 Figure 2 베이즈 버전") +
  xlab("척추경 넓이 (mm)") +
  theme_minimal(base_size = 13)

print(p2)
ggsave(here("fig2_T2T3T4_concave.png"), p2, width=9, height=6, dpi=150)

# ═══════════════════════════════════════════════
# 11. 시각화 #3: 그룹 간 차이의 사후분포
# ═══════════════════════════════════════════════
diff_cols <- paste0("diff_nonsPT_minus_sPT[", 1:6, ",1]")
diff_labels <- paste0("Diff_", levels_label, "_Concave")

diff_idx <- match(diff_cols, colnames(posterior))
colnames(posterior)[diff_idx] <- diff_labels

p3 <- mcmc_areas(
  posterior,
  pars = diff_labels,
  prob = 0.95
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  annotate("text", x = 0.05, y = 6.5, label = "차이 없음", color = "gray40", size = 3.5) +
  ggtitle("non-sPT − sPT 차이 (Concave 측): 95% Credible Interval") +
  xlab("넓이 차이 (mm) [양수 = non-sPT가 더 넓음]") +
  theme_minimal(base_size = 13)

print(p3)
ggsave(here("fig3_difference.png"), p3, width=9, height=6, dpi=150)

cat("\n=== 분석 완료 ===\n")
cat("저장된 그래프: fig1_concave_comparison.png, fig2_T2T3T4_concave.png, fig3_difference.png\n")
