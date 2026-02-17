# main_analysis.R (실행 및 시각화)
library(rstan)
library(here)

# main_analysis.R 상단에 추가 (멀티코어 수술 지원)
options(mc.cores = parallel::detectCores()) # CPU 코어를 모두 사용합니다.
rstan_options(auto_write = TRUE)            # 바뀐 게 없다면 재컴파일을 건너뜁니다.

# 1. 레시피 호출 (데이터 준비)
source(here("data.R"))

# 2. Stan 모델 가동 (어제 만든 simple_model.stan 사용)
fit <- stan(
  file = here("simple_model.stan"),
  data = stan_data,
  iter = 2000, chains = 4, core = 4
)

# 3. 결과 확인: T2 레벨의 오목/볼록측 차이 확인
print(fit, pars = c("mu")) # 평균값 추정치 확인

# 4. 시각화 (선생님이 가장 좋아하시는 사후분포 그래프) 
plot(fit, show_density = TRUE, pars = "mu")

# main_analysis.R 의 마지막 부분에 추가하세요

# --- 4. 통계 결과 요약 및 정렬 (기존 코드) ---
summary_fit <- summary(fit)$summary
# 1. 전체 파라미터(lp__, y_rep 포함)를 평균 순으로 정렬
sorted_summary <- summary_fit[order(summary_fit[, "mean"]), c("mean", "sd", "2.5%", "97.5%")]

print("=== 1. 전체 결과 정렬 (lp__ 포함 버전) ===")
print(head(sorted_summary, 10)) # 너무 기니까 상위 10개만 먼저 봅니다.


# --- 4-1. mu(마디별 평균)만 골라내기 (새로운 코드 추가) ---
# 2. 행 이름(rownames) 중에 "mu["가 포함된 것만 필터링합니다.
mu_summary <- summary_fit[grep("mu\\[", rownames(summary_fit)), ]

# 3. 골라낸 mu들만 다시 평균 순으로 정렬합니다.
sorted_mu <- mu_summary[order(mu_summary[, "mean"]), c("mean", "sd", "2.5%", "97.5%")]

print("=== 2. 핵심 마디(mu)만 정렬 (교수님 맞춤형) ===")
print(sorted_mu)

# --- 5. 시각화 (LA 아침의 선물 세트) ---
library(bayesplot)
library(ggplot2)

# 파라미터 이름을 직관적으로 변경 (논문용 이름표)
posterior <- as.matrix(fit)
# mu[1,1]부터 mu[6,2]까지 순서대로 이름을 붙입니다.
# 1은 Concave(Left), 2는 Convex(Right)를 의미합니다.
target_names <- c("T1_Concave", "T1_Convex", "T2_Concave", "T2_Convex", 
                  "T3_Concave", "T3_Convex", "T4_Concave", "T4_Convex", 
                  "T5_Concave", "T5_Convex", "T6_Concave", "T6_Convex")
colnames(posterior)[1:12] <- target_names
posterior[1:6, 1:5]

# 밀도 그래프 출력
mcmc_areas(posterior, pars = target_names, prob = 0.95) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red") + # 3mm 가이드라인 추가
  ggtitle("Posterior Distributions of Pedicle Width (T1-T6)") +
  xlab("Width (mm)") +
  theme_minimal()


