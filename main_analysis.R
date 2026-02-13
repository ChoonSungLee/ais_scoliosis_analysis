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

# 4. 시각화 (선생님이 가장 좋아하시는 사후분포 그래프) [cite: 18]
plot(fit, show_density = TRUE, pars = "mu")