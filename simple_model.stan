// simple_model.stan
// A basic Bayesian linear regression model to analyze pedicle width differences across vertebral levels.
// This model estimates a mean pedicle width difference (concave_width - convex_width) for each vertebral level (T1-T6).

// simple_model.stan (좌우 구분 버전)

data {
  int<lower=0> N;
  array[N] int<lower=1, upper=6> vertebra_level_numeric;
  array[N] int<lower=1, upper=2> side_numeric; // 1: Concave, 2: Convex
  array[N] real width; // 척추경의 절대 넓이 (mm)
}

parameters {
  // 6개 레벨 x 2개 사이드 = 총 12개의 평균값을 추정합니다.
  matrix[6, 2] mu; 
  real<lower=0> sigma;
}

model {
  to_vector(mu) ~ normal(5, 3); 
  sigma ~ exponential(1);

  for (i in 1:N) {
    width[i] ~ normal(mu[vertebra_level_numeric[i], side_numeric[i]], sigma);
  }
}

generated quantities {
  // 1. 모든 레벨(T1-T6)의 좌우 평균 차이 계산
  matrix[6, 2] mu_prob_narrow; 
  for (k in 1:6) {
    for (s in 1:2) {
     // 각 레벨의 각 측면 평균이 3mm 미만일 확률 (지표 변수)
    // 3mm는 나사가 들어갈 수 있는 최소한의 안전 마진 예시입니다. 
      mu_prob_narrow[k, s] = (mu[k, s] < 3.0); 
    }
  }

// 2. 전체 데이터에 대한 사후 예측 (모델 검증용)
  array[N] real y_rep;
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu[vertebra_level_numeric[n], side_numeric[n]], sigma);
  }
}










