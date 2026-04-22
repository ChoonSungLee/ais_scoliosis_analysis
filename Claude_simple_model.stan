// simple_model.stan
// 2018 Spine 논문의 베이즈 재분석 (그룹 비교 버전)
// 그룹: 1=non-sPT, 2=sPT
// Side: 1=Concave(Right), 2=Convex(Left)
// Level: 1=T1, 2=T2, 3=T3, 4=T4, 5=T5, 6=T6

data {
  int<lower=0> N;
  array[N] int<lower=1, upper=6> vertebra_level_numeric;
  array[N] int<lower=1, upper=2> side_numeric;
  array[N] int<lower=1, upper=2> group_numeric;  // 1=non-sPT, 2=sPT
  array[N] real width;
}

parameters {
  // mu[group, level, side]: 2 groups x 6 levels x 2 sides = 24개 평균
  array[2] matrix[6, 2] mu;
  real<lower=0> sigma;
}

model {
  // 사전분포 (Prior): 척추경 평균 넓이 ~5mm, 표준편차 3mm
  for (g in 1:2)
    to_vector(mu[g]) ~ normal(5, 3);
  sigma ~ exponential(1);

  // 가능도 (Likelihood)
  for (i in 1:N) {
    width[i] ~ normal(
      mu[group_numeric[i]][vertebra_level_numeric[i], side_numeric[i]],
      sigma
    );
  }
}

generated quantities {

  // ─────────────────────────────────────────────────
  // 1. 그룹 간 차이: sPT - non-sPT (논문 Table 2 베이즈 버전)
  //    양수 = sPT가 더 좁음 (non-sPT가 더 넓음)
  // ─────────────────────────────────────────────────
  matrix[6, 2] diff_nonsPT_minus_sPT;

  for (k in 1:6) {
    for (s in 1:2) {
      diff_nonsPT_minus_sPT[k, s] = mu[1][k, s] - mu[2][k, s];
    }
  }

  // ─────────────────────────────────────────────────
  // 2. 핵심 확률: non-sPT가 sPT보다 넓을 확률
  //    = P(non-sPT > sPT) — 논문의 p-value 대응
  // ─────────────────────────────────────────────────
  matrix[6, 2] prob_nonsPT_wider;

  for (k in 1:6) {
    for (s in 1:2) {
      prob_nonsPT_wider[k, s] = (mu[1][k, s] > mu[2][k, s]);
    }
  }

  // ─────────────────────────────────────────────────
  // 3. 임상 안전 확률: 각 그룹에서 3mm 미만일 확률
  //    (3mm = 표준 나사 삽입 최소 안전 마진)
  // ─────────────────────────────────────────────────
  matrix[6, 2] prob_narrow_nonsPT;
  matrix[6, 2] prob_narrow_sPT;

  for (k in 1:6) {
    for (s in 1:2) {
      prob_narrow_nonsPT[k, s] = (mu[1][k, s] < 3.0);
      prob_narrow_sPT[k, s]    = (mu[2][k, s] < 3.0);
    }
  }

  // ─────────────────────────────────────────────────
  // 4. Posterior predictive check (모델 검증용)
  // ─────────────────────────────────────────────────
  array[N] real y_rep;
  for (n in 1:N) {
    y_rep[n] = normal_rng(
      mu[group_numeric[n]][vertebra_level_numeric[n], side_numeric[n]],
      sigma
    );
  }
}
