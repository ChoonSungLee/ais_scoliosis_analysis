# data.R (척추경 넓이 전용 레시피 - 그룹 비교 버전)
# Gr=1: non-sPT (113명), Gr=2: sPT (69명)
# 좌측 PT curve이므로 Rt(right) = concave, Lt(left) = convex

library(readxl)
library(tidyverse)
library(here)

# 1. 엑셀 로드 (행 1이 superheader, 행 2가 컬럼명)
raw_df <- read_excel(here("kbpark_modify.xlsm"), skip = 1)

# 2. 중복 컬럼명 문제 해결: 위치 기반으로 직접 선택
# Pedicle width 섹션: LtT1~LtL5 (33~49열), RtT1~RtL5 (50~66열) [1-based]
# R에서는 0-based이므로 select로 위치 지정

# 컬럼명 직접 지정 (위치 기반)
lt_names <- paste0("Lt", c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12",
                            "L1","L2","L3","L4","L5"))
rt_names <- paste0("Rt", c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12",
                            "L1","L2","L3","L4","L5"))

# 위치로 선택 후 이름 부여
pedicle_df <- raw_df %>%
  select(1, 2, 33:66) %>%   # Gr(1), ID(2), LtT1~RtL5(33-66)
  setNames(c("Gr", "ID", lt_names, rt_names)) %>%
  filter(!is.na(ID))

# 3. Long format 변환
clean_df <- pedicle_df %>%
  pivot_longer(
    cols = c(all_of(lt_names), all_of(rt_names)),
    names_to = "key",
    values_to = "width"
  ) %>%
  mutate(
    Level      = str_extract(key, "[TL]\\d+"),
    Side_Label = ifelse(str_detect(key, "^Lt"), "Left", "Right"),
    # 좌측 PT curve: Right = Concave, Left = Convex
    Side_ConcaveConvex = ifelse(Side_Label == "Right", "Concave", "Convex")
  ) %>%
  filter(
    !is.na(width),
    width > 0,
    Level %in% c("T1","T2","T3","T4","T5","T6")
  ) %>%
  mutate(
    level_idx     = as.integer(factor(Level, levels = c("T1","T2","T3","T4","T5","T6"))),
    # side_numeric: 1=Concave(Right), 2=Convex(Left)
    side_numeric  = ifelse(Side_ConcaveConvex == "Concave", 1, 2),
    # group_numeric: 1=non-sPT (Gr=1), 2=sPT (Gr=2)
    group_numeric = as.integer(Gr)
  )

# 4. 데이터 확인
cat("총 관측치:", nrow(clean_df), "\n")
cat("그룹별 분포:\n")
print(table(clean_df$group_numeric, clean_df$Side_ConcaveConvex))

# 5. Stan 데이터 리스트
stan_data <- list(
  N                      = nrow(clean_df),
  width                  = as.numeric(clean_df$width),
  vertebra_level_numeric = clean_df$level_idx,
  side_numeric           = clean_df$side_numeric,
  group_numeric          = clean_df$group_numeric
)
