# data.R (레시피 스크립트)
library(readxl)
library(tidyverse)
library(here) # 파일 경로 안전장치

# 1. 엑셀 파일 불러오기 (박건보 교수님 자료)
# 엑셀 파일이 프로젝트 루트 폴더에 있다고 가정합니다.
raw_df <- read_excel(here("kbpark_modify.xlsm"), skip = 1)

# 2. 필요한 변수만 추출 (T1~T6 척추경 넓이)
# Lt: Convex(볼록), Rt: Concave(오목) 가설 기반
clean_df <- raw_df %>%
  select(ID, 
         starts_with("LtT"), # LtT1 ~ LtT6
         starts_with("RtT")) %>% # RtT1 ~ RtT6
  # 분석을 위해 길쭉한 형태(Long format)로 변환합니다.
  pivot_longer(cols = -ID, names_to = "key", values_to = "width") %>%
  mutate(
    Level = str_extract(key, "T\\d+"), # T1, T2 등 추출
    Side = ifelse(str_detect(key, "Lt"), "Convex", "Concave")
  ) %>%
  filter(!is.na(width), width > 0) # 결측치 및 0값 제외

# 3. Stan 모델에 넣을 데이터 리스트 생성
#stan_data <- list(
#  N = nrow(clean_df),
#  width = clean_df$width,
#  level_idx = as.integer(factor(clean_df$Level)), # T1=1, T2=2...
#  side_idx = ifelse(clean_df$Side == "Concave", 1, 2) # Concave=1, Convex=2
#)



# data.R 의 마지막 부분을 아래와 같이 수정하고 저장하세요!

# 3. Stan 모델의 변수명(vertebra_level_numeric)과 완벽하게 일치시킵니다.
stan_data <- list(
  N = nrow(clean_df),
  width = as.numeric(clean_df$width),
  # Stan 파일 6행에서 요구하는 그 이름입니다!
  vertebra_level_numeric = as.integer(factor(clean_df$Level)), 
  side_numeric = ifelse(clean_df$Side == "Concave", 1, 2)
)






