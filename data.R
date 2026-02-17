# data.R (척추경 넓이 전용 레시피)
library(readxl)
library(tidyverse)
library(here)

# 1. 엑셀 로드
raw_df <- read_excel(here("kbpark_modify.xlsm"), skip = 1)

# data.R 2번 섹션 최종 수정
clean_df <- raw_df %>%
  select(ID, 33:66) %>% 
  rename_with(~str_remove(., "\\.\\.\\..*"), -ID) %>%
  pivot_longer(cols = -ID, names_to = "key", values_to = "width") %>%
  mutate(
    Level = str_extract(key, "[TL]\\d+"), 
    Side_Label = ifelse(str_detect(key, "Lt"), "Left", "Right")
  ) %>%
  filter(!is.na(width), width > 0) %>%
  # --- 여기서 T1~T6만 골라내는 '여과지'를 추가합니다 ---
  filter(Level %in% c("T1", "T2", "T3", "T4", "T5", "T6")) %>%
  # ----------------------------------------------------
mutate(level_idx = as.integer(factor(Level, levels = c("T1", "T2", "T3", "T4", "T5", "T6"))))

# 3. Stan 데이터 리스트 (level_idx를 사용합니다)
stan_data <- list(
  N = nrow(clean_df),
  width = as.numeric(clean_df$width),
  vertebra_level_numeric = clean_df$level_idx, 
  side_numeric = ifelse(clean_df$Side_Label == "Left", 1, 2)
)

