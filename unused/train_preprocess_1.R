setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
library(tidyverse)
library(readr)

na.count <- function(data) sapply(data, function(x) sum(is.na(x)))

train_activity <- read_csv('raw/train_activity.csv')
train_combat <- read_csv('raw/train_combat.csv')
train_label <- read_csv('raw/train_label.csv')
train_payment <- read_csv('raw/train_payment.csv')
train_pledge <- read_csv('raw/train_pledge.csv')
train_trade <- read_csv('raw/train_trade.csv')


# 유저, 캐릭터 별 누적활동일자 인덱싱 -------------------------------------------------------
acc_day <- train_activity %>% count(acc_id, day)
acc_day <- acc_day %>% select(-n) %>% mutate(acc_day = 1)
acc_day <- acc_day %>% group_by(acc_id) %>% mutate(acc_day = cumsum(acc_day))
char_day <- train_activity %>% count(acc_id, char_id, day)
char_day <- char_day %>% select(-n) %>% mutate(char_day = 1)
char_day <- char_day %>% group_by(acc_id, char_id) %>% mutate(char_day = cumsum(char_day))

train_activity <- train_activity %>% 
  left_join(acc_day, by = c('day','acc_id')) %>%
  left_join(char_day, by = c('day','acc_id','char_id'))



# 생존기간: 활동시작 ~ 이탈/생존 시점까지 ---------------------------------------------------
first_day <- train_activity %>% 
  filter(acc_day == 1) %>% select(day, acc_id, acc_day) %>% distinct() %>% select(-acc_day) 

train_label <- train_label %>% left_join(first_day, by = 'acc_id')
train_label <- train_label %>% mutate(survival_time_y = survival_time + (28 - day))
train_label %>% filter(survival_time != 64) %>%
  ggplot() + geom_bar(aes(survival_time_y)) + theme_minimal()


# CSV 저장 ----------------------------------------------------------------------------------
write.csv(train_activity, 'preprocess/train_activity_preprocess_1.csv', row.names = FALSE)
write.csv(train_label, 'preprocess/train_label_preprocess_1.csv', row.names = FALSE)


