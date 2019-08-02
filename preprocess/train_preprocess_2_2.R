setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
library(tidyverse)

main_activity <- read_csv('preprocess/train_main_activity.csv')
main_trade <- read_csv('preprocess/train_main_trade.csv')
main_pledge <- read_csv('preprocess/train_main_pledge.csv')
main_combat <- read_csv('preprocess/train_main_combat.csv')

train_main <- main_activity %>% left_join(main_trade)
train_main <- train_main %>% left_join(select(main_pledge, 1, 2, 4, 5))
train_main <- train_main %>% left_join(select(main_combat, -6))

sapply(train_main, function(x) sum(is.na(x)))

train_main <- train_main %>% rename(pledge_combat_play_time = combat_play_time)
summary(train_main)
colnames(train_main)

# csv 저장
write.csv(train_main, 'preprocess/train_main.csv', row.names = FALSE)
