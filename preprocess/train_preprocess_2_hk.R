setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
library(tidyverse)

train_activity <- read_csv('preprocess/train_activity_preprocess_1.csv')
train_label <- read_csv('preprocess/train_label_preprocess_1.csv')
train_combat <- read_csv('raw/train_combat.csv')
train_pledge <- read_csv('raw/train_pledge.csv')
train_trade <- read_csv('raw/train_trade.csv')
train_payment <- read_csv('raw/train_payment.csv')


# 최초~최종접속기간이 28일인 계정의 메인 캐릭터 
allday_id <- train_activity %>% filter(day == 1) %>% select(acc_id) %>% distinct() %>% .$acc_id

main_char <- train_activity %>% filter(acc_id %in% allday_id) %>%
  group_by(acc_id, char_id) %>% summarise(playtime = sum(playtime))
main_char <- main_char %>% group_by(acc_id) %>% filter(playtime == max(playtime))

# write.csv(main_char, 'preprocess/train_main_char_hk.csv', row.names = FALSE)

# train_activity 변수 요약 ---------------------------------------------------------------------------
# main_char <- read_csv('preprocess/train_main_char_hk.csv')

train_activity_main <- train_activity %>% 
  filter(acc_id %in% main_char$acc_id, char_id %in% main_char$char_id)
train_activity_main <- train_activity_main %>% arrange(day, acc_id)


# 주요 활동
activity_main <- train_activity_main %>% group_by(acc_id) %>% 
  select(acc_id, playtime, npc_kill, rich_monster, death, fishing, private_shop, enchant_count) %>%
  summarise_all(sum)

# 경험치
solo_unit <- train_activity %>% filter(solo_exp > 0) %>% .$solo_exp %>% min()
party_unit <- train_activity %>% filter(party_exp > 0) %>% .$party_exp %>% min()
quest_unit <- train_activity %>% filter(quest_exp > 0) %>% .$quest_exp %>% min()

exp_main <- train_activity_main %>% 
  mutate(solo_exp = solo_exp / solo_unit,
         party_exp = party_exp / party_unit,
         quest_exp = quest_exp / quest_unit) %>%
  transmute(acc_id, total_exp = solo_exp + party_exp + quest_exp) %>% 
  mutate(total_exp = total_exp / sd(total_exp)) %>%
  group_by(acc_id) %>% summarise_all(sum)

# 아데나 변동량
money_main <- train_activity_main %>% 
  select(day, acc_id, game_money_change) %>%
  mutate(money_sign = ifelse(game_money_change >= 0, 'game_money_up', 'game_money_down')) %>%
  
  group_by(acc_id, money_sign) %>%
  summarise(game_money_change = sum(game_money_change)) %>% 
  
  spread(key = money_sign, value = game_money_change) %>%
  mutate(game_money_down = ifelse(is.na(game_money_down), 0, game_money_down),
         game_money_up = ifelse(is.na(game_money_up), 0, game_money_up))

# 활동일수, 캐릭터 수
acc_main <- train_activity %>%
  group_by(acc_id, char_id) %>% 
  summarise(acc_day_cnt = last(acc_day), char_day_cnt = last(char_day))
acc_main <- acc_main %>% group_by(acc_id) %>%
  summarise(acc_day_cnt = last(acc_day_cnt), char_day_cnt = last(char_day_cnt), char_cnt = n())

# 다 합치기
activity_main <- activity_main %>% left_join(select(main_char, -playtime))
activity_main <- activity_main %>% left_join(exp_main)
activity_main <- activity_main %>% left_join(money_main)
activity_main <- activity_main %>% left_join(acc_main)

activity_main <- activity_main %>%
  select(acc_id, char_id, acc_day_cnt, char_day_cnt, char_cnt,
         playtime, total_exp, npc_kill, rich_monster, death, fishing, private_shop,
         game_money_down, game_money_up, enchant_count)

# write.csv(activity_main, 'preprocess/train_main_activity.csv', row.names = FALSE)
# rm(exp_main, money_main, acc_main, train_activity_main)


