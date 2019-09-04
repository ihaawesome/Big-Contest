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


# train_pledge 요약 ----------------------------------------------------------------------------------------
# main char & pledge join 
train_main_char_pledge <- main_char %>% 
  left_join(train_pledge, by = c("acc_id", "char_id")) %>% 
  select(day, acc_id, char_id, pledge_id, combat_char_cnt, combat_play_time)   

# 혈맹 가입한 day가 많은 혈맹으로 대표 혈맹 구분
df <- train_main_char_pledge %>% 
  group_by(acc_id, char_id, pledge_id) %>% summarize(daycnt = n()) %>% 
  filter(daycnt == max(daycnt))

# 혈맹 가입 day가 같아서 중복 나오는 행 index처리 & 삭제
df$index <- ifelse(df$acc_id == lag(df$acc_id), 1, 0) 
df <- df[-which(df$index==1),]

train_main_char_pledge <- df %>% 
  left_join(train_main_char_pledge, c("acc_id", "char_id", "pledge_id")) %>% 
  group_by(acc_id, char_id, pledge_id) %>% 
  summarize(combat_char_cnt = sum(combat_char_cnt), 
            combat_play_time = sum(combat_play_time)) 

train_main_char_pledge <- train_main_char_pledge %>%
  mutate(combat_char_cnt = ifelse(is.na(combat_char_cnt), 0, combat_char_cnt),
         combat_play_time = ifelse(is.na(combat_play_time), 0, combat_play_time))

# CSV 저장
write.csv(train_main_char_pledge, 'preprocess/train_main_pledge.csv', row.names = FALSE)
rm(df)


# train_combat 요약 --------------------------------------------------------------------------------------
# combat table join 
df <- main_char %>% left_join(train_combat , by = c("acc_id", "char_id")) %>% 
  select(day, acc_id, char_id, class, level, pledge_cnt, random_attacker_cnt, temp_cnt, etc_cnt) %>% 
  mutate(pledge_cnt_change = pledge_cnt/0.06418424262244408,
         random_attacker_cnt_change = random_attacker_cnt/2.8472503288064885,
         temp_cnt_change = temp_cnt/0.4797277642444799,
         etc_cnt_change = etc_cnt/0.11261152623951591) 

# cnt 변환 후 sum
train_combat_cnt <- df %>% 
  group_by(acc_id, char_id, class) %>% 
  summarize(pledge_cnt = sum(pledge_cnt),
            pledge_cnt_change = sum(pledge_cnt_change), 
            random_attacker_cnt_change = sum(random_attacker_cnt_change), 
            temp_cnt_change = sum(temp_cnt_change),
            etc_cnt_change = sum(etc_cnt_change))
train_combat_cnt_sum <- data.frame(train_combat_cnt[,c(1:5)], 
                                   combat_cnt = apply(train_combat_cnt[,-c(1:5)], 1, sum))
#***train_combat_cnt_sum NA : 245개 - 실제로 combat table에 속하지 않음 

#max level 갖고 오기 
train_combat_max_level <- unique(df %>% group_by(acc_id, char_id) %>% 
                                   filter(level == max(level)) %>% select(acc_id, char_id, level))

#combat_최종 
train_main_char_combat <- train_combat_cnt_sum %>% 
                            left_join(train_combat_max_level, by = c("acc_id", "char_id")) %>% 
                            select(acc_id, char_id, class, level, pledge_cnt, pledge_cnt_change, combat_cnt) %>%
                            data.frame()

train_main_char_combat <- train_main_char_combat %>%
  mutate(combat_cnt = combat_cnt / sd(combat_cnt, na.rm = T))

train_main_char_combat$class <- ifelse(is.na(train_main_char_combat$class), 0, train_main_char_combat$class)
train_main_char_combat$level <- ifelse(is.na(train_main_char_combat$level), 0, train_main_char_combat$level)
train_main_char_combat$pledge_cnt <- ifelse(is.na(train_main_char_combat$pledge_cnt), 0, train_main_char_combat$pledge_cnt)
train_main_char_combat$pledge_cnt_change <- ifelse(is.na(train_main_char_combat$pledge_cnt_change), 0, train_main_char_combat$pledge_cnt_change)
train_main_char_combat$combat_cnt <- ifelse(is.na(train_main_char_combat$combat_cnt), 0, train_main_char_combat$combat_cnt)

# CSV 저장 
# write.csv(train_main_char_combat, 'preprocess/train_main_combat.csv', row.names = FALSE)
# rm(df, train_main_char_combat, train_combat_cnt, train_combat_cnt_sum, train_combat_max_level)
