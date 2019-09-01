options(scipen = 100)
library(tidyverse)
library(lubridate)

make.data <- function(set = 'train') {
  library(glue)
  library(reshape2) 
  
  train_activity <- read_csv(glue('raw/{set}_activity.csv'))
  train_combat <- read_csv(glue('raw/{set}_combat.csv'))
  train_payment <- read_csv(glue('raw/{set}_payment.csv'))
  train_pledge <- read_csv(glue('raw/{set}_pledge.csv'))
  train_trade <- read_csv(glue('raw/{set}_trade.csv'))
  
  ########################################
  ############ ID collection #############
  ########################################
  activity_day_id <- train_activity %>% select(day, acc_id) %>% distinct()
  payment_day_id <- train_payment %>% select(day, acc_id) %>% distinct()
  day_id <- activity_day_id %>% full_join(payment_day_id) %>% distinct() %>% arrange(acc_id, day)
  
  rm(activity_day_id, payment_day_id)
  
  ###################################
  ############ Activity #############
  ###################################
  
  min_unit <- apply(train_activity[,-(1:4)], 2, function(x) min(x[x > 0]))  
  server_special <- c("bj", "bk", "bl", "bm", "bn" ,"bo")

  # day & acc_id
  train_activity <- train_activity %>%   
    mutate(total_exp = solo_exp/min_unit['solo_exp'] + 
             party_exp/min_unit['party_exp'] + 
             quest_exp/min_unit['party_exp']) %>%
    mutate(solo_exp_per = (solo_exp/min_unit['solo_exp'])/total_exp, 
           party_exp_per = (party_exp/min_unit['party_exp'])/total_exp) %>%
    mutate(total_exp = total_exp/sd(total_exp), revive_per = revive/death) 
  
  train_activity_day_acc_id <- train_activity %>% group_by(day, acc_id) %>% 
    summarize(char_cnt_act = n(), 
              playtime = sum(playtime),
              total_exp = sum(total_exp), 
              party_exp = sum(party_exp), 
              fishing = sum(fishing), 
              private_shop = sum(private_shop),
              game_money_change = sum(game_money_change),
              enchant_count = sum(enchant_count)) 
  
  train_activity_final <- day_id %>% left_join(train_activity_day_acc_id)
  
  rm(train_activity_day_acc_id)
  
  ###################################
  ############ T R A D E ############
  ###################################
  train_trade <- train_trade %>% mutate(hour = hour(time))
  
  train_trade_t <- train_trade %>% 
    rename(acc_id = target_acc_id) %>%
    count(acc_id, day, type)
  train_trade_t <- train_trade_t %>% spread(type, n, fill = 0)
  train_trade_t <- train_trade_t %>% rename(purchase_ex = '1', purchase_pr = '0')
  
  train_trade_final <- day_id %>% left_join(train_trade_t)
  train_trade_final <- train_trade_final %>%
    mutate_all(function(x) ifelse(is.na(x), 0, x))

  rm(train_trade_t)
  
  ###################################
  ############ Combat ###############
  ###################################

  train_combat_1 <- train_combat %>% 
    mutate(combat_cnt = random_attacker_cnt + temp_cnt + etc_cnt) %>% 
    group_by(day, acc_id) %>% 
    summarize(sum_level = sum(level),
              combat_cnt = sum(combat_cnt))
  
  train_combat_final <- day_id %>% left_join(train_combat_1) 
  
  rm(train_combat_1)
  
  ###################################
  ############ Pledge ###############
  ###################################
  main_char <- train_activity %>% 
    group_by(day, acc_id, char_id) %>% summarize(playtime = sum(playtime))
  main_char <- main_char %>% group_by(day, acc_id) %>%
    filter(playtime == max(playtime)) %>% 
    filter(playtime == first(playtime)) 

  train_pledge_1 <- train_pledge %>% 
    select(day, acc_id, char_id, pledge_id) %>% distinct() %>% 
    arrange(day, acc_id, char_id, pledge_id) 
  train_pledge_1 <- main_char %>% inner_join(train_pledge_1) 
  
  train_pledge_2 <- train_pledge %>% 
    group_by(day, pledge_id) %>% 
    summarize(play_char_cnt_pld = sum(play_char_cnt), 
              pledge_combat_cnt_pld = sum(pledge_combat_cnt),
              combat_play_time_pld = sum(combat_play_time)) %>%
    arrange(day, desc(play_char_cnt_pld), pledge_id)
  
  train_pledge_final <- train_pledge_1 %>% left_join(train_pledge_2) 
  
  train_pledge_final <- ungroup(train_pledge_final) %>% 
    group_by(day, acc_id) %>% filter(playtime == max(playtime))
  train_pledge_final <- ungroup(train_pledge_final) %>%
    group_by(day, acc_id) %>% 
    filter(play_char_cnt_pld == max(play_char_cnt_pld), 
           pledge_combat_cnt_pld == max(pledge_combat_cnt_pld),
           combat_play_time_pld == max(combat_play_time_pld))

  train_pledge_final <- train_pledge_final %>% select(-char_id, -pledge_id, -playtime) %>% distinct()
  train_pledge_final <- day_id %>% left_join(train_pledge_final)
  
  rm(train_pledge_1, train_pledge_2)
  
  
  #############################
  ########## Summary ##########
  #############################
  
  ##### ALL Join 
  train_table <- day_id %>% left_join(train_activity_final) 
  train_table <- train_table %>% left_join(train_payment, by = c("day", "acc_id"))
  train_table <- train_table %>% left_join(train_trade_final, by = c("day", "acc_id"))
  train_table <- train_table %>% left_join(train_combat_final, by = c("day", "acc_id"))
  train_table <- train_table %>% left_join(train_pledge_final, by = c("day", "acc_id"))
  
  train_table <- train_table %>% mutate_all(function(x) ifelse(is.na(x), 0, x))
  
  train_table <- train_table %>% 
    mutate(week = ifelse(day < 8, 1, ifelse(day < 15, 2, ifelse(day < 22, 3, 4))))
  
  
  ##### Risk Ratio 
  day_all <- train_table %>% select(acc_id, day, week) %>% arrange()
  day_all <- day_all %>% group_by(acc_id) %>% 
    mutate(day_diff = c(0, diff(day)))
  day_all <- day_all %>% group_by(acc_id) %>% 
    mutate(risk_ratio = day_diff / mean(day_diff, na.rm = T)) %>%
    mutate(risk_ratio = ifelse(is.nan(risk_ratio), 0, risk_ratio))
  
  train_table <- select(day_all, -day_diff) %>% left_join(train_table)
  # write.csv(train_table, 'preprocess/train_table_2.csv', row.names = F)

  
  ##### Train Table Summary
  train_week <- train_table %>% group_by(acc_id, week) %>% summarize_all(mean) %>% select(-day)
  train_week <- train_week %>% melt(id = c('acc_id', 'week'))
  train_week <- train_week %>% 
    mutate(var_w = str_c(variable, week, sep = '_w')) %>%
    select(-week, -variable)
  train_week <- train_week %>% spread(var_w, value)
  
  
  ##### First Week index 
  first_week <- ungroup(train_table) %>% select(day, acc_id) %>% arrange(acc_id, day) 
  first_week <- first_week %>% mutate(index = ifelse(acc_id == lag(acc_id), 1, 0))
  first_week <- first_week[-which(first_week$index == 1),]
  first_week <- first_week %>% 
    transmute(acc_id, 
              first_week = ifelse(day < 8, 1, ifelse(day < 15, 2, ifelse(day < 22, 3, 4))))
  
  
  ##### ALL Summary
  train_final <- train_week %>% left_join(first_week)
  train_final <- train_final %>%
    select(acc_id, first_week, starts_with('risk'), starts_with('amount_spent'), 
           starts_with('char_cnt'), starts_with('playtime'), starts_with('total_exp'), starts_with('party_exp'), 
           starts_with('fishing'), starts_with('private_shop'), starts_with('game_money'), starts_with('enchant'), 
           starts_with('purchase'), starts_with('sum_level'), starts_with('combat_cnt'), contains('_pld_'))
  train_final <- train_final %>%
    mutate(risk_ratio_w2 = ifelse(is.na(risk_ratio_w2), risk_ratio_w1, risk_ratio_w2),
           risk_ratio_w3 = ifelse(is.na(risk_ratio_w3), risk_ratio_w2, risk_ratio_w3),
           risk_ratio_w4 = ifelse(is.na(risk_ratio_w4), risk_ratio_w3, risk_ratio_w4))
  
  train_final <- data.frame(train_final[,(1:6)], scale(train_final[,-(1:6)], center = F))
  train_final <- train_final %>% mutate_all(function(x) ifelse(is.na(x), 0, x))

  return(train_final)
}

