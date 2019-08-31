# rm(list=ls())
setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
options(scipen = 100)
# options(crayon.enabled = FALSE)
library(tidyverse)
library(lubridate)

make.data <- function(set = 'train') {
  library(glue)
  train_activity <- read_csv(glue('raw/{set}_activity.csv'))
  train_combat <- read_csv(glue('raw/{set}_combat.csv'))
  train_label <- read_csv(glue('raw/{set}_label.csv'))
  train_payment <- read_csv(glue('raw/{set}_payment.csv'))
  train_pledge <- read_csv(glue('raw/{set}_pledge.csv'))
  train_trade <- read_csv(glue('raw/{set}_trade.csv'))
  
  ######################################
  ############ 기준 아이디 #############
  ######################################
  activity_day_id <- train_activity %>% select(day, acc_id) %>% distinct()
  payment_day_id <- train_payment %>% select(day, acc_id) %>% distinct()
  day_id <- activity_day_id %>% full_join(payment_day_id) %>% distinct() %>% arrange(acc_id, day)
  
  ###################################
  ############ Activity #############
  ###################################
  #특수 서버 접속 여부
  server_special <- c("bj", "bk", "bl", "bm", "bn" ,"bo")
  train_activity$server_index <- ifelse(train_activity$server %in% server_special, 1, 0)
  
  #day & acc_id 기준 정렬
  train_activity <- train_activity %>%   
    mutate(total_exp = solo_exp/0.00000007535525 + 
             party_exp/0.0000002813903 + 
             quest_exp/0.00000769816, 
           solo_exp_per = (solo_exp/0.00000007535525)/total_exp, 
           party_exp_per = (party_exp/0.0000002813903)/total_exp) %>%
    mutate(total_exp = total_exp/sd(total_exp),
           revive_per = revive/death) 
  
  train_activity_day_acc_id <- train_activity %>% group_by(day, acc_id) %>% 
    summarize(char_cnt = n(), 
              playtime = sum(playtime),
              npc_kill = sum(npc_kill),
              total_exp = sum(total_exp), 
              solo_exp = sum(solo_exp), 
              party_exp = sum(party_exp), 
              quest_exp = sum(quest_exp),
              solo_exp_per = mean(solo_exp_per), 
              party_exp_per = mean(party_exp_per), 
              rich_monster = sum(rich_monster),
              death = sum(death),
              revive_per = mean(revive_per), 
              exp_recovery = sum(exp_recovery),
              fishing = sum(fishing), 
              private_shop = sum(private_shop),
              game_money_change = sum(game_money_change),
              enchant_count = sum(enchant_count), 
              server_special = sum(server_index)) 
  
  #최종
  train_activity_final <- day_id %>% left_join(train_activity_day_acc_id)
  
  ###################################
  ############ T R A D E ############
  ###################################
  train_trade <- train_trade %>% mutate(hour = hour(time))
  
  dawn_target_id <- train_trade %>% filter(between(hour, 4, 8)) %>% 
    select(target_acc_id, day) %>% distinct() %>%
    arrange(target_acc_id, day) %>% 
    transmute(acc_id = target_acc_id, day, dawn = 1)
  dawn_source_id <- train_trade %>% filter(between(hour, 4, 8)) %>% 
    select(source_acc_id, day) %>% distinct() %>%
    arrange(source_acc_id, day) %>% 
    transmute(acc_id = source_acc_id, day, dawn = 1)
  
  train_trade_s <- train_trade %>% 
    rename(acc_id = source_acc_id) %>%
    group_by(acc_id, day) %>%
    summarise(s_item_amount = sum(item_amount),
              s_item_price = sum(item_price, na.rm = TRUE),
              s_type = sum(type))
  train_trade_t <- train_trade %>% 
    rename(acc_id = target_acc_id) %>%
    group_by(acc_id, day) %>%
    summarise(t_item_amount = sum(item_amount),
              t_item_price = sum(item_price, na.rm = TRUE),
              t_type = sum(type))
  
  train_trade_day_acc_id <- train_trade_s %>% full_join(train_trade_t)
  train_trade_day_acc_id <- train_trade_day_acc_id %>% left_join(dawn_source_id)
  train_trade_day_acc_id <- train_trade_day_acc_id %>% left_join(dawn_target_id)
  
  train_trad_final <- day_id %>% left_join(train_trade_day_acc_id)
  train_trad_final <- train_trad_final %>% 
    mutate(s_item_amount = ifelse(is.na(s_item_amount), 0, s_item_amount),
           s_item_price = ifelse(is.na(s_item_price), 0, s_item_price),
           s_type = ifelse(is.na(s_type), 0, s_type),
           t_item_amount = ifelse(is.na(t_item_amount), 0, t_item_amount),
           t_item_price = ifelse(is.na(t_item_price), 0, t_item_price),
           t_type = ifelse(is.na(t_type), 0, t_type),
           dawn = ifelse(is.na(dawn), 0, dawn))
  
  ###################################
  ############ Combat ###############
  ###################################
  #기준 acc_id에 메인 캐릭터 갖고 오기 
  day.acc_id <- train_activity[,c(1:2)]
  df <- train_activity %>% select(day, acc_id, char_id, playtime) %>% 
    group_by(day, acc_id) %>% filter(playtime == max(playtime)) %>% arrange(day, acc_id) 
  df$index <- ifelse(df$acc_id == lag(df$acc_id), 1, 0) 
  df <- df[-which(df$index == 1),]
  
  #특수 서버 접속 여부
  train_combat$server_index <- ifelse(train_combat$server %in% server_special, 1, 0)
  
  #day별 정리 
  train_combat_group <- train_combat %>% group_by(day, acc_id, char_id, class) %>% 
    summarize(max_level = max(level), 
              sum_level = sum(level),
              pledge_cnt = sum(pledge_cnt), 
              random_attacker_cnt = sum(random_attacker_cnt), 
              random_defender_cnt = sum(random_defender_cnt), 
              temp_cnt = sum(random_defender_cnt), 
              same_pledge_cnt = sum(same_pledge_cnt), 
              etc_cnt = sum(etc_cnt), 
              num_opponent = sum(num_opponent), 
              sever_index = sum(server_index))
  train_combat_ing <- df %>% select(day, acc_id, char_id) %>% 
    left_join(train_combat_group, by = c("day"="day", "acc_id"="acc_id", "char_id"="char_id")) 
  
  #캐릭터 개수 
  train_combat_chr_cnt <- train_combat %>% select(day, acc_id, char_id) %>% 
    group_by(day, acc_id) %>% summarize(chr_cnt = n())
  
  #최종
  train_combat_final <- train_combat_ing %>% 
    left_join(train_combat_chr_cnt, by = c("day"="day", "acc_id"="acc_id"))
  train_combat_final <- day_id %>% left_join(train_combat_final)
  
  ###################################
  ############ PLEDGE ###############
  ###################################
  #기준 acc_id에 pledge id 붙이기
  df <- train_activity %>% select(day, acc_id, char_id, playtime) %>% 
    group_by(day, acc_id) %>% filter(playtime == max(playtime)) %>% arrange(day, acc_id)
  df$index <- ifelse(df$acc_id == lag(df$acc_id), 1, 0) 
  df <- df[-which(df$index == 1),]
  
  # 혈맹 가입 playtime가 같아서 중복 나오는 행 index처리 & 삭제
  day.acc_id <- day_id %>% 
    left_join(df, by=c("day"="day", "acc_id"="acc_id")) %>% 
    select(day, acc_id, char_id)
  day.acc_id.pledge_id <- day.acc_id %>% 
    left_join(train_pledge, by = c("day"="day", "acc_id"="acc_id", "char_id"="char_id")) %>% 
    select(day, acc_id, pledge_id) %>% distinct() %>% arrange(day, acc_id)
  day.acc_id.pledge_id$index <- ifelse(day.acc_id.pledge_id$acc_id == lag(day.acc_id.pledge_id$acc_id), 1, 0) 
  day.acc_id.pledge_id <- day.acc_id.pledge_id[-which(day.acc_id.pledge_id$index == 1),]
  day.acc_id.pledge_id <- day.acc_id.pledge_id %>% select(day, acc_id, pledge_id)
  
  #pledge_id 기준 summary
  pledge_id <- train_pledge %>% 
    select(day, pledge_id, 
           play_char_cnt, combat_char_cnt, pledge_combat_cnt, random_attacker_cnt, random_defender_cnt,
           same_pledge_cnt, temp_cnt, etc_cnt, combat_play_time) %>% distinct()
  
  train_pledge_sum <- pledge_id %>% group_by(day, pledge_id) %>% 
    summarize(play_char_cnt = sum(play_char_cnt), 
              combat_char_cnt = sum(combat_char_cnt), 
              pledge_combat_cnt = sum(pledge_combat_cnt), 
              random_attacker_cnt = sum(random_attacker_cnt), 
              random_defender_cnt = sum(random_defender_cnt),
              same_pledge_cnt = sum(same_pledge_cnt), 
              temp_cnt = sum(temp_cnt), 
              etc_cnt = sum(etc_cnt), 
              combat_play_time = sum(combat_play_time))
  
  train_pledge_final <- day.acc_id.pledge_id %>% 
    left_join(train_pledge_sum, by=c("day"="day", "pledge_id"="pledge_id")) %>% select(-pledge_id)
  
  #######################################
  ######### table summary ###############
  #######################################
  #Activity Join 
  train_table <- day_id %>% left_join(train_activity_final) 
  
  #Trade Join 
  train_table <- train_table %>% left_join(train_trad_final, by = c("day"="day", "acc_id"="acc_id"))
  
  #Combat Join(.x붙은 변수들) 
  train_table <- train_table %>% left_join(train_combat_final,by = c("day"="day", "acc_id"="acc_id"))
  
  #Pledge  Join(.y붙은 변수들)
  train_table <- train_table %>% left_join(train_pledge_final, by = c("day"="day", "acc_id"="acc_id"), suffix = c('', '_pld'))
  
  #결제데이터 join
  train_table <- train_table %>% left_join(train_payment, by = c("day"="day", "acc_id"="acc_id"))
  
  #Week index 
  df <- train_table %>% select(day, acc_id) %>% arrange(acc_id,day)
  df$index <- ifelse(df$acc_id == lag(df$acc_id), 1, 0) 
  df <- df[-which(df$index == 1),]
  df$week_index <- ifelse(df$day < 8, 1, 
                          ifelse(df$day < 15, 2,
                                 ifelse(df$day < 22, 3, 4))) 
  df <- df %>% select(acc_id, week_index)
  train_table <- train_table %>% left_join(df, by = c("acc_id"))
  
  train_table <- train_table %>% mutate_all(function(x) ifelse(is.na(x), 0, x))
  
  return(train_table)
}

