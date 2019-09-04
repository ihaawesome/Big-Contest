setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
options(scipen = 100)
library(tidyverse)
library(lubridate)
library(glue)
library(reshape2)

make.data <- function(set = 'train', save = FALSE) {
   
  train_activity <- read_csv(glue('raw/{set}_activity.csv'))
  train_combat <- read_csv(glue('raw/{set}_combat.csv'))
  train_payment <- read_csv(glue('raw/{set}_payment.csv'))
  train_pledge <- read_csv(glue('raw/{set}_pledge.csv'))
  train_trade <- read_csv(glue('raw/{set}_trade.csv'))
  
  ################################
  ########## Processing ##########
  ################################
  
  # ID collection --------------------------------------------------------------------------------------
  activity_day_id <- train_activity %>% select(day, acc_id) %>% distinct()
  payment_day_id <- train_payment %>% select(day, acc_id) %>% distinct()
  day_id <- activity_day_id %>% full_join(payment_day_id) %>% distinct() %>% arrange(acc_id, day)
  
  rm(activity_day_id, payment_day_id)
  
  # Activity -------------------------------------------------------------------------------------------
  # Replace Outliers
  activity_quantile <- train_activity %>% 
    select(-day, -acc_id, -char_id, -server, -rich_monster) %>%
    apply(2, function(x) quantile(x, probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)))
  train_activity_1 <- train_activity %>%
    select(-exp_recovery) %>%
    mutate(npc_kill = pmax(activity_quantile['1%','npc_kill'], 
                           pmin(npc_kill, activity_quantile['99%','npc_kill'])),
           solo_exp = pmax(activity_quantile['1%','solo_exp'], 
                           pmin(solo_exp, activity_quantile['99%','solo_exp'])),
           party_exp = pmax(activity_quantile['1%','party_exp'], 
                            pmin(party_exp, activity_quantile['99%','party_exp'])),
           quest_exp = pmax(activity_quantile['1%','quest_exp'], 
                            pmin(quest_exp, activity_quantile['99%','quest_exp'])),
           death = pmax(activity_quantile['1%','death'], 
                        pmin(death, activity_quantile['99%','death'])),
           enchant_count = ifelse(enchant_count > 0, 1, 0)
           )
  
  min_unit <- apply(train_activity[,-(1:4)], 2, function(x) min(x[x > 0]))  
  server_special <- c("bj", "bk", "bl", "bm", "bn" ,"bo")
  
  train_activity_1 <- train_activity_1 %>%   
    mutate(total_exp = solo_exp / min_unit['solo_exp'] + 
             party_exp / min_unit['party_exp'] + 
             quest_exp / min_unit['quest_exp']) %>%
    mutate(solo_exp_per = (solo_exp / min_unit['solo_exp']) / (total_exp + 1e-8), 
           party_exp_per = (party_exp / min_unit['party_exp']) / (total_exp + 1e-8)) %>%
    mutate(total_exp = total_exp / sd(total_exp), revive_per = revive / death) 
  
  train_activity_2 <- train_activity_1 %>% group_by(day, acc_id) %>% 
    summarize(char_cnt_act = n(),
              playtime = sum(playtime),
              total_exp = sum(total_exp), 
              party_exp_per = mean(party_exp_per), 
              fishing = sum(fishing),
              private_shop = sum(private_shop), 
              death = sum(death),
              game_money_change = sum(game_money_change),
              enchant_count = sum(enchant_count)) 
  
  train_activity_final <- day_id %>% left_join(train_activity_2)
  
  rm(train_activity_1, train_activity_2)
  
  # Trade ----------------------------------------------------------------------------------------------
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
  
  # Combat ---------------------------------------------------------------------------------------------
  # Replace Outliers
  combat_quantile <- train_combat %>% 
    select(-day, -acc_id, -char_id, -server, -class, -level) %>%
    apply(2, function(x) quantile(x, probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)))
  train_combat_1 <- train_combat %>%
    select(-same_pledge_cnt, -random_attacker_cnt) %>%
    mutate(pledge_cnt = pmax(combat_quantile['1%','pledge_cnt'], 
                             pmin(pledge_cnt, 
                                  combat_quantile['99%','pledge_cnt'])),
           random_defender_cnt = pmax(combat_quantile['1%','random_defender_cnt'], 
                                      pmin(random_defender_cnt, 
                                           combat_quantile['99%','random_defender_cnt'])),
           temp_cnt = pmax(combat_quantile['1%','temp_cnt'], 
                           pmin(temp_cnt, 
                                combat_quantile['99%','temp_cnt'])),
           etc_cnt = pmax(combat_quantile['1%','etc_cnt'], 
                          pmin(etc_cnt, 
                               combat_quantile['99%','etc_cnt'])),
           num_opponent = pmax(combat_quantile['1%','num_opponent'], 
                               pmin(num_opponent, 
                                    combat_quantile['99%','num_opponent']))
    )
  
  min_unit_combat <- apply(train_combat[,(7:13)], 2, function(x) min(x[x > 0]))  
  train_combat_1 <- train_combat_1 %>% 
    mutate(combat_cnt = 
             temp_cnt / min_unit_combat['temp_cnt'] + 
             etc_cnt / min_unit_combat['etc_cnt'] + 
             pledge_cnt / min_unit_combat['pledge_cnt']) %>%
    mutate(combat_cnt = combat_cnt / sd(combat_cnt)) %>%
    group_by(day, acc_id) %>% 
    summarize(sum_level = sum(level),
              combat_cnt = sum(combat_cnt),
              num_opponent = sum(num_opponent)) 
  
  train_combat_final <- day_id %>% left_join(train_combat_1) 
  
  rm(train_combat_1)
  
  # Pledge ---------------------------------------------------------------------------------------------
  # 활동 기준 메인 캐릭터
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
  
  # ALL Join  ------------------------------------------------------------------------------------------
  train_table <- day_id %>% left_join(train_activity_final) 
  train_table <- train_table %>% left_join(train_payment, by = c("day", "acc_id"))
  train_table <- train_table %>% left_join(train_trade_final, by = c("day", "acc_id"))
  train_table <- train_table %>% left_join(train_combat_final, by = c("day", "acc_id"))
  train_table <- train_table %>% left_join(train_pledge_final, by = c("day", "acc_id"))
  
  train_table <- train_table %>% mutate_all(function(x) ifelse(is.na(x), 0, x))
  apply(train_table, 2, sd)
  
  # Scaling
  train_table[,-(1:2)] <- scale(train_table[,-(1:2)], center = FALSE)
  
  train_table <- train_table %>% 
    mutate(week = ifelse(day < 8, 1, ifelse(day < 15, 2, ifelse(day < 22, 3, 4))))
  
  # Risk Ratio -----------------------------------------------------------------------------------------
  day_all <- train_table %>% select(acc_id, day, week) %>% arrange()
  day_all <- day_all %>% group_by(acc_id) %>% 
    mutate(day_diff = c(0, diff(day)))
  day_all <- day_all %>% group_by(acc_id) %>% 
    mutate(risk_ratio = day_diff / mean(day_diff, na.rm = T)) %>%
    mutate(risk_ratio = ifelse(is.nan(risk_ratio), 0, risk_ratio))
  
  train_table <- select(day_all, -day_diff) %>% left_join(train_table)
  
  # First Week index -----------------------------------------------------------------------------------
  first_week <- ungroup(train_table) %>% select(day, acc_id) %>% arrange(acc_id, day) 
  first_week <- first_week %>% mutate(index = ifelse(acc_id == lag(acc_id), 1, 0))
  first_week <- first_week[-which(first_week$index == 1),]
  first_week <- first_week %>% 
    transmute(acc_id, 
              first_week = ifelse(day < 8, 1, ifelse(day < 15, 2, ifelse(day < 22, 3, 4))))
  
  # CSV File
  if (save) { 
    train_table <- train_table %>% left_join(first_week)
    write.csv(train_table, glue('preprocess/{set}_day.csv'), row.names = FALSE)
    train_table <- train_table %>% select(-first_week)
  }
  
  # Summary by week ------------------------------------------------------------------------------------
  # Login
  train_week_1 <- train_table %>%
    group_by(acc_id) %>%
    mutate(diff_day = c(0, diff(day))) %>%
    group_by(acc_id, week) %>% 
    summarize(n_day = n(), max_diff_day = max(diff_day))

  # Payment 
  train_week_2 <- train_table %>%
    group_by(acc_id, week) %>%
    summarize(n_payment = n(), max_payment = max(amount_spent))
  
  # Total
  train_week_3 <- train_table %>% group_by(acc_id, week) %>% 
    summarize_all(mean) %>% select(-day) %>% rename(mean_payment = amount_spent)
  
  train_week <- train_week_1 %>% left_join(train_week_2) %>% left_join(train_week_3)
  if (set = 'train') write.csv(cor(train_week[,-(1:2)]), 'etc/correlation.csv')
  
  train_week <- train_week %>% melt(id = c('acc_id', 'week'))
  train_week <- train_week %>% 
    mutate(var_w = str_c(variable, week, sep = '_w')) %>%
    select(-week, -variable)
  train_week <- train_week %>% spread(var_w, value)
  
  rm(train_week_1, train_week_2, train_week_3)
  
  # ALL Summary ----------------------------------------------------------------------------------------
  train_final <- train_week %>% left_join(first_week)
  train_final <- train_final %>%
    select(acc_id, first_week, starts_with('n_day'), starts_with('max_diff_day'), 
           contains('_payment_'), starts_with('risk'), 
           starts_with('char_cnt'), starts_with('playtime'), starts_with('total_exp'), 
           starts_with('party_exp'), starts_with('fishing'), starts_with('private_shop'), 
           starts_with('death'), starts_with('game_money'), starts_with('enchant'), 
           starts_with('purchase'), starts_with('sum_level'), starts_with('combat_cnt'), 
           starts_with('num_component'), contains('_pld_'))
  train_final <- train_final %>%
    mutate(risk_ratio_w2 = ifelse(is.na(risk_ratio_w2), risk_ratio_w1, risk_ratio_w2),
           risk_ratio_w3 = ifelse(is.na(risk_ratio_w3), risk_ratio_w2, risk_ratio_w3),
           risk_ratio_w4 = ifelse(is.na(risk_ratio_w4), risk_ratio_w3, risk_ratio_w4))
  
  train_final <- train_final %>% mutate_all(function(x) ifelse(is.na(x), 0, x))
  
  if (save) write.csv(train_final, glue('preprocess/{set}_week.csv'), row.names = FALSE)
  return(train_final)
}


# Create datasets
train_week <- make.data('train', save = TRUE)
test1_week <- make.data('test1', save = TRUE)
test2_week <- make.data('test2', save = TRUE)

