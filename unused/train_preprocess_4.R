library(tidyverse)
library(lubridate)
setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')

train_activity <- read_csv('preprocess/train_activity_preprocess_1.csv')
train_combat <- read_csv('raw/train_combat.csv')
train_label <- read_csv('raw/train_label.csv')
train_payment <- read_csv('raw/train_payment.csv')
train_pledge <- read_csv('raw/train_pledge.csv')
train_trade <- read_csv('raw/train_trade.csv')

theme_set(theme_light() + theme(panel.grid = element_blank()))


# acc_id, day 기준으로 28일치 변수 생성하기
# activity ----------------------------------------------------------------------------------------------
acc_activity <- train_activity %>%
  select(-server, -char_id, -char_day) %>%
  group_by(acc_id, day, acc_day) %>% summarise_all(sum)

# combat ------------------------------------------------------------------------------------------------
acc_combat <- train_combat %>%
  select(-server, -char_id, -level, -class) %>% 
  group_by(acc_id, day) %>% summarise_all(sum)

# trade -------------------------------------------------------------------------------------------------
train_trade <- train_trade %>% mutate(hour = hour(time))

train_trade %>% count(hour) %>%
  mutate(dawn = (between(hour, 4, 8))) %>%
  ggplot() + geom_col(aes(hour, n, fill = dawn)) +
  scale_fill_viridis_d()

dawn_target_id <- train_trade %>% filter(between(hour, 4, 8)) %>% 
  select(target_acc_id, day) %>% distinct() %>%
  arrange(target_acc_id, day) %>% 
  transmute(acc_id = target_acc_id, day, dawn = 1)
dawn_source_id <- train_trade %>% filter(between(hour, 4, 8)) %>% 
  select(source_acc_id, day) %>% distinct() %>%
  arrange(source_acc_id, day) %>% 
  transmute(acc_id = source_acc_id, day, dawn = 1)

# (graphs)
train_label %>%
  mutate(dawn = (acc_id %in% dawn_source_id$acc_id)) %>%
  ggplot() + 
  geom_density(aes(survival_time, fill = dawn), size = 1, alpha = 0.5) +
  scale_fill_manual(values = c('FALSE' = 'white', 'TRUE' = 'pink')) +
  theme(legend.position = 'top')

train_label %>%
  mutate(dawn = (acc_id %in% dawn_source_id$acc_id)) %>%
  ggplot() + 
  geom_bar(aes(survival_time, fill = dawn), size = 1, alpha = 0.5, position = 'fill') +
  scale_fill_manual(values = c('FALSE' = 'blue', 'TRUE' = 'pink')) +
  theme(legend.position = 'top')

acc_trade <- train_trade %>% 
  select(day, hour, acc_id = source_acc_id, item_amount) %>%
  group_by(acc_id, day) %>%
  summarise(trade_source_amt = sum(item_amount))
acc_trade2 <- train_trade %>% 
  select(day, hour, acc_id = target_acc_id, item_amount) %>%
  group_by(acc_id, day) %>%
  summarise(trade_target_amt = sum(item_amount))

acc_trade <- acc_trade %>% full_join(acc_trade2)
rm(acc_trade2)

acc_trade <- acc_trade %>% left_join(dawn_source_id)
acc_trade <- acc_trade %>% left_join(dawn_target_id)

acc_trade <- acc_trade %>% 
  mutate(trade_source_amt = ifelse(is.na(trade_source_amt), 0, trade_source_amt),
         trade_target_amt = ifelse(is.na(trade_target_amt), 0, trade_target_amt),
         dawn = ifelse(is.na(dawn), 0, dawn))

# payment -----------------------------------------------------------------------------------------------
acc_payment <- train_payment %>% arrange(acc_id, day)

# label -------------------------------------------------------------------------------------------------
train_label <- train_label %>% arrange(acc_id) %>% mutate(survived = as.numeric(survival_time == 64))
# write.csv(train_label, 'preprocess/train_label_preprocess_1.csv', row.names = F)


# JOIN --------------------------------------------------------------------------------------------------
acc_data <- expand.grid(acc_id = train_label$acc_id, day = c(1:28)) %>% arrange(acc_id, day)
acc_data <- acc_data %>% left_join(acc_activity)
acc_data <- acc_data %>% left_join(acc_combat)
acc_data <- acc_data %>% left_join(acc_trade)
acc_data <- acc_data %>% left_join(acc_payment)

# acc_data %>%
#   gather(variable, value, -acc_id, -day, -acc_day) %>% na.omit() %>% 
#   ggplot() + 
#   geom_boxplot(aes(y = value, group = variable)) + facet_wrap(~variable, scales = 'free')

acc_data <- acc_data %>% left_join(select(train_label, -amount_spent))
ggplot(acc_data) + geom_boxplot(aes(y = playtime, fill = survived)) + 
  scale_fill_brewer(palette = 'Pastel1')
ggplot(acc_data) + geom_boxplot(aes(y = (private_shop), fill = survived)) + 
  scale_fill_brewer(palette = 'Pastel1')

for (i in 4:27) { acc_data[,i] <- ifelse(is.na(acc_data[,i]), 0, acc_data[,i]) }
write.csv(acc_data, 'preprocess/train_preprocess_sequence.csv', row.names = F)



# -------------------------------------------------------------------------------------------------------
# train_activity_unit <- sapply(train_activity[,5:16], function(x) round(x / min(x[x > 0])))
# train_activity_unit <- as.data.frame(train_activity_unit) %>% 
#   mutate(fishing = fishing / 60, private_shop = private_shop / 60)
# 
# View(train_activity_unit %>% filter(playtime < fishing) %>% arrange(playtime, fishing))
# View(train_activity %>% filter(playtime < fishing) %>% arrange(playtime, fishing))
# train_activity %>% filter(playtime == 0) %>% .$private_shop %>% summary
# 
# servers <- train_activity %>% count(server)
# ggplot(servers) + geom_col(aes(server, n))
