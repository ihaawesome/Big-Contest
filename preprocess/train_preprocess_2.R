library(tidyverse)
library(lubridate)
library(glue)

train_week <- read_csv('preprocess/train_week.csv')
# Label Processing -----------------------------------------------------------------------------------------
train_label <- read_csv('raw/train_label.csv') %>% arrange(acc_id) %>%
  mutate(churn = ifelse(survival_time < 64, 1, 0), payment = ifelse(amount_spent > 0, 1, 0)) %>%
  left_join(select(train_week, acc_id, first_week))
write.csv(train_label, 'preprocess/train_label.csv', row.names = F)

# Padding (timesteps = 28) --------------------------------------------------------------------------------- 
make.sequence <- function(set = 'train', save = TRUE) {
  train_day <- read_csv(glue('preprocess/{set}_day.csv'))
  first_week <- train_day %>% select(acc_id, first_week) %>% distinct()
  train_day <- train_day %>% select(-first_week)
  train_sequence <- expand.grid(day = (1:28), acc_id = sort(unique(train_day$acc_id))) %>% 
    left_join(train_day) %>%
    mutate_all(function(x) ifelse(is.na(x), 0, x))
  train_sequence <- train_sequence %>% left_join(first_week)
  if (save) write.csv(train_sequence, glue('preprocess/{set}_sequence.csv'), row.names = FALSE) 
  return(train_sequence)
}

train_sequence <- make.sequence('train')
test1_sequence <- make.sequence('test1')
test2_sequence <- make.sequence('test2')

train_sequence <- read_csv('preprocess/train_sequence.csv')

