setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
library(survival)
library(tidyverse)

theme_set(theme_light() + 
            theme(plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5)))
    
train_label <- read_csv('preprocess/train_label.csv')

# Kaplan-Meier Curve -----------------------------------------------------------------------------------
KMfit <- survfit(Surv(survival_time, churn) ~ 1, train_label)
ggplot() + 
  geom_step(aes(KMfit$time, KMfit$surv), size = 1) +
  labs(x = 't', y = 'S(t)', title = 'Kaplan-Meier Curve')
KMtable <- data.frame(survival_time = KMfit$time, survival_prob = KMfit$surv)
train_label_surv <- train_label %>% 
  left_join(KMtable) %>% 
  select(acc_id, first_week, survival_time, churn, survival_prob, amount_spent, payment)
write.csv(train_label_surv, 'model/train_label_surv.csv', row.names = F)

# Weibull Q-Q Plot -> ALT
ggplot() + 
  geom_line(aes(log(KMfit$time), log(-log(KMfit$surv))), size = 1) +
  labs(x = 't', y = 'S(t)', title = 'Kaplan-Meier Curve')

# Cumulative Hazard
ggplot() + 
  geom_step(aes(KMfit$time, KMfit$cumhaz), size = 1) +
  labs(x = 't', y = 'H(t)', title = 'Kaplan-Meier Curve')


# With Predictors ( EDA ) ------------------------------------------------------------------------------
xdata <- read_csv('preprocess/train_day.csv')
colnames(xdata)

# Quick Function
KMplot.groups <- function(gindex, var) {
  surv_high <- survfit(Surv(survival_time, churn) ~ 1, data = train_label[gindex,])
  surv_low <- survfit(Surv(survival_time, churn) ~ 1, data = train_label[-gindex,])
  surv_table <- data.frame(t = surv_high$time, St = surv_high$surv, group = 'high') %>%
    full_join(data.frame(t = surv_low$time, St = surv_low$surv, group = 'low'))
  
  ggplot(surv_table) + geom_step(aes(t, St, color = group), size = 1) +
    scale_color_manual(var, values = c('low' = 'darkblue', 'high' = 'red')) +
    ggtitle('Kaplan-Meier Curve by Variable') + 
    theme(legend.position = 'top')
}

# Plots
# risk_ratio
gindex <- xdata %>% 
  filter(risk_ratio > median(risk_ratio)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'risk_ratio')

# amount_spent
gindex <- xdata %>% 
  filter(amount_spent > median(amount_spent)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'amount_spent')

# playtime
gindex <- xdata %>% 
  filter(playtime > median(playtime)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'playtime')

# total_exp
gindex <- xdata %>% 
  filter(total_exp > median(total_exp)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'total_exp')

# party_exp_per
gindex <- xdata %>% 
  filter(party_exp_per > median(party_exp_per)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'party_exp_per')

# death
gindex <- xdata %>% 
  filter(death > median(death)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'death')

# fishing
gindex <- xdata %>% 
  filter(fishing > median(fishing)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'fishing')

# private_shop
gindex <- xdata %>% 
  filter(private_shop > median(private_shop)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'private_shop')

# enchant_count
gindex <- xdata %>% 
  filter(enchant_count > median(enchant_count)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'enchant_count')

# purchase_ex
gindex <- xdata %>% 
  filter(purchase_ex > median(purchase_ex)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'purchase_ex')

# purchase_pr
gindex <- xdata %>% 
  filter(purchase_pr > median(purchase_pr)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'purchase_pr')

# sum_level
gindex <- xdata %>% 
  filter(sum_level > median(sum_level)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'sum_level')

# combat_cnt
gindex <- xdata %>% 
  filter(combat_cnt > median(combat_cnt)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'combat_cnt')

# num_opponent
gindex <- xdata %>% 
  filter(num_opponent > median(num_opponent)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'num_opponent')

# play_char_cnt_pld
gindex <- xdata %>% 
  filter(play_char_cnt_pld > median(play_char_cnt_pld)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'play_char_cnt_pld')

# pledge_combat_cnt_pld
gindex <- xdata %>% 
  filter(pledge_combat_cnt_pld > median(pledge_combat_cnt_pld)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'pledge_combat_cnt_pld')

# combat_play_time_pld
gindex <- xdata %>% 
  filter(combat_play_time_pld > median(combat_play_time_pld)) %>% select(acc_id) %>% distinct()
gindex <- which(train_label$acc_id %in% gindex$acc_id)
KMplot.groups(gindex, 'combat_play_time_pld')

