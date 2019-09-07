setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
library(tidyverse)
library(survival)
library(randomForestSRC)

theme_set(
  theme_light() +
    theme(plot.title = element_text(hjust = 0.5)))

# Load data
xdata <- read_csv('preprocess/train_week.csv')
ydata <- read_csv('preprocess/train_label.csv')


# week 1 model ---------------------------------------------------------------------------------
data.w1 <- xdata %>% filter(first_week == 1) %>% select(-first_week) %>%
  left_join(select(ydata, acc_id, survival_time, churn))

set.seed(1)
tr1 <- sample(nrow(data.w1), size = nrow(data.w1)*0.8)

# COXPH ----------------------------------------------------------------------------------------
model.cox.w1 <- coxph(Surv(survival_time, churn) ~ ., data = data.w1[tr1, -1])
model.cox.w1 <- step(model.cox.w1, direction = 'both')
# quick
# model.cox.w1 <- coxph(
#   Surv(survival_time, churn) ~ n_day_w1 + n_day_w3 +
#   n_day_w4 + max_diff_day_w1 + max_diff_day_w2 + max_diff_day_w3 + 
#   max_diff_day_w4 + max_payment_w2 + max_payment_w3 + max_payment_w4 + 
#   mean_payment_w2 + mean_payment_w3 + n_payment_w3 + n_payment_w4 + 
#   risk_ratio_w1 + risk_ratio_w2 + char_cnt_act_w1 + char_cnt_act_w3 + 
#   char_cnt_act_w4 + playtime_w1 + playtime_w3 + playtime_w4 + 
#   total_exp_w1 + total_exp_w2 + total_exp_w3 + total_exp_w4 + 
#   party_exp_per_w1 + party_exp_per_w3 + fishing_w1 + private_shop_w1 + 
#   private_shop_w3 + private_shop_w4 + death_w1 + death_w2 + 
#   game_money_change_w3 + purchase_ex_w4 + purchase_pr_w2 + 
#   sum_level_w1 + sum_level_w4 + combat_cnt_w3 + combat_cnt_w4 + 
#   combat_play_time_pld_w1 + combat_play_time_pld_w4 + play_char_cnt_pld_w1 + 
#   play_char_cnt_pld_w4 + pledge_combat_cnt_pld_w1 + pledge_combat_cnt_pld_w4, 
#   data = data.w1[tr, -1]
# )
summary(model.cox.w1)

pred.cox.w1 <- survfit(model.cox.w1, newdata = data.w1[-tr1,])
pred.s.cox.w1 <- data.frame(t = pred.cox.w1$time, pred.cox.w1$surv)
pred.t.cox.w1 <- pred.s.cox.w1 %>% 

# ALT ------------------------------------------------------------------------------------------
model.alt.w1 <- survreg(Surv(survival_time, churn) ~ ., dist = 'weibull', data.w1[tr1, -1])
model.alt.w1 <- step(model.alt.w1, direction = 'both')
summary(model.alt.w1)
# quick
# model.alt.w1 <- survreg(
#   Surv(survival_time, churn) ~ n_day_w1 + n_day_w3 +
#   n_day_w4 + max_diff_day_w1 + max_diff_day_w2 + max_diff_day_w3 +
#   max_diff_day_w4 + max_payment_w2 + max_payment_w3 + max_payment_w4 +
#   mean_payment_w2 + mean_payment_w3 + n_payment_w3 + n_payment_w4 +
#   risk_ratio_w1 + risk_ratio_w2 + char_cnt_act_w1 + playtime_w1 +
#   playtime_w3 + playtime_w4 + total_exp_w1 + total_exp_w2 +
#   total_exp_w3 + total_exp_w4 + party_exp_per_w1 + party_exp_per_w2 +
#   party_exp_per_w3 + fishing_w1 + private_shop_w1 + private_shop_w3 +
#   private_shop_w4 + death_w1 + death_w2 + game_money_change_w3 +
#   purchase_ex_w4 + purchase_pr_w2 + sum_level_w1 + sum_level_w4 +
#   combat_cnt_w3 + combat_cnt_w4 + combat_play_time_pld_w1 +
#   combat_play_time_pld_w2 + combat_play_time_pld_w4 + play_char_cnt_pld_w1 +
#   play_char_cnt_pld_w2 + play_char_cnt_pld_w4 + pledge_combat_cnt_pld_w1 +
#   pledge_combat_cnt_pld_w2 + pledge_combat_cnt_pld_w4,
#   data = data.w1[tr, -1], dist = "weibull"
# )



# week 4 model ---------------------------------------------------------------------------------
data.w4 <- xdata %>% filter(first_week == 4) %>% select(acc_id, ends_with('w4')) %>%
  left_join(select(ydata, acc_id, survival_time, churn))

set.seed(4)
tr4 <- sample(nrow(data.w4), size = nrow(data.w4)*0.8)

# COXPH ----------------------------------------------------------------------------------------
model.cox.w4 <- coxph(Surv(survival_time, churn) ~ ., data = data.w4[tr4, -1])
model.cox.w4 <- step(model.cox.w4, direction = 'both')
summary(model.cox.w4)

pred.cox.w4 <- survfit(model.cox.w4, newdata = data.w4[-tr4,])
pred.s.cox.w4 <- data.frame(t = pred.cox.w4$time, pred.cox.w4$surv)

# ALT ------------------------------------------------------------------------------------------
model.alt.w4 <- survreg(Surv(survival_time, churn) ~ ., dist = 'weibull', data.w4[tr4, -1])
model.alt.w4 <- step(model.alt.w4, direction = 'both')
summary(model.alt.w4)

pred.alt.w4 <- predict(model.alt.w4, data.w4[-tr4, -1], type = 'quantile', p = 0.5)



# week 2-3 model -------------------------------------------------------------------------------
data.w23 <- xdata %>% filter(first_week %in% (2:3)) %>% select(-ends_with('w1')) %>%
  left_join(select(ydata, acc_id, survival_time, churn))

set.seed(23)
tr23 <- sample(nrow(data.w23), size = nrow(data.w23)*0.8)

# COXPH ----------------------------------------------------------------------------------------
model.cox.w23 <- coxph(Surv(survival_time, churn) ~ ., data = data.w23[tr23, -1])
model.cox.w23 <- step(model.cox.w23, direction = 'both')
summary(model.cox.w23)

pred.cox.w23 <- survfit(model.cox.w23, newdata = data.w23[-tr23,])
pred.s.cox.w23 <- data.frame(t = pred.cox.w23$time, pred.cox.w23$surv)
# pred.s.cox.w23 <- exp(-predict(model.cox.w23, newdata = data.w23[-tr23,], type = 'risk'))
summary(pred.s.cox.w23)
pred.t.cox.w23 <- pred.s.cox.w23 %>% gather(obs, s.prob, -t)
pred.t.cox.w23 <- pred.t.cox.w23 %>% group_by(obs) %>% filter(s.prob < 0.5)
pred.t.cox.w23 <- pred.t.cox.w23 %>% group_by(obs) %>% summarize(t = first(t))

# ALT ------------------------------------------------------------------------------------------
model.alt.w23 <- survreg(Surv(survival_time, churn) ~ ., dist = 'weibull', data.w23[tr23, -1])
model.alt.w23 <- step(model.alt.w23, direction = 'both')
summary(model.alt.w23)

pred.alt.w23 <- predict(model.alt.w23, data.w23[-tr23, -1], type = 'quantile', p = 0.5)

