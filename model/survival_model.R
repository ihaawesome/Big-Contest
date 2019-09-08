setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
library(tidyverse)
library(survival)
library(randomForestSRC)

theme_set(
  theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)))

# Load data
xdata <- read_csv('preprocess/train_week.csv')
ydata <- read_csv('preprocess/train_label.csv')


# Kaplan-Meier
km.fit <- survfit(Surv(survival_time, churn) ~ 1, ydata)
pred.s.km <- data.frame(t= km.fit$time, s.prob = km.fit$surv, H = km.fit$cumhaz)


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

# RSF --------------------------------------------------------------------------------------------------
p = ncol(data.w1) - 3
rf.grid1 <- expand.grid(ntree = c(500, 1000),
                        mtry = c(sqrt(p), p),
                        nodesize = c(5, 15),
                        OOBerror = 0)
for (i in 1:nrow(rf.grid1)) {
  m <- rfsrc(Surv(survival_time, churn) ~ ., data.w1[tr1, -1], importance = TRUE,
             ntree = rf.grid1$ntree[i], mtry = rf.grid1$mtry[i], nodesize = rf.grid1$nodesize[i])
  rf.grid1$OOBerror[i] <- last(m$err.rate)
}
rf.grid1 <- rf.grid1 %>% arrange(OOBerror)
model.rsf.w1 <- rfsrc(Surv(survival_time, churn) ~ ., data.w1[tr1, -1], importance = TRUE,
                      ntree = rf.grid1$ntree[1], mtry = rf.grid1$mtry[1], nodesize = rf.grid1$nodesize[1])

pred.rsf.w1 <- predict(model.rsf.w1, data.w1[-tr1,])
pred.s.rsf.w1 <- data.frame(t = pred.rsf.w1$time.interest, t(pred.rsf.w1$survival))

# variable importance
data.frame(variable = names(model.rsf.w1$importance), 
           importance = model.rsf.w1$importance)[1:10,] %>%
  ggplot() + geom_col(aes(fct_reorder(variable, importance), importance), fill = 'Dark Slate Blue') + 
  labs(x = 'variable') + coord_flip()


# Summary 1 --------------------------------------------------------------------------------------------
km.fit.w1 <- survfit(Surv(survival_time, churn) ~ 1, data.w1[-tr1, -1])
pred.s.km.w1 <- data.frame(t= km.fit.w1$time, s.prob = km.fit.w1$surv, H = km.fit.w1$cumhaz)

ggplot() + geom_step(aes(pred.s.km.w1$t, pred.s.km.w1$s.prob, color = 'K-M')) +
  geom_step(aes(pred.s.cox.w1$t, rowMeans(pred.s.cox.w1[,-1]), color = 'COXPH')) +
  geom_step(aes(pred.rsf.w1$t, rowMeans(pred.s.rsf.w1[,-1]), color = 'RSF')) +
  scale_color_manual('', values = c('K-M' = 'black', 'COXPH' = 'red', 'RSF' = 'blue')) +
  labs(x = 'time', y = 'survival.prob', title = 'Week 4: Predicted Survival Probabilities') +
  theme(legend.position = 'top')



# ------------------------------------------------------------------------------------------------------
# week 4 model -----------------------------------------------------------------------------------------
data.w4 <- xdata %>% filter(first_week == 4) %>% select(acc_id, ends_with('w4')) %>%
  left_join(select(ydata, acc_id, survival_time, churn))

set.seed(4)
tr4 <- sample(nrow(data.w4), size = nrow(data.w4)*0.8)

# COXPH ------------------------------------------------------------------------------------------------
model.cox.w4 <- coxph(Surv(survival_time, churn) ~ ., data = data.w4[tr4, -1])
model.cox.w4 <- step(model.cox.w4, direction = 'both')
summary(model.cox.w4)

pred.cox.w4 <- survfit(model.cox.w4, newdata = data.w4[-tr4,])
pred.s.cox.w4 <- data.frame(t = pred.cox.w4$time, pred.cox.w4$surv)


# RSF --------------------------------------------------------------------------------------------------
p = ncol(data.w4) - 3
rf.grid4 <- expand.grid(ntree = c(500, 1000),
                        mtry = c(sqrt(p), p),
                        nodesize = c(5, 15),
                        OOBerror = 0)
for (i in 1:nrow(rf.grid4)) {
  m <- rfsrc(Surv(survival_time, churn) ~ ., data.w4[tr4, -1], importance = TRUE,
             ntree = rf.grid4$ntree[i], mtry = rf.grid4$mtry[i], nodesize = rf.grid4$nodesize[i])
  rf.grid4$OOBerror[i] <- last(m$err.rate)
}
rf.grid4 <- rf.grid4 %>% arrange(OOBerror)
model.rsf.w4 <- rfsrc(Surv(survival_time, churn) ~ ., data.w4[tr4, -1], importance = TRUE,
                      ntree = rf.grid4$ntree[1], mtry = rf.grid4$mtry[1], nodesize = rf.grid4$nodesize[1])

pred.rsf.w4 <- predict(model.rsf.w4, data.w4[-tr4,])
pred.s.rsf.w4 <- data.frame(t = pred.rsf.w4$time.interest, t(pred.rsf.w4$survival))

# variable importance
data.frame(variable = names(model.rsf.w4$importance), 
           importance = model.rsf.w4$importance)[1:10,] %>%
  ggplot() + geom_col(aes(fct_reorder(variable, importance), importance), fill = 'Dark Slate Blue') + 
  labs(x = 'variable') + coord_flip()


# Summary 4 --------------------------------------------------------------------------------------------
km.fit.w4 <- survfit(Surv(survival_time, churn) ~ 1, data.w4[-tr4, -1])
pred.s.km.w4 <- data.frame(t= km.fit.w4$time, s.prob = km.fit.w4$surv, H = km.fit.w4$cumhaz)

ggplot() + geom_step(aes(pred.s.km.w4$t, pred.s.km.w4$s.prob, color = 'K-M')) +
  geom_step(aes(pred.s.cox.w4$t, rowMeans(pred.s.cox.w4[,-1]), color = 'COXPH')) +
  geom_step(aes(pred.rsf.w4$t, rowMeans(pred.s.rsf.w4[,-1]), color = 'RSF')) +
  scale_color_manual('', values = c('K-M' = 'black', 'COXPH' = 'red', 'RSF' = 'blue')) +
  labs(x = 'time', y = 'survival.prob', title = 'Week 4: Predicted Survival Probabilities') +
  theme(legend.position = 'top')


# ------------------------------------------------------------------------------------------------------
# week 2-3 model ---------------------------------------------------------------------------------------
data.w23 <- xdata %>% filter(first_week %in% (2:3)) %>% select(-ends_with('w1')) %>%
  left_join(select(ydata, acc_id, survival_time, churn))

set.seed(23)
tr23 <- sample(nrow(data.w23), size = nrow(data.w23)*0.8)

# COXPH ------------------------------------------------------------------------------------------------
model.cox.w23 <- coxph(Surv(survival_time, churn) ~ ., data = data.w23[tr23, -1])
model.cox.w23 <- step(model.cox.w23, direction = 'both')
summary(model.cox.w23)

pred.cox.w23 <- survfit(model.cox.w23, newdata = data.w23[-tr23,])
pred.s.cox.w23 <- data.frame(t = pred.cox.w23$time, pred.cox.w23$surv)


# RSF  ----------------------------------------------------------------------------------------------------------
p = ncol(data.w23) - 3
rf.grid23 <- expand.grid(ntree = c(500, 1000),
                         mtry = c(sqrt(p), p),
                         nodesize = c(5, 15),
                         OOBerror = 0)
for (i in 1:nrow(rf.grid23)) {
  m <- rfsrc(Surv(survival_time, churn) ~ ., data.w23[tr23, -1], importance = TRUE,
             ntree = rf.grid23$ntree[i], mtry = rf.grid23$mtry[i], nodesize = rf.grid23$nodesize[i])
  rf.grid23$OOBerror[i] <- last(m$err.rate)
}
rf.grid23 <- rf.grid23 %>% arrange(OOBerror)
model.rsf.w23 <- rfsrc(Surv(survival_time, churn) ~ ., data.w23[tr23, -1], importance = TRUE,
                       ntree = rf.grid23$ntree[1], mtry = rf.grid23$mtry[1], nodesize = rf.grid23$nodesize[1])
# variable importance
data.frame(variable = names(model.rsf.w23$importance), 
           importance = model.rsf.w23$importance)[1:10,] %>%
  ggplot() + geom_col(aes(fct_reorder(variable, importance), importance)) + 
  labs(x = 'variable') + coord_flip()


pred.rsf.w23 <- predict(model.rsf.w23, data.w23[-tr23,])
pred.s.rsf.w23 <- data.frame(t = pred.rsf.w23$time.interest, t(pred.rsf.w23$survival))


# Summary 23 ---------------------------------------------------------------------------------------------------
km.fit.w23 <- survfit(Surv(survival_time, churn) ~ 1, data.w23[-tr23, -1])
pred.s.km.w23 <- data.frame(t= km.fit.w23$time, s.prob = km.fit.w23$surv, H = km.fit.w23$cumhaz)

ggplot() + geom_step(aes(pred.s.km.w23$t, pred.s.km.w23$s.prob, color = 'K-M')) +
  geom_step(aes(pred.s.cox.w23$t, rowMeans(pred.s.cox.w23[,-1]), color = 'COXPH')) +
  geom_step(aes(pred.rsf.w23$t, rowMeans(pred.s.rsf.w23[,-1]), color = 'RSF')) +
  scale_color_manual('', values = c('K-M' = 'black', 'COXPH' = 'red', 'RSF' = 'blue')) +
  labs(x = 'time', y = 'survival.prob', title = 'Week 2-3: Predicted Survival Probabilites') +
  theme(legend.position = 'top')




# 생존확률 기준 찾기
threshold <- function(pred.s, s) {
  pred.t <- as.data.frame(pred.s) %>% gather(obs, s.prob, -t) 
  obs <- pred.t %>% select(obs) %>% distinct()
  pred.t <- pred.t %>%
    group_by(obs) %>% filter(s.prob < s) %>% summarize(pred.t = first(t)) %>%
    mutate(churn = ifelse(pred.t < 64, 1, 0))
  pred.t <- obs %>% left_join(pred.t)
  pred.t <- pred.t %>% 
    mutate(pred.t = ifelse(is.na(pred.t), 64, pred.t), churn = ifelse(is.na(churn), 0, churn))
  return(pred.t)
}

threshold.curve <- function(data, tr, pred.s) {
  s <- seq(0.01, 0.99, by = 0.01)
  ch <- matrix(0, nrow = length(s), ncol = nrow(data) - length(tr))
  acc <- c()
  for (i in 1:length(s)) {
    ch[i,] <- threshold(pred.s, s[i])$churn
    acc[i] <- sum(ch[i,] == data$churn[-tr]) / length(ch[i,])
  }
  ggplot() + geom_step(aes(s, acc), color = 'dark slate blue', size = 1) + 
    geom_vline(aes(xintercept = s[which.max(acc)]), linetype = 2) +
    labs(x = 'survival.prob', y = 'accuracy', 
         title = glue('{round(max(acc), 3)*100}%'),
         subtitle = glue('with prob = {s[which.max(acc)]}'))
}

threshold.curve(data.w1, tr1, pred.s.cox.w1)
threshold.curve(data.w23, tr23, pred.s.cox.w23)
threshold.curve(data.w4, tr4, pred.s.cox.w4)







# ALT (망함) ----------------------------------------------------------------------------------------------
# Week 1 --------------------------------------------------------------------------------------------------
# model.alt.w1 <- survreg(Surv(survival_time, churn) ~ ., dist = 'weibull', data.w1[tr1, -1])
# model.alt.w1 <- step(model.alt.w1, direction = 'both')
# summary(model.alt.w1)
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
# pred.alt.w1 <- exp(predict(model.alt.w1, type = 'lp'))

# Week 23 -------------------------------------------------------------------------------------------------
# model.alt.w23 <- survreg(Surv(survival_time, churn) ~ ., dist = 'weibull', data.w23[tr23, -1])
# model.alt.w23 <- step(model.alt.w23, direction = 'both')
# summary(model.alt.w23)
# 
# pred.alt.w23 <- predict(model.alt.w23, data.w23[-tr23, -1], type = 'quantile', p = 0.5)

# Week 4 --------------------------------------------------------------------------------------------------
# model.alt.w4 <- survreg(Surv(survival_time, churn) ~ ., dist = 'weibull', data.w4[tr4, -1])
# model.alt.w4 <- step(model.alt.w4, direction = 'both')
# summary(model.alt.w4)
# pred.alt.w4 <- predict(model.alt.w4, data.w4[-tr4, -1], type = 'quantile', p = 0.5)


