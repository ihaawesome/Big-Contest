library(tidyverse)
library(survival)
library(randomForestSRC)
library(glue)
library(MLmetrics)

RMSE <- function(pred, true) sqrt(mean((pred - true)**2))

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

summary(model.cox.w1)
cox.final.w1 <- model.cox.w1$formula

pred.cox.w1 <- survfit(model.cox.w1, newdata = data.w1[-tr1, -1])
pred.s.cox.w1 <- data.frame(t = pred.cox.w1$time, pred.cox.w1$surv)

cind.cox.w1 <- 1 - concordance(model.cox.w1)$concordance

# RSF --------------------------------------------------------------------------------------------------
# X


# Summary 1 --------------------------------------------------------------------------------------------
km.fit.w1 <- survfit(Surv(survival_time, churn) ~ 1, data.w1[-tr1, -1])
pred.s.km.w1 <- data.frame(t= km.fit.w1$time, s.prob = km.fit.w1$surv, H = km.fit.w1$cumhaz)

ggplot() + geom_step(aes(pred.s.km.w1$t, pred.s.km.w1$s.prob, color = 'K-M')) +
  geom_step(aes(pred.s.cox.w1$t, rowMeans(pred.s.cox.w1[,-1]), color = 'COXPH')) +
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

pred.cox.w4 <- survfit(model.cox.w4, data.w4[-tr4, -1])
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

pred.rsf.w4 <- predict(model.rsf.w4, data.w4[-tr4, -1])
pred.s.rsf.w4 <- data.frame(t = pred.rsf.w4$time.interest, t(pred.rsf.w4$survival))

# variable importance
data.frame(variable = names(model.rsf.w4$importance), 
           importance = model.rsf.w4$importance)[1:10,] %>%
  ggplot() + 
  geom_col(aes(fct_reorder(str_remove(variable, '_w4'), importance), importance, fill = importance)) +
  scale_fill_gradient(low = '#18bec5', high = 'black') +
  theme(legend.position = '', axis.text.x = element_blank()) +
  labs(x = '', y = '') + coord_flip()
# playtime, n_day, max_payment, n_payment, mean_payment, total_exp


# Summary 4 --------------------------------------------------------------------------------------------
km.fit.w4 <- survfit(Surv(survival_time, churn) ~ 1, data.w4[-tr4,])
pred.s.km.w4 <- data.frame(t= km.fit.w4$time, s.prob = km.fit.w4$surv, H = km.fit.w4$cumhaz)

ggplot() + geom_step(aes(pred.s.km.w4$t, pred.s.km.w4$s.prob, color = 'K-M')) +
  geom_step(aes(pred.s.cox.w4$t, rowMeans(pred.s.cox.w4[,-1]), color = 'COXPH')) +
  geom_step(aes(pred.rsf.w4$t, rowMeans(pred.s.rsf.w4[,-1]), color = 'RSF')) +
  scale_color_manual('', values = c('K-M' = 'black', 'COXPH' = 'red', 'RSF' = 'blue')) +
  labs(x = 'time', y = 'survival.prob', title = 'Week 4: Predicted Survival Function') +
  theme(legend.position = 'top')

cind.cox.w4 <- 1 - concordance(model.cox.w4)$concordance
cind.rsf.w4 <- mean(model.rsf.w4$err.rate, na.rm = T)
c(cind.cox.w4, cind.rsf.w4)

# ------------------------------------------------------------------------------------------------------
# week 2-3 model ---------------------------------------------------------------------------------------
data.w23 <- xdata %>% filter(first_week %in% (2:3)) %>% 
  select(-ends_with('w1'), -first_week) %>%
  left_join(select(ydata, acc_id, survival_time, churn))

set.seed(23)
tr23 <- sample(nrow(data.w23), size = nrow(data.w23)*0.8)

# COXPH ------------------------------------------------------------------------------------------------
model.cox.w23 <- coxph(Surv(survival_time, churn) ~ ., data = data.w23[tr23, -1])
model.cox.w23 <- step(model.cox.w23, direction = 'both')
summary(model.cox.w23)

cox.final.w23 <- model.cox.w23$formula
pred.cox.w23 <- survfit(model.cox.w23, newdata = data.w23[-tr23,])
pred.s.cox.w23 <- data.frame(t = pred.cox.w23$time, pred.cox.w23$surv)


# RSF  -------------------------------------------------------------------------------------------------
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
model.rsf.w23 <- rfsrc(cox.final.w23, data.w23[tr23, -1], importance = TRUE,
                       ntree = rf.grid23$ntree[1], mtry = rf.grid23$mtry[1], nodesize = rf.grid23$nodesize[1])
# variable importance
data.frame(variable = names(model.rsf.w23$importance), 
           importance = model.rsf.w23$importance)[1:10,] %>%
  ggplot() + 
  geom_col(aes(fct_reorder(variable, importance), importance, fill = importance)) +
  scale_fill_gradient(low = '#18bec5', high = 'black') +
  theme(legend.position = '', axis.text.x = element_blank()) +
  labs(x = '', y = '') + coord_flip()

pred.rsf.w23 <- predict(model.rsf.w23)
pred.s.rsf.w23 <- data.frame(t = pred.rsf.w23$time.interest, t(pred.rsf.w23$survival))


# Summary 23 -------------------------------------------------------------------------------------------
km.fit.w23 <- survfit(Surv(survival_time, churn) ~ 1, data.w23[-tr23, -1])
pred.s.km.w23 <- data.frame(t= km.fit.w23$time, s.prob = km.fit.w23$surv, H = km.fit.w23$cumhaz)

ggplot() + geom_step(aes(pred.s.km.w23$t, pred.s.km.w23$s.prob, color = 'K-M')) +
  geom_step(aes(pred.s.cox.w23$t, rowMeans(pred.s.cox.w23[,-1]), color = 'COXPH')) +
  geom_step(aes(pred.rsf.w23$t, rowMeans(pred.s.rsf.w23[,-1]), color = 'RSF')) +
  scale_color_manual('', values = c('K-M' = 'black', 'COXPH' = 'red', 'RSF' = 'blue')) +
  labs(x = 'time', y = 'survival.prob', title = 'Week 2-3: Predicted Survival Probabilites') +
  theme(legend.position = 'top')

cind.cox.w23 <- 1 - concordance(model.cox.w23)$concordance
cind.rsf.w23 <- mean(model.rsf.w23$err.rate, na.rm = T)
c(cind.cox.w23, cind.rsf.w23)



# 생존확률 기준 시간 찾기 -------------------------------------------------------------------------------
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

pred.t.cox.w1 <- threshold(pred.s.cox.w1, 0.5)
RMSE(pred.t.cox.w1$pred.t, data.w1$survival_time[-tr1])

pred.t.cox.w23 <- threshold(pred.s.cox.w23, 0.5)
RMSE(pred.t.cox.w23$pred.t, data.w23$survival_time[-tr23])

pred.t.rsf.w4 <- threshold(pred.s.rsf.w4, 0.5)
RMSE(pred.t.rsf.w4$pred.t, data.w4$survival_time[-tr4])


# test 데이터 예측
xtest1 <- read_csv('preprocess/test1_week.csv')
xtest1.w1 <- xtest1 %>% filter(first_week == 1) %>% select(-first_week) 
xtest1.w23 <- xtest1 %>% filter(first_week %in% (2:3)) %>% select(-first_week) 
xtest1.w4 <- xtest1 %>% filter(first_week == 4) %>% select(-first_week) 

xtest2 <- read_csv('preprocess/test2_week.csv')
xtest2.w1 <- xtest2 %>% filter(first_week == 1) %>% select(-first_week) 
xtest2.w23 <- xtest2 %>% filter(first_week %in% (2:3)) %>% select(-first_week) 
xtest2.w4 <- xtest2 %>% filter(first_week == 4) %>% select(-first_week) 

# test1
pred.test1.w1 <- survfit(model.cox.w1, newdata = xtest1.w1)
pred.s.test1.w1 <- data.frame(t = pred.test1.w1$time, pred.test1.w1$surv)
pred.t.test1.w1 <- threshold(pred.s.test1.w1, 0.35)
ggplot(pred.t.test1.w1) + geom_bar(aes(pred.t))

pred.test1.w23 <- survfit(model.cox.w23, newdata = xtest1.w23)
pred.s.test1.w23 <- data.frame(t = pred.test1.w23$time, pred.test1.w23$surv)
pred.t.test1.w23 <- threshold(pred.s.test1.w23, 0.3)
ggplot(pred.t.test1.w23) + geom_bar(aes(pred.t))

pred.test1.w4 <- predict(model.rsf.w4, xtest1.w4)
pred.s.test1.w4 <- data.frame(t = pred.test1.w4$time.interest, t(pred.test1.w4$survival))
pred.t.test1.w4 <- threshold(pred.s.test1.w4, 0.35)
ggplot(pred.t.test1.w4) + geom_bar(aes(pred.t))

# test2
pred.test2.w1 <- survfit(model.cox.w1, newdata = xtest2.w1)
pred.s.test2.w1 <- data.frame(t = pred.test2.w1$time, pred.test2.w1$surv)
pred.t.test2.w1 <- threshold(pred.s.test2.w1, 0.35)
ggplot(pred.t.test2.w1) + geom_bar(aes(pred.t))

pred.test2.w23 <- survfit(model.cox.w23, newdata = xtest2.w23)
pred.s.test2.w23 <- data.frame(t = pred.test2.w23$time, pred.test2.w23$surv)
pred.t.test2.w23 <- threshold(pred.s.test2.w23, 0.3)
ggplot(pred.t.test2.w23) + geom_bar(aes(pred.t))

pred.test2.w4 <- predict(model.rsf.w4, xtest2.w4)
pred.s.test2.w4 <- data.frame(t = pred.test2.w4$time.interest, t(pred.test2.w4$survival))
pred.t.test2.w4 <- threshold(pred.s.test2.w4, 0.35)
ggplot(pred.t.test2.w4) + geom_bar(aes(pred.t))


pred.t.test1.w1 <- pred.t.test1.w1 %>% transmute(acc_id = xtest1.w1$acc_id, survival_time = pred.t) 
pred.t.test1.w23 <- pred.t.test1.w23 %>% transmute(acc_id = xtest1.w23$acc_id, survival_time = pred.t) 
pred.t.test1.w4 <- pred.t.test1.w4 %>% transmute(acc_id = xtest1.w4$acc_id, survival_time = pred.t) 

pred.t.test2.w1 <- pred.t.test2.w1 %>% transmute(acc_id = xtest2.w1$acc_id, survival_time = pred.t) 
pred.t.test2.w23 <- pred.t.test2.w23 %>% transmute(acc_id = xtest2.w23$acc_id, survival_time = pred.t) 
pred.t.test2.w4 <- pred.t.test2.w4 %>% transmute(acc_id = xtest2.w4$acc_id, survival_time = pred.t) 

pred.t.test1 <- pred.t.test1.w1 %>% 
  full_join(pred.t.test1.w23) %>% full_join(pred.t.test1.w4) %>%
  arrange(acc_id)
pred.t.test2 <- pred.t.test2.w1 %>% 
  full_join(pred.t.test2.w23) %>% full_join(pred.t.test2.w4) %>%
  arrange(acc_id)

write.csv(pred.t.test1, 'predict/test1_predict_survival.csv', row.names = FALSE)
write.csv(pred.t.test2, 'predict/test2_predict_survival.csv', row.names = FALSE)


