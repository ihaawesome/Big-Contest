library(tidyverse)

test1.spent <- read_csv('predict/test1_predict_amount_spent.csv')
test1.surv <- read_csv('predict/test1_predict_survival.csv')
test1.final <- test1.surv %>% left_join(test1.spent)

test2.spent <- read_csv('predict/test2_predict_amount_spent.csv')
test2.surv <- read_csv('predict/test2_predict_survival.csv')
test2.final <- test2.surv %>% left_join(test2.spent)

sum(is.na(test1.final))
sum(is.na(test2.final))

write.csv(test1.final, 'predict/test1_predict.csv', row.names = FALSE, fileEncoding = 'utf8')
write.csv(test2.final, 'predict/test2_predict.csv', row.names = FALSE, fileEncoding = 'utf8')
