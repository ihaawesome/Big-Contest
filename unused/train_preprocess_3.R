setwd('C:/Users/HK/Desktop/GitHub/Big-Contest')
library(tidyverse)
library(mclust)

train_label <- read_csv('preprocess/train_label_preprocess_1.csv')
train_main <- read_csv('preprocess/train_main.csv')
train_main <- train_main %>% mutate(class = factor(class, levels = 0:7))
train_main <- train_main %>% left_join(train_label)

mat <- scale(train_main[,-c(1,2,20)])

km.list <- list()
wss <- c()
for (i in 2:5) km.list[[i]] <- kmeans(mat, centers = i, nstart = 100)
for (i in 2:5) wss[i] <- km.list[[i]]$tot.withinss

km.out <- kmeans(mat, centers = 2, nstart = 50)

table(km.out$cluster)
km.out$tot.withinss


mclust.out <- Mclust(mat, G = 2)
summary(mclust.out)

plot(mclust.out)

group <- km.out$cluster
group <- mclust.out$classification
table(group)
ggplot(train_main) + geom_density(aes(survival_time, fill = factor(group)), alpha = 0.5)

hcl.out <- hclust(dist(mat), method = 'average')
summary(hcl.out)
plot(hcl.out)
