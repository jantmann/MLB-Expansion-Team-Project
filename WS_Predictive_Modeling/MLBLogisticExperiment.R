library(tidyverse)
library(MASS, exclude = "select")
library(klaR)
library(ICS)
library(corrplot)

team_stats <- read.csv('/Users/jantmann17/Desktop/Portfolio-Projects/MLB-Expansion-Team-Project/all_team_data.csv')
team_stats <- team_stats[, -c(1,2)]
team_stats$WS_Winner <- factor(team_stats$WS_Winner)
set.seed(61702)

# Train v Test split
sample.data <- sample.int(nrow(team_stats), floor(.50*nrow(team_stats)), replace = F)
train <- team_stats[sample.data, ]
test <- team_stats[-sample.data, ]

# Some EDA
table(train$WS_Winner)
prop.table(table(train$WS_Winner))

ggplot2::ggplot(train, aes(x=WS_Winner, y = OPS))+
  geom_boxplot()+
  labs(x="Won the World Series?", y="OPS", 
       title="Boxplot of WS vs OPS")

contrasts(train$WS_Winner)
levels(train$WS_Winner)



result_train1 <- glm(WS_Winner ~ Runs_Scored + Hits_For + Doubles + Triples + HR_For + Walks_For
                     + SO_Bat + SB + Runs_Allowed + ER + ERA + CG + tSho + SV + Hits_Allowed 
                     +  HR_Allowed + Walks_Allowed + SO_Pitch + Errors_Committed + GDP + Fielding_Percentage
                     + Batting_Age + Pitching_Age, family = binomial, data = train)

result_train2 <- glm(WS_Winner ~ Runs_Scored + Hits_For + Walks_For + SO_Bat + SB + Runs_Allowed
                     + ER + tSho + SV, family = binomial, data = train)

result_train3 <- glm(WS_Winner ~ Runs_Scored + SO_Bat + Runs_Allowed
                    + tSho + SV, family = binomial, data = train)

result_train4 <- glm(WS_Winner ~ Runs_Scored + Runs_Allowed
                     + tSho + SV, family = binomial, data = train)

summary(result_train)

summary(vif_result)

vif_result2 <- glm(WS_Winner ~ tSho + SV + SO_Bat + Pitching_Age, family = binomial, data = train)
summary(vif_result2)

summary(result_train1)

summary(result_train2)

summary(result_train3)

summary(result_train4)

preds <- predict(result_train3, newdata = test, type="response")
rates <- ROCR::prediction(preds, test$WS_Winner)
roc_result <- ROCR::performance(rates, measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc<-ROCR::performance(rates, measure = "auc")
auc@y.values


# trying to find optimal threshold

install.packages('pROC')
library(pROC)
roc_obj <- roc(test$WS_Winner, preds)
plot(roc_obj, main = "ROC Curve")
optimal_threshold <- coords(roc_obj, "best", ret = "threshold")

confusion.mat <- table(test$WS_Winner,preds > 0.1)
confusion.mat


#############################
# trying to address oversampling

install.packages('ROSE')
library(ROSE)

team_stats <- read.csv('/Users/jantmann17/Desktop/Portfolio-Projects/MLB-Expansion-Team-Project/all_team_data.csv')
team_stats <- team_stats[, -c(1,2)]
team_stats$WS_Winner <-factor(team_stats$WS_Winner)

## oversampling with replacement minority class, undersampling without replacement majority class
data_balanced_both <- ovun.sample(WS_Winner ~ ., data = train, method = "both", p=0.5, N = 1387, seed = 4630)$data

table(data_balanced_both$WS_Winner)

contrasts(data_balanced_both$WS_Winner)
levels(data_balanced_both$WS_Winner)

result_train3 <- glm(WS_Winner ~ Runs_Scored + Triples + SO_Bat + tSho + SV, family = binomial, data = train)

result_train4 <- glm(WS_Winner ~ Runs_Scored + SO_Bat + tSho + SV, family = binomial, data = train)

summary(result_train3)

summary(result_train4)

preds <- predict(result_train4, newdata = test, type="response")
rates <- ROCR::prediction(preds, test$WS_Winner)
roc_result <- ROCR::performance(rates, measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc<-ROCR::performance(rates, measure = "auc")
auc@y.values

confusion.mat <- table(test$WS_Winner,preds > 0.4)
confusion.mat

