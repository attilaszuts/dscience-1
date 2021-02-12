library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)
library(caret)
library(party)
library(doMC)
library(pROC)

raw_train <- fread("Titanic/train.csv", header=TRUE)
raw_test <- fread("Titanic/test.csv", header=TRUE)

train <- raw_train[, c("Survived", "Sex", "Fare"), with=FALSE]
test <- raw_test[, c("Sex", "Fare"), with=FALSE]

colMeans(test[,c("Age")], na.rm=TRUE)

test[is.na(test$Fare), c("Fare")] <- colMeans(train[, c("Fare")], na.rm=TRUE)
train[is.na(train$Fare),c("Fare")] <- colMeans(train[,c("Fare")], na.rm=TRUE)

train$Survived <- factor(train$Survived, labels=c("Survived", "NotSurvived"))

trctrl <- trainControl(method = "none")
lmCaret <- train(Survived~., 
                 data = train, 
                 method = "glm",
                 preProcess=c("center", "scale"),
                 trControl=trctrl,
                 na.action=na.pass
)
summary(lmCaret)
lmRoc <- roc(
  predictor=predict(lmCaret, train, type='prob', decision.values=T)$Survived, 
  response=raw_train$Survived)
lmRoc
plot(lmRoc)

to_submit <- data.table(
  PassengerId = raw_test$PassengerId,
  Survived = round(predict(lmCaret, test, type='prob', decision.values=T)$Survived))

write.csv(to_submit, "submit_titanic.csv", row.names=FALSE)
