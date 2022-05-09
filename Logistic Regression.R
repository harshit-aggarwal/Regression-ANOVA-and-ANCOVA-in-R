library(pscl)
library(InformationValue)
library(pROC)
library(ResourceSelection)
setwd("~/Documents/GWU/Stats for Analytics II/Week 1")
data <- read.table("FlixIt_2022.txt", header=TRUE)
head(data)
summary(data)

logit <- glm(Partic~Age, data=data, family=binomial(link="logit"))
summary(logit)

pR2(logit)
exp(0.27695)
data["PredVal"] <- predict(logit, list(Age=data$Age), type="link")
data["PredProb"] <- predict(logit, list(Age=data$Age), type="response")
head(data)

confusionMatrix(data$Partic, data$PredProb, 0.5)
sensitivity(data$Partic, data$PredProb, 0.5)
specificity(data$Partic, data$PredProb, 0.5)
precision(data$Partic, data$PredProb, 0.5)
npv(data$Partic, data$PredProb, 0.5)
(134+29)/(134+29+15+22)

pred30 <- predict(logit, newdata=data.frame(Age=c(30)), type="link")
exp(pred30)

pred40 <- predict(logit, newdata=data.frame(Age=c(40)), type="link")
exp(pred40)

pred30to40 <- predict(logit, newdata=data.frame(Age=c(40,30)), type="link")
odds <- exp(pred30to40)
odds[1]/odds[2]

ROC.curve = roc(Partic~Age, data=data)
plot(ROC.curve, col='red')
auc(ROC.curve)

optimalCutoff(data$Partic, data$PredProb)
confusionMatrix(data$Partic, data$PredProb, 0.3814781)
(132+37)/(132+37+14+17)

hoslem.test(data$Partic, fitted(logit), g=10)
