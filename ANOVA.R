setwd("~/Documents/GWU/Stats for Analytics/Week 3")
data <- read.table("Odnetnin.dat", header=TRUE)
nrow(data)

Healthiness.slr <- lm(Costs~Healthiness, data=data)
summary(Healthiness.slr)

Engagement.slr <- lm(Costs~Engagement, data=data)
summary(Engagement.slr)

data.mlr <- lm(Costs~Engagement+Healthiness, data=data)
summary(data.mlr)

require(heplots)
etasq(data.mlr, anova=TRUE, partial=FALSE)

library(agricolae)
correlation(data$Engagement, data$Healthiness, method="pearson")

predict(data.mlr, data.frame(Healthiness=50, Engagement=25))