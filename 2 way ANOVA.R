setwd("~/Documents/GWU/Stats for Analytics/Week 5")

data <- read.table("Aristocrats.dat", header=TRUE)

data$Combo <- as.factor(data$Combo)
summary(data)

NP <- lm(NetProfit~Combo, data=data)
summary(NP)
require(heplots)
etasq(NP, anova=TRUE, partial=FALSE)
library(agricolae)
scheffe.test(NP, "Combo", group=TRUE, console=TRUE, main="Scheffe Test")


Multi <- lm(NetProfit~Combo+Temp, data=data)
summary(Multi)
etasq(Multi, anova=TRUE, partial=FALSE)
library(lsmeans)
lsmeans(Multi, "Combo")
