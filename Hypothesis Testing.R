setwd("~/Documents/GWU/Stats for Analytics/Week 6")
data <- read.table("Restaurant.txt", header=TRUE)

data$Agegroup <- as.factor(data$Agegroup)
summary(data)

age27.37 <- subset(data, Agegroup==2)
age27.37.B <- subset(age27.37, Pref=="B")

library(stats)
prop.test(x=nrow(age27.37.B), n=nrow(age27.37), p=0.20, alternative="greater", conf.level = 0.95, correct=FALSE)


age38.47 <- subset(data, Agegroup==3)
age27.37.C <- subset(age27.37, Pref=="C")
age38.47.C <- subset(age38.47, Pref=="C")
prop.test(x=c(nrow(age27.37.C), nrow(age38.47.C)), n=c(nrow(age27.37), nrow(age38.47)), alternative="less",conf.level = 0.95, correct=FALSE)

all_R <- nrow(subset(data, Pref=="R"))
all_B <- nrow(subset(data, Pref=="B"))
prop.test(x=all_R, n=all_R+all_B, p=0.60, alternative = "greater", conf.level = 0.95, correct=FALSE)