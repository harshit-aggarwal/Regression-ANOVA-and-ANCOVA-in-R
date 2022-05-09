setwd("~/Documents/GWU/Stats for Analytics/Week 1/Assignment 1")
FlixIt <- read.table("FlixIt.dat", header = FALSE)
nrow(FlixIt)
summary(FlixIt)

Customer_ID <- FlixIt[, 1]
Hours <- FlixIt[, 2]
Children <- FlixIt[, 3]
Income <- FlixIt[, 4]
History <- FlixIt[, 5]

sd(Income)
sd(Children)
sd(History)

Income.slr <- lm(Hours~Income, data=FlixIt)
plot(Hours~Income, data=FlixIt)
abline(Income.slr)
summary(Income.slr)

Children.slr <- lm(Hours~Children, data=FlixIt)
plot(Hours~Children, data=FlixIt)
abline(Children.slr)
summary(Children.slr)

History.slr <- lm(Hours~History, data=FlixIt)
plot(Hours~History, data=FlixIt)
abline(History.slr)
summary(History.slr)
