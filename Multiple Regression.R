setwd("~/Documents/GWU/Stats for Analytics/Week 2/Assignment 2")
FlixIt <- read.table("FlixIt.dat", header = FALSE)
nrow(FlixIt)
summary(FlixIt)

Customer_ID <- FlixIt[, 1]
Hours <- FlixIt[, 2]
Children <- FlixIt[, 3]
Income <- FlixIt[, 4]
History <- FlixIt[, 5]

Children.slr <- lm(Hours~Children)
summary(Children.slr)

Income.slr <- lm(Hours ~ Income)
summary(Income.slr)

History.slr <- lm(Hours~History)
summary(History.slr)

FlixIt.mlr <- lm(Hours ~ Children+Income+History)
summary(FlixIt.mlr)

require(heplots)
etasq(FlixIt.mlr, anova=TRUE, partial=FALSE)
