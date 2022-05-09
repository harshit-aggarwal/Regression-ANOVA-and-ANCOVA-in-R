setwd("~/Documents/GWU/Stats for Analytics II/Week 4")
data <- read.table("AAA.dat", header=TRUE)

# Linear Model
AAA.lm <- lm(Demand~Price, data=data)
summary(AAA.lm)

data["LnDemand"] <- log(data$Demand)
data["LnPrice"] <- log(data$Price)

# Log-Lin (LnDemand)
AAA.ll1 <- lm(LnDemand~Price, data=data)
summary(AAA.ll1)

# Lin-Log (LnPrice)
AAA.ll2 <- lm(Demand~LnPrice, data=data)
summary(AAA.ll2)

# Log-Log Model
AAA.log <- lm(LnDemand~LnPrice, data=data)
summary(AAA.log)

predict(AAA.lm, data.frame(Price = 3))

lnPredict <- predict(AAA.ll1, data.frame(Price=3))
exp(lnPredict)

lnInquiryPrice <- log(3)
logPredict <- predict(AAA.log, data.frame(LnPrice=lnInquiryPrice))
exp(logPredict)
