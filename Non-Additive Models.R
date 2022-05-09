setwd("~/Documents/GWU/Stats for Analytics II/Week 3")
data <- read.table("Daxguard.txt", header=TRUE)
data$Gender <- as.factor(data$Gender)
summary(data)

Daxguard.slr <- lm(Sysdiff~Age, data=data)
summary(Daxguard.slr)

male.data <- subset(data, Gender=="M")
summary(male.data)
male.slr <- lm(Sysdiff~Age, data=male.data)
summary(male.slr)

plot(1, type = "n",
     xlab="Age", ylab="Sysdiff",
     xlim=c(0,80), ylim=c(0,50))
abline(male.slr)

female.data <- subset(data, Gender=="F")
summary(female.data)
female.slr <- lm(Sysdiff~Age, data=female.data)
summary(female.slr)

Daxguard.slr2 <-  lm(Sysdiff~Age+Gender+Age*Gender, data=data)
summary(Daxguard.slr2)
predict(Daxguard.slr2,data.frame(Gender="F", Age=49.51)) - predict(Daxguard.slr2,data.frame(Gender="M", Age=49.51))

# Main Effect of Age
avgint <- Daxguard.slr$coefficients[1]+Daxguard.slr$coefficients[3]/2
avgslope <- Daxguard.slr$coefficients[2]+Daxguard.slr$coefficients[4]/2
newdata <- matrix(nrow=2,ncol=2)
newdata[1,1] <- avgint + avgslope*0
newdata[2,1] <- avgint + avgslope*80
newdata[1,2] <- 0
newdata[2,2] <- 80
newdata <- data.frame(newdata)
names(newdata) <- c("Sysdiff", "Age")
newdata.lm <- lm(Sysdiff~Age, data=newdata)
plot(1, type="n",
     xlab="Age", ylab="Sysdiff",
     xlim=c(0,80), ylim=c(0,50))
abline(female.slr, lty=1)
abline(male.slr, lty=2)
abline(newdata.lm, lty=3)
legend("bottomright", c("F","M"), lty=c(1,2))

predict(Daxguard.slr2,data.frame(Gender="F", Age=25)) - predict(Daxguard.slr2,data.frame(Gender="M", Age=30))

predict(Daxguard.slr2,data.frame(Gender="M", Age=30)) - predict(Daxguard.slr2,data.frame(Gender="F", Age=25))

predict(Daxguard.slr2,data.frame(Gender="F", Age=25))
predict(Daxguard.slr2,data.frame(Gender="M", Age=30))
