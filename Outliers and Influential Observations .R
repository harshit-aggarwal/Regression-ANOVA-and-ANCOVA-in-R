library(gvlma)
setwd("~/Documents/GWU/Stats for Analytics II/Week 5")
data <- read.table("Assignment4_2022.dat", header=TRUE)

data.lm <- lm(sales~age+income+education+tenure, data=data)
summary(data.lm)

data.gvl <- gvlma(data.lm)
summary(data.gvl)

# Identifying X-outliers

hv <- as.data.frame(hatvalues(data.lm))
colnames(hv) <- c("hatvalues")
mn <- mean(hv$hatvalues)
hv$warn <- ifelse(hv$hatvalues>3*mn, 'x3',
            ifelse(hv$hatvalues>2*mn, 'x2', '-'))
subset(hv, warn %in% c('x2', 'x3'))

# Identifying Y-outliers

rs <- as.data.frame(rstudent(data.lm))
colnames(rs) <- "rstudent"
critval <- qt(.95, nrow(rs)-2-1)
rs$warn <- ifelse(abs(rs$rstudent) > critval,
                  'Warn', '-')
subset(rs, warn=="Warn")

# Influential Observations with respect to intercept and slopes

dfb <- as.data.frame(dfbetas(data.lm))
critval <- 2/sqrt(nrow(dfb))
dfb$Warn <- ifelse(abs(dfb)>critval, "Warn", "-")
subset(dfb, Warn[,1]=="Warn" | Warn[,2]=="Warn" | Warn[,3]=="Warn" | Warn[,4]=="Warn" | Warn[,5]=="Warns")

# Influential Observations with respect to Cook's Distance

cd <- as.data.frame(cooks.distance(data.lm))
colnames(cd) <- "CooksD"
critval <- qf(.50, 2, nrow(cd)-2)
cd$warn <- ifelse(abs(cd$CooksD)>critval,
                  'Warn', '-')
subset(cd, warn=="Warn")


