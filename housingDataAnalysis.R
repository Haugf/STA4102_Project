library(ggplot2)
getwd()
MyData <- read.csv(file='Desktop/STA4102Project/STA4102_Project/Data/AmesHousing.csv')


hist(MyData$SalePrice)

MyData$logSale = log(MyData$SalePrice)

hist(MyData$logSale, breaks=15)

hist(MyData$Year.Built)


#----- Year Built / Sale Price
plot(MyData$Year.Built, MyData$SalePrice, xlab="Year Built", ylab="Sale Pirce", main="Year Built  / Sale Price")
linearMod <- lm(MyData$SalePrice ~ MyData$Year.Built, data=MyData)  # build linear regression model on full data
abline(lm(MyData$SalePrice ~ MyData$Year.Built, data=MyData) , col="red")
summary(linearMod)
mean(linearMod$residuals)

cor(MyData$Year.Built, MyData$SalePrice)



par(mfrow=c(1, 2))  # divide graph area in 2 columns
#plot(density(MyData$Year.Built), main="Density Plot: Year Built", ylab="Frequency", sub=paste("Skewness:", round(MyData$Year.Built, 2)))  # density plot for 'speed'
#polygon(density(MyData$Year.Built), col="red")
#plot(density(MyData$SalePrice), main="Density Plot: Sale Price", ylab="Frequency", sub=paste("Skewness:", round(MyData$SalePrice, 2)))  # density plot for 'dist'
#polygon(density(MyData$SalePrice), col="red")

Regression <- lm(MyData$SalePrice ~ MyData$Year.Built +MyData$Year.Remod.Add)

summary(Regression)

#cor(Regression)

#qqnorm(rstandard(Regression))
#qqline(rstandard(Regression))

#s.test <- shapiro.test(residuals(Regression))
#print(s.test)

#s1.test <- shapiro.test(residuals(MyData$logSale))

#print(s1.test)

# look at dist of each variable, and try to take the log or exp transformation of the distribution


boxplot(MyData$SalePrice~MyData$MS.Zoning,xlab="Zoning", ylab="Sale Pirce", main="Zoning  / Sale Price")

ggplot()
