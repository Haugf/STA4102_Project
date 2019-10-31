getwd()
MyData <- read.csv(file='Desktop/STA4102Project/STA4102_Project/Data/AmesHousing.csv')

#----- Year Built / Sale Price
plot(MyData$Year.Built, MyData$SalePrice, xlab="Year Built", ylab="Sale Pirce", main="Year Built  / Sale Price")
linearMod <- lm(MyData$SalePrice ~ MyData$Year.Built, data=MyData)  # build linear regression model on full data
abline(lm(MyData$SalePrice ~ MyData$Year.Built, data=MyData) , col="red")
summary(linearMod)

#----- Year Remod  / Sale Price
plot(MyData$Year.Remod.Add, MyData$SalePrice, xlab="Year Built", ylab="Sale Pirce", main="Year Remod  / Sale Price")
linearMod <- lm(MyData$SalePrice ~ MyData$Year.Remod.Add, data=MyData)  # build linear regression model on full data
abline(lm(MyData$SalePrice ~ MyData$Year.Remod.Add, data=MyData) , col="red")
summary(linearMod)

#-----Failure of an exponential linear model
exponential.model <- lm(log(MyData$SalePrice) ~ MyData$Year.Built)
summary(exponential.model)

#-----A 3d plot I don't understand
x<- MyData$Year.Remod.Add
y<-MyData$Year.Built

cone <- function(x, y){
  sqrt(x^2+y^2)
}
x <- y <- seq(-1, 1, length= 20)
z <- outer(x, y, cone)
persp(x, y, z, main="Sales Price")

#-----Faliure of a multivariate model [I dont understand what the x-cor is]
plot((MyData$Year.Built+MyData$Year.Remod.Add), MyData$SalePrice)
linearMod2 <- lm(MyData$SalePrice ~ I(MyData$Year.Built + MyData$Year.Remod.Add))
summary(linearMod2)
abline(linearMod2, col="blue")

#Bar plot on the bathroom 
barplot(height=MyData$SalePrice, width=MyData$Full.Bath, main="Car Distribution")




