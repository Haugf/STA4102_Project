# Ames Iowa Dataset Code 

# Fix Numbers
options(scipen = 999)

# Imports 
library(tidyverse)

# If you don't have tidyverse, try running install.packages("tidyverse",dependencies = TRUE) 
# in the console

# Load in the Ames Iowa Housing Dataset 
ames.table <- read.csv("Desktop/STA4102Project/STA4102_Project/Data/AmesHousing.csv", fileEncoding = "UTF-8")

# Make a copy for cleaning purposes 
ames.copy <- data.frame(ames.table)

# We can use $ to access specific coulmns in our table 
ames.table$SalePrice

# Check to see if we have the same memory address 
# tracemem(ames.table)==tracemem(ames.copy) # Should return false if we copied correctly

# Plot Sale Price in a histogram using ggplot2 
ggplot(data=ames.table) + 
  geom_histogram(mapping = aes(x = ames.table$SalePrice))

# Plot Sale Price vs Lot Area 
ggplot(data=ames.table) + 
  geom_point(mapping = aes(x=ames.table$Lot.Area, y=ames.table$SalePrice))

# Data Cleaning 
# TODO: Eliminate all sales except for the "normal" from SALES CONDITION variable 
# TODO: Remove all homes with a living area of (GR LIVING AREA ) above 1500 square feet 

# Eliminate all sales except for "normal" from SALES CONDITION variable 
ames.copy <- filter(ames.copy, ames.copy$Sale.Condition == "Normal")
# ames.copy$Sale.Condition

# Sale Price vs Lot Area 
ggplot(data=ames.copy) + 
  geom_point(mapping = aes(x=ames.copy$Gr.Liv.Area, y=ames.copy$SalePrice))

# Check to see if all sales conditions are normal
table(ames.copy$Sale.Condition)

# Redo the above plots 
ggplot(data=ames.copy) + 
  geom_histogram(mapping = aes(x=ames.copy$SalePrice))

ggplot(data=ames.copy) + 
  geom_point(mapping = aes(x=ames.copy$Lot.Area, y=ames.copy$SalePrice))

# Plot boxplot of lot area 
ggplot(data=ames.copy) + 
  geom_boxplot(mapping = aes(x="",y=ames.copy$Gr.Liv.Area))
  
# Remove all homes with a living area above 1500 square feet (82 observations) 
ames.copy <- filter(ames.copy, ames.copy$Gr.Liv.Area <= 1500)

# Plot price vs Gr Liv Area 
ggplot(data = ames.copy) + 
  geom_point(mapping = aes(x=ames.copy$Gr.Liv.Area, y=ames.copy$SalePrice))

# Plot histogram of price 
ggplot(data=ames.copy) + 
  geom_histogram(mapping = aes(x=ames.copy$SalePrice))

# Choose numerical columns 
# Find the columns which are numeric 
numeric_cols <- sapply(ames.copy, is.numeric)

#Now store the numeric columns as its own list 
ames_list <- ames.copy[,numeric_cols]

# We need to drop a few columns 
# Order - Observation Number 
# PID - (Nominal) Parcel identification number 
# MS.SubClass - (Nominal) Identifies type of dwelling involved in the sale 
# Overall.Qual - (Ordinal) Rates overall material and finish of the house 
# Overall.Cond - (Ordinal) Rates the overall condition of the house 
# BsmtFin.SF.1 - (Continuous) Type 1 finished 
# BsmtFin.SF.2 - (Continuous) Type 2 finished 


s.test <- shapiro.test(ames.copy$SalePrice)
print(s.test)

#boxplot(ames.copy$SalePrice~ames.copy$MS.Zoning,xlab="Zoning", ylab="Sale Pirce", main="Zoning  / Sale Price")

ggplot(data=ames.copy, aes(x=ames.copy$MS.Zoning, y=ames.copy$SalePrice)) + geom_boxplot()

#boxplot(ames.copy$SalePrice~ames.copy$MS.Zoning,xlab="Zoning", ylab="Sale Pirce", main="Zoning  / Sale Price")


amesNum <-subset(ames.copy, select = c(Lot.Area, Mas.Vnr.Area)) 
#heatmap(as.matrix(amesNum))


Regression <- lm(ames.copy$SalePrice ~ ames.copy$Year.Built +ames.copy$Year.Remod.Add+ames.copy$MS.Zoning)

summary(Regression)

qqnorm(rstandard(Regression))
qqline(rstandard(Regression))

s.test <- shapiro.test(residuals(Regression))
print(s.test)


plot(ames.copy$Lot.Frontage, ames.copy$SalePrice, xlab="Lot Frontage", ylab="Sale Pirce", main="ames.copy Lot Frontage  / Sale Price")

ggplot(data=ames.copy, aes(x=ames.copy$Neighborhood, y=ames.copy$SalePrice)) + geom_boxplot()
ggplot(data=ames.copy, aes(x=ames.copy$Foundation, y=ames.copy$SalePrice)) + geom_boxplot()
ggplot(data=ames.copy, aes(x=ames.copy$Overall.Cond, y=ames.copy$SalePrice)) + geom_boxplot()
ggplot(data=ames.copy, aes(x=ames.copy$Overall.Qual, y=ames.copy$SalePrice)) + geom_bar(stat="identity", fill="steelblue")

