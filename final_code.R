# Ames Iowa Dataset Code 

# We have added all of our groups code into one file 

# Data Cleaning and initial plotting 
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

# Production code to plot final graphs and fit final models

# Deals with Scientific notation in Plots 
options(scipen = 999)

# Useful functions  
library(tidyverse)

# Path Information 
path_dirty = "~/School_Work/F19/CPSD/Group_Proj/STA4102_Project/Data/AmesHousing.csv"
path_clean = "~/School_Work/F19/CPSD/Group_Proj/STA4102_Project/Data/cleaned_data.csv"
#Import Data
ames.dirty <- read.csv(path_dirty)

ames.clean <-read.csv(path_clean)

# Plot of clean and uncleaned housing price 
fig1 <- ggplot(data = ames.dirty) + 
  geom_histogram(mapping = aes(x = ames.dirty$SalePrice )) +
  ggtitle("Uncleaned")
fig2 <- ggplot(data = ames.clean) + 
  geom_histogram(mapping = aes(x = ames.clean$SalePrice)) + 
  ggtitle("Cleaned")
cowplot::plot_grid(fig1, fig2)

# Looking for NA outliers 
na_count <-sapply(ames.clean, function(y) sum(is.na(y)))
print("Sum of the NA values in each column: ")
na_count

# Store location of NA values for Lot.Frontage 
na_frontage <- is.na(ames.clean$Lot.Frontage)
ames.clean[na_frontage,]

# Grab all the numeric columns 

# Find the columns which are numeric 
numeric_cols <- sapply(ames.clean, is.numeric)

#Now store the numeric columns as its own list 
clean_numeric <- ames.clean[,numeric_cols]

# Print column names 
colnames(clean_numeric)

# Copy clean data frame 
copy_numeric <- data.frame(clean_numeric)

# Check to see if we properly copied 
#racemem(copy_numeric)==tracemem(clean_numeric)
# the above should return false 

# Drop "X" and "Order" as they are meaningless
copy_numeric <-select(copy_numeric, -one_of(c("X","Order","PID")))

copy_numeric
length(copy_numeric)

# Fit full model 
Regression <- lm(SalePrice ~., data=copy_numeric)
summary(Regression)

# Analysis of residuals 
qqnorm(rstandard(Regression))
qqline(rstandard(Regression))
s.text <- shapiro.test(residuals(Regression))
print(s.text)

# Look for linearly dependent terms 
alias(Regression2)

# Lets make a heatmap of the correlations 
heat_vals <- c("SalePrice", "Total.Bsmt.SF", "BsmtFin.SF.1", "BsmtFin.SF.2",
               "Bsmt.Unf.SF", "Gr.Liv.Area", "X1st.Flr.SF", "X2nd.Flr.SF",
               "Low.Qual.Fin.SF")

my_cor_vals <- select(copy_numeric, heat_vals)
cormat <- round(cor(my_cor_vals),2)
melted_cormat <- melt(cormat)
melted_cormat

# Plot heatmap 
ggplot(data=melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Drop the two linearly dependent columns  
copy_numeric <-select(copy_numeric, -one_of(c("Total.Bsmt.SF", "Gr.Liv.Area")))

length(copy_numeric)

# Refit the model 
Regression2 <- lm(SalePrice ~., data=copy_numeric)
summary(Regression2)

# Stepwise Regression 
# Can't preform stepwise regression with missing values, should we just omit them?
na_omit_copy_numeric <-na.omit(copy_numeric)
omit_regression <- lm(SalePrice ~., data=na_omit_copy_numeric)
first_step <- MASS::stepAIC(omit_regression, direction = "both", trace = FALSE)
summary(first_step)

# Analysis of residuals 
qqnorm(rstandard(first_step))
qqline(rstandard(first_step))
s.text <- shapiro.test(residuals(first_step))
print(s.text)

# Regression of year build and year remoldedd 
Regression <- lm(MyData$SalePrice ~ MyData$Year.Built +MyData$Year.Remod.Add)

summary(Regression)
qqnorm(rstandard(Regression))
qqline(rstandard(Regression))

s.test <- shapiro.test(residuals(Regression))
print(s.test)

































