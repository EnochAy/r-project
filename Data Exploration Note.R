#Data Exploration
#Loading data sets
#Load datasets : training and test
library(readxl)
test <- read_excel("C:/Users/Hp/Desktop/KPMG_Data/KPMG_Project/Test.xlsx")
Train_1 <-  read_excel("C:/Users/Hp/Desktop/KPMG_Data/KPMG_Project/Train_1.xlsx")
Train_2 <- read_excel("C:/Users/Hp/Desktop/KPMG_Data/KPMG_Project/Train_2.xlsx")
Train_3 <- read_excel("C:/Users/Hp/Desktop/KPMG_Data/KPMG_Project/Train_3.xlsx")
Postcode_N <- read_excel("Postcode_N.xlsx")
library(dplyr)
# joining train(1,2,3) data sets transaction and identity tables
train <- full_join(Train_2, Train_3)
train <- full_join(train, Train_1)
rm(Train_1, Train_2, Train_3)


#save data
save(list=c("test","train"),file="test_train.Rdata")
data(test_train.Rdata)
#Exploring data structure
str(test)
#1000 obs. (records or examples: customers, n = 1000) of  23 variables(features)
str(train)
#20510 obs. (records or examples: customers, n=20510) of  29 variables(features)

#listing the names of variables/features/columns
names(test)
names(train)
#Data types #types of each variable or attributes
sapply(test, class)
sapply(train, class)

#Class Distribution
y <- train$wealth_segment
cbind(freq=table(y), percentage=prop.table(table(y))*100)
#                   freq percentage
#Affluent Customer  4986   24.31721
#High Net Worth     5214   25.42918
#Mass Customer     10304   50.25361
x <- test$wealth_segment
cbind(freq=table(x), percentage=prop.table(table(x))*100)
#                  freq percentage
#Affluent Customer  241       24.1
#High Net Worth     251       25.1
#Mass Customer      508       50.8

#List
#corrections:
#Discrete age  is represented as categorical data, a factor
#Point one
#Checking the Age and tenure variables distributions in the train dataset
#skewness: measure of asymmetry of distribution, i.e deviation from normal
#bell-shaped distribution, positive: right side is longer, otherwise, negative.
#The mean of a positive skew > the median, opp in negative skewness.
#The more skewed the data, the less accurate the model will be.
#To deal with skewness, use;
#Binning (Also called quantization): This used for transforming continuous
#numeric features into discrete ones(categories), e.g the age. The numbers are
# considered as caegories or bins into which the continuos values are binned or
# grouped
#
#
#Code to install packages for discretizig variables
if(!require(psych)){install.packages("psych")}
if(!require(cluster)){install.packages("cluster")}
if(!require(fpc)){install.packages("fpc")}
library(psych)
library(cluster)
library(fpc)

install.packages("arules")
library(arules)
#Discretizing Age variable
#for train dataset
train$Age <- as.integer(train$Age)
test$Age <- as.integer(test$Age)
full$Age <- as.integer(full$Age)


x <- train$Age
train_Age_D <- discretize(x, method = "frequency", breaks = 3, labels = NULL,
           include.lowest = TRUE, right = FALSE, dig.lab = 3,
           ordered_result = FALSE, infinity = FALSE, onlycuts = FALSE)
summary(train_Age_D)
#Converting the Age var into discrete
train$Age <- train_Age_D
hist(x, breaks = 20, main = "Data")
def.par <- par(no.readonly = TRUE)
layout(mat = rbind(1:2, 3:4))

table(discretize(x, breaks = 3))
hist(x, breaks = 20, main = "Equal Frequency")
abline(v = discretize(x, breaks = 3, onlycuts = TRUE, col = "red"))

#for test dataset
y <- test$Age
test_Age_D <- discretize(y, method = "frequency", breaks = 3, labels = NULL,
                          include.lowest = TRUE, right = FALSE, dig.lab = 3,
                          ordered_result = FALSE, infinity = FALSE, onlycuts = FALSE)
summary(test_Age_D)
#Converting the Age var into discrete
test$Age <- test_Age_D
hist(x, breaks = 20, main = "Data")
def.par <- par(no.readonly = TRUE)
layout(mat = rbind(1:2, 3:4))

table(discretize(x, breaks = 3))
hist(x, breaks = 20, main = "Equal Frequency")
abline(v = discretize(x, breaks = 3, onlycuts = TRUE, col = "red"))



z <- full$Age
full_Age_D <- discretize(z, method = "frequency", breaks = 3, labels = NULL,
                         include.lowest = TRUE, right = FALSE, dig.lab = 3,
                         ordered_result = FALSE, infinity = FALSE, onlycuts = FALSE)
summary(full_Age_D)
#Converting the Age var into discrete
full$Age <- full_Age_D
hist(z, breaks = 20, main = "Data")
def.par <- par(no.readonly = TRUE)
layout(mat = rbind(1:2, 3:4))

table(discretize(x, breaks = 3))
hist(z, breaks = 20, main = "Equal Frequency")
abline(v = discretize(z, breaks = 3, onlycuts = TRUE, col = "red"))

train$Age <- as.integer(train$Age)
test$Age <- as.integer(test$Age)
full$Age <- as.integer(full$Age)




#Equal interval width
train$Age <- table(discretize(x, method = "interval", breaks = 3))
hist(x, breaks = 20, main = "Equal Interval length")
abline(v = discretize(x, method = "interval",  breaks = 3,
                      onlycuts = TRUE, col = "red"))
#k-means clustering
table(discretize(x, method = "cluster", breaks = 3))
hist(x, breaks = 20, main = "k-means")
abline(v = discretize(x, method = "cluster",  breaks = 3,
                      onlycuts = TRUE), col = "red")
#user-specified (with labels)
table(discretize(x, method = "fixed", breaks = c(-Inf, Inf),
                 labels = c("Young", "Old")))
hist(x, breaks = 20, main = "fixed")
abline(v = discretize(x, method = "fixed",  breaks =  100,
                      onlycuts = TRUE), col = "red")
par(def.par) #reset to default
#Discretizing all continuous variables in the dataset
train_D <- discretizeDF(train, methods = list(Age = "frequency", breaks = 2,
                        labels = c("Young", "Old")),
                        default = list(method = "none"))


#Transformation
head(train)
tail(train)
summary(train)
head(test)
tail(test)
summary(test)
#plots for train and test datasets after conversion are now rectangular
hist(train$Age)
hist(test$Age)
#before transformation
#skewed to the left (negatively skewed). It is skewed towards the older age grp
# how will this impact my results when using it to predict over the
# remaining customer base?
hist(test$tenure)
hist(train$tenure)
#skewed to the right a little#
hist(test$Age)
#skewed to the left (negatively skewed)
hist(test$tenure)
#It is symmetric, bell-shaped
hist(train$past_3_years_bike_related_purchases)
#To calculate the skewness for each variable
skew <- apply(train[, 1:29], 2, skew)

#Point two:
# the geographic remoteness of different postcodes may be used as an indicator
# of proximity to consider to whether a customer is in need of a bike to ride
# to work from the ABS data  at different geographic levels thus creating
# additional features for the model.
# Task: To create additional feature for the model (name?)

# Point three:
# Exploration of interactions between different variables through correlation
# analysis and look out for multicollinearity e.g correlation independent
# variables age and tenure
cor.test(train$Age, train$tenure)
cor.test(train$past_3_years_bike_related_purchases, train$tenure)
cor.test(train$postcode, train$tenure)

library(psych)
library(cluster)
library(fpc)
#Describe
describeBy(train, group="Age")
describeBy(train, group="wealth_segment")
describeBy(train, group="tenure")
describeBy(train, group="tenure")
describeBy(train, group="past_3_years_bike_related_purchases")

# Start up cNORM
install.packages("cNORM")
library(cNORM)

rankByGroup(elfe, group = "Age")
model <- bestModel(normData)
printSubset(model)
plotSubset(model, type = 0)
plotSubset(model, type = 1)
# use the original elfe data
d <- prepareData(elfe)

# do a cross validation with 2 repetitions, restrict number of max terms to 10
cnorm.cv(d, max=10, repetitions = 2)
# Plots the fitted and the manifest percentiles
#Model Validation
plot <- plotPercentiles(normData, model)
# Displays a series of plots of the fitted and the manifest percentiles
plotPercentileSeries(normData, model)
plotRaw(normData, model, group="group")
plotNorm(normData, model, group="group", minNorm = 25, maxNorm = 75)
plotDerivative(model, minAge=1, maxAge=6, minNorm=20, maxNorm=80)
getNormCurve(50, model, minAge = 2, maxAge = 5, step = 0.25, minRaw = 0, maxRaw = 28)
plotNormCurves(model, normList = c(30, 40, 50, 60, 70), minAge = 2, maxAge = 5, step = 0.1, minRaw = 0, maxRaw = 28)
predictNorm(15, 4.7, model, minNorm = 25, maxNorm = 75)
predictRaw(55, 4.5, model$coefficients, minRaw = 0, maxRaw = 28)


#Pearson's product-moment correlation

#data:  train$Age and train$tenure
#t = -54.946, df = 20036, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.3738438 -0.3497774
#sample estimates:
#       cor
#-0.3618708
#This result shows a negative cor, meaning that increase in age implies
#reduction in tenure, which reasonable.
#  Point four:
#transformation of required data  ensuring that the data types are appropriate
#and rolling data up to an aggregated level. Or, joining in already aggregated
#ABS data at a geographic level to create additional variables.
#
#  Point five:
#  Document assumptions, limitations and exclusions for the data; as well as
#  how you would further improve in the next stage if there was additional time
#  to address assumptions and remove limitations.
#
 #Convert variables to different types (train and test data sets)
#for Train_comb
train$wealth_segment = as.factor(train$wealth_segment)
train$owns_car = as.factor(train$owns_car)
train$gender = as.factor(train$gender)
train$deceased_indicator = as.factor(train$deceased_indicator)
train$online_order = as.factor(train$online_order)
train$order_status = as.factor(train$order_status)
train$product_class = as.factor(train$product_class)
train$product_size = as.factor(train$product_size)
#Converting to integer
train$Age <- as.integer(train$Age)
train$postcode <- as.integer(train$postcode)
train$property_valuation <- as.integer(train$property_valuation)
train$past_3_years_bike_related_purchases <- as.integer(train$past_3_years_bike_related_purchases)
#for Test
#Converting to factor
test$wealth_segment <- as.factor(test$wealth_segment)
test$owns_car <- as.factor(test$owns_car)
test$gender <- as.factor(test$gender)
test$deceased_indicator <- as.factor(test$deceased_indicator)
#Converting to integer
test$Age <- as.integer(test$Age)
test$postcode <- as.integer(test$postcode)
test$property_valuation <- as.integer(test$property_valuation)
test$Value <- as.integer(test$Value)
test$Rank <- as.integer(test$Rank)
test$past_3_years_bike_related_purchases <- as.integer(test$past_3_years_bike_related_purchases)

#for Test
#Converting to factor
test$wealth_segment <- as.factor(test$wealth_segment)
test$owns_car <- as.factor(test$owns_car)
test$gender <- as.factor(test$gender)
test$deceased_indicator <- as.factor(test$deceased_indicator)
#Converting to integer
test$Age <- as.integer(test$Age)
test$postcode <- as.integer(test$postcode)
test$property_valuation <- as.integer(test$property_valuation)
test$Value <- as.integer(test$Value)
test$Rank <- as.integer(test$Rank)
test$past_3_years_bike_related_purchases <- as.integer(test$past_3_years_bike_related_purchases)
str(test)
str(train)
glimpse(test)
glimpse(train)

library(dplyr)
#Make full dataset
#This line of code separates the train from test dataset
train$key <- "train"
test$key <- "test"
#Full dataset
full <- full_join(test, train)
#Before imputation of NAs, form new datasets
train <- full[full$key=='train', ]
test <- full[full$key=='test', ]
#Basic one by one conversion
full$wealth_segment = as.factor(full$wealth_segment)
full$owns_car = as.factor(full$owns_car)
full$gender = as.factor(full$gender)
full$deceased_indicator = as.factor(full$deceased_indicator)
full$online_order = as.factor(full$online_order)
full$order_status = as.factor(full$order_status)
full$product_class = as.factor(full$product_class)
full$product_size = as.factor(full$product_size)

#convert to numeric
full$wealth_segment <- as.numeric(full$wealth_segment)
full$owns_car <- as.numeric(full$owns_car)
full$gender <- as.numeric(full$gender)
full$online_order <- as.numeric(full$online_order)
full$order_status <- as.numeric(full$order_status)

#convert to integer
full$wealth_segment <- as.integer(full$wealth_segment)
full$owns_car <- as.integer(full$owns_car)
full$gender <- as.integer(full$gender)
full$online_order <- as.integer(full$online_order)
full$order_status <- as.integer(full$order_status)
#Data Exploration
str(full)
#21510 obs. of  37 variables
#create numeric factors out of the two levels in owns_car, three levels in wealth_segment
full["owns_car"] <- lapply(full["owns_car"], factor, levels = c("Yes", "No"), label = c(1,0))

full["gender"] <- lapply(full["gender"], factor, levels = c("Male", "Female"), label = c(1,0))
full["deceased_indicator"] <- lapply(full["deceased_indicator"], factor, levels = c("N", "Y"), label = c(0,1))

full["online_order"] <- lapply(full["online_order"], factor, levels = c("FALSE", "TRUE"), label = c(0,1))

full["order_status"] <- lapply(full["order_status"], factor, levels = c("Cancelled", "Approved"), label = c(0,1))


full["wealth_segment"] <- lapply(full["wealth_segment"], factor, levels = c("Mass Customer", "Affluent Customer", "High Net Worth"), label = c(0,1,2))

#Exploring Data sets
str(full)
#21510 obs. of  37 variables
str(train)
#20510 obs. of  30 variables:
str(test)
#1000 obs. of  24 variables
library(skimr)
skim(full)
#There are:
# 10 Variable type:character (address with 32 mv, brand 1707 mv,
# country 32 mv, deceased_indicator 6 mv, first_name 6 mv,
# job_industry_category 6 mv, job_title 2589 mv,last_name 684 mv,
# product_line 1707 mv, state 32 mv)
# 2 Variable type:factor
#product_class with 1707 mv, product_size 1707 mv
# 11 Variable type:integer
# Age 1418 mv, gender 6mv, online_order 1870 mv,  order_status 1510 mv
# owns_car  6 mv, past_3_years_bike_related_purchases 6 mv
# postcode 32 mv,  property_valuation 32 mv,
#  Rank 20510 mv, Value 20510 mv, wealth_segment 6 mv
# 12 Variable type:numeric
#  ...17 20510 mv,  ...18 20510 mv ,...19  20510 mv, ...20 20510 mv, ...21 20510 mv,
# customer_id 1000 mv, list_price 1510 mv, product_first_sold_date 1707 mv,
# product_id 1510 mv, standard_cost 1707 mv, tenure 463 mv, transaction_id  1510 mv
# 1 Variable type:POSIXct
# transaction_date    1510 mv
#Exploring numeric variables
summary(full$past_3_years_bike_related_purchases)
summary(full$Age)
summary(full$property_valuation)
summary(full$Value)
summary(full$owns_car)
summary(full$wealth_segment)
summary(full$gender)
summary(full$Rank)
#Exploring Data sets (All Numeric Variables)
Num_summary <- summary(full[c("past_3_years_bike_related_purchases", "Age", "property_valuation",
               "Rank", "postcode", "wealth_segment", "owns_car", "Value", "gender")])
str(Num_summary)
#Measuring the central tendency - mean and median (inside summary results)
mean()
median()

#Measuring spread - quartiles and the five-number summary
#(also inside summary results)
range(full$property_valuation)
diff(range(full$property_valuation))
IQR(full$property_valuation)
quantile(full$property_valuation)
quantile(full$property_valuation, probs = c(0.01, 0.99))
quantile(full$property_valuation, seq(from = 0, to = 1, by = 0.20))
#Visualizing numeric variables -
##Create plots: boxplots
boxplot(full$property_valuation, main="Boxplot of full property_valuation ",
        ylab="property_valuation ($)")
#Create plots: hist, barplots, etc.
hist(full$Age)
hist(full$past_3_years_bike_related_purchases)
hist(full$postcode)
hist(full$property_valuation)
hist(full$Value)
hist(full$Rank)
hist(full$wealth_segment)
hist(imputed$property_valuation, main = "Histogram of full property_valuation",
     xlab = "property_valuation($)")

# variance and standard deviation
var(full$Value)
sd(full$Value)

#Exploring categorical variables
table(full$wealth_segment)
table(full$gender)
prop.table(table(full$gender))

#Exploring relationships between variables
#Visualizing relationships - scatterplots
plot(x = full$property_valuation, y = full$past_3_years_bike_related_purchases,
     main = "property_valuation vs. past_3_years_bike_related_purchases",
     xlab = "property_valuation (mi.)", ylab = "past_3_years_bike_related_purchases ($)")

#Correlation test between response variables and the target variable
cor.test(full$property_valuation, full$past_3_years_bike_related_purchases)
cor.test(full$property_valuation, full$Age)
cor.test(full$property_valuation, full$owns_car)
cor.test(full$property_valuation, full$Value)
cor.test(full$property_valuation, full$Rank)
cor.test(full$property_valuation, full$postcode)
cor.test(full$property_valuation, full$wealth_segment)
cor.test(full$property_valuation, full$gender)
cor.test(full$property_valuation, full$online_order)
cor.test(full$property_valuation, full$order_status)

#correlation between variables
library(GGally)

#Generating frequency tables
#table
table(full$wealth_segment)
#2- way cross tabulation
install.packages("gmodels")
library(gmodels)
CrossTable(full$property_valuation, full$past_3_years_bike_related_purchases)

#Sample: samplying 100 of the full
full[sample(1:nrow(full), 100, replace = FALSE), ]

#Remove duplicate
set.seed(150)
full
full <- unique(full)

#Find class level count average and sum in r
tapply(full$past_3_years_bike_related_purchases, full$property_valuation, sum)
tapply(full$past_3_years_bike_related_purchases, full$property_valuation, mean)

#Recognize and deal with missing values and outliers
is.na(full)
sum(is.na(full))
#169588 in all
mean(full, na.rm = TRUE)
str(full)

#saving data in the current working directory
#Couldn't locate it yet??
full_Data  <- tempfile("full", fileext = ".rds")
saveRDS(full, full_Data)
readRDS(full_Data)
#To load the saved data
load("full_Data")
#listing all data and functions available in working directory
ls()
#removing
rm(Train_1, Train_2, Train_3)
#removing all
rm(list=ls())



