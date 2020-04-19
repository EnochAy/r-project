#Research Questiion: There is no significant linear relationship between the
#Response variable, y (property_variation) and the predictors, X's (Age, )
#Load datasets : training and test
library(readxl)
Train_1 <-  read_excel("KPMG_Project/Train_1.xlsx")
Train_2 <- read_excel("KPMG_Project/Train_2.xlsx")
Train_3 <- read_excel("KPMG_Project/Train_3.xlsx")
test <- read_excel("KPMG_Project/Test.xlsx")

library(dplyr)
# joining transaction and identity tables
train <- full_join(Train_2, Train_3)
train <- full_join(train, Train_1)
#Joining, by = "customer_id"

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

#Checking the structure of test and train data sets
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
#== bind_rows(Test, train)
full <- bind_rows(test, train)

#I define the function `missing_vars`,
#which I can use to get an overview of what proportion of each variable is #missing, and re-use it later if I need to.
missing_vars <- function(x) {
  var <- 0
  missing <- 0
  missing_prop <- 0
  for (i in 1:length(names(x))) {
    var[i] <- names(x)[i]
    missing[i] <- sum(is.na(x[, i]))
    missing_prop[i] <- missing[i] / nrow(x)
  }
  (missing_data <- data.frame(var = var, missing = missing, missing_prop = missing_prop) %>%
      arrange(desc(missing_prop)))
}
#Call the function on full dataset
missing_vars(full)

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

#create numeric factors out of the two levels in owns_car, three levels in wealth_segment
full["owns_car"] <- lapply(full["owns_car"], factor, levels = c("Yes", "No"), label = c(1,0))

full["gender"] <- lapply(full["gender"], factor, levels = c("Male", "Female"), label = c(1,0))
full["deceased_indicator"] <- lapply(full["deceased_indicator"], factor, levels = c("N", "Y"), label = c(0,1))

full["online_order"] <- lapply(full["online_order"], factor, levels = c("FALSE", "TRUE"), label = c(0,1))

full["order_status"] <- lapply(full["order_status"], factor, levels = c("Cancelled", "Approved"), label = c(0,1))


full["wealth_segment"] <- lapply(full["wealth_segment"], factor, levels = c("Mass Customer", "Affluent Customer", "High Net Worth"), label = c(0,1,2))


library(dplyr)
glimpse(full)
library(skimr)
skim(full)

#samplying data
full_data <- as.data.frame(full[, c("customer_id",
                                    "past_3_years_bike_related_purchases", "Age", "property_valuation",
                                    "Rank", "postcode", "wealth_segment",
                                    "owns_car", "Value", "gender")])

#Targer variable: one to predict: property_valuation
#Predictors, varaibles tat are considered as the cause for the target variable
#Imputting missing values using Hmisc
library(missForest)
library(Hmisc)

#rm(full, full_mis)

#seed 10% missing values
full_mis <- prodNA(full_data, noNA = 0.1)
summary(full_mis)
#using argImpute() which identifies the variables type and treat them accordingly.
impute_arg <- aregImpute(~ Age + past_3_years_bike_related_purchases +
                            property_valuation + wealth_segment + Value +
                            Rank + customer_id + owns_car + postcode,
                            data = full_mis, n.impute = 10, match = 'closest')
impute_arg

imputed <- impute.transcan(impute_arg, imputation=3, data=full_mis,
                           list.out=TRUE, pr=FALSE, check=FALSE)
imputed <- as.data.frame(imputed)
summary(imputed)
glimpse(imputed)

#par(mfrow=c(2,1))
plot(impute_arg)
plot(imputed)
#Checking imputed data using Hmisc
impute_arg$imputed$Age
impute_arg$imputed$past_3_years_bike_related_purchases
impute_arg$imputed$property_valuation
impute_arg$imputed$wealth_segment
impute_arg$imputed$Value
impute_arg$imputed$Rank
impute_arg$imputed$customer_id
impute_arg$imputed$owns_car
impute_arg$imputed$postcode

#converting factors to numeric
imputed$wealth_segment <- as.numeric(imputed$wealth_segment)
imputed$owns_car <- as.numeric(imputed$owns_car)
#correlation between variables

install.packages("corrplot")
library(corrplot)
corrplot(cor(housing), method="number", tl.cex=0.5)

library(GGally)
ggcorr(imputed, method = c("pairwise", "spearman"), nbreaks = 6, hjust = 0.8,
       label = TRUE, label_size = 3, color = "grey50")

#Splitting imputed data into test and train datasets
test <- imputed[1:1000,]
train <- imputed[1001:21504,]

#Subsetting Target variable, y (property_val) for train and test datasets respectively
#Saved as a vector
train_y <- train[ , 3]
test_y <- test[, 3]

#Subsetting response variables, Xs (Age, Wealth_segments, Value, postcode, past_3_years... ) #Saved as a dataframe
test_X <- test[, c(1:2, 4:9)]
train_X <- train[, c(1:2, 4:9)]


library(MASS)
#Model for train dataset, target var, y = property_valuation
#Using glm: Fitting Generalized Linear Models
#For full_mis data set,  i.e full imputted data
model_1_full <- glm(formula = property_valuation ~ Age + wealth_segment +
                postcode + past_3_years_bike_related_purchases  +
                owns_car + Value, data = imputed, family = gaussian())
model_1_full
anova(model_1_full)
summary(model_1_full)
plot(model_1_full, las = 1)      # Residuals, Fitted, ...
par(model_1_full)
#First model
#This should be for train, not full data set (poisson reg ==glm(needs MASS package/ multiple reg==lm)
#Multiple reg
model_train <- glm(formula = train_y ~ Age + wealth_segment + postcode +
                    past_3_years_bike_related_purchases + owns_car +
                    Value, data = train, family = gaussian())
model_train
anova(model_train)
summary(model_train)
lapply(model_train, class)
model_train$aic
#94542.8
head(model_train$fitted)
range(model_train$fitted)
# 4.048633 9.430038
predicted_model_train <- round(model_train$fitted)
comparison_matrix <- ftable(predicted_model_train, train$property_valuation)
accuracy <- sum(diag(comparison_matrix))/2550*100
accuracy
# 43.64706
predicted= predict(model_train, test)
sum(predicted)
#7439.978
fivenum(predicted)
accuracy(predicted, test_y)
#confusion matrix
table_mat <- table(test$property_valuation, predict_test > 0.5)
table_mat

plot(resid(model_train))


install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(full_mis$property_valuation, fitted(model_train))

predict_train <- predict(model_train, test)

predict_test <- predict(object = model_train, newdata = test, se.fit = FALSE,
                         scale = NULL, df = Inf,interval = c("none", "confidence", "prediction"),
                         level = 0.95)
predict_test
summary(predict_test)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#4.199   5.980   7.713   7.440   8.790   9.388
summary(test$property_valuation)
plot(predict_test, test$property_valuation)
#Function
sumofsquares <- function(x) {
  return(sum(x^2))
  }
#Test function
sumofsquares(1:5)

diff <- predict_test - test$property_valuation
diff
sumofsquares(diff)
#6039.888
#s result is the sum of the squares of the differences
#between predicted and actual values

#Least squares regression:  b0 + b1*x
# b0 as the intercept and b1 as the slope of the line
x <-  test$property_valuation
Y <- predict_test

#calculate our b0 and b1 from our x and Y, as follows:
b1 <- sum((x-mean(x))*(Y-mean(Y)))/sum((x-mean(x))^2)
b0 <- mean(Y)-b1*mean(x)
c(b0,b1)
#b0 = 5.5744554, b1 = 0.2531925
# plot the raw data
plot(x,Y)
# add a least squares regression line to the plot
abline(c(b0,b1),col="blue",lwd=2)

#Relative importance: relaimpo package
#tell you which variables are providing the most effect on your results
install.packages("relaimpo")
library(relaimpo)
relimp_model <- calc.relimp(model_train, type=c("lmg","last","first","pratt"), rela=TRUE)
#Response variable: train_y
#Total response variance: 8.032367
#Analysis based on 20504 observations

#6 Regressors:
#Age wealth_segment postcode past_3_years_bike_related_purchases owns_car Value
#Proportion of variance explained by model: 26.74%
#Relative importance metrics:

                                             #lmg         last        first
#Age                                 6.177088e-04 1.377733e-04 1.094821e-03
#wealth_segment                      4.622502e-04 3.576117e-04 5.636770e-04
#postcode                            9.885408e-01 9.904918e-01 9.866302e-01
#past_3_years_bike_related_purchases 3.937271e-05 1.337268e-06 7.290426e-05
#owns_car                            9.803768e-03 8.620536e-03 1.094435e-02
#Value                               5.361109e-04 3.909915e-04 6.940522e-04#
#pratt
#Age                                 3.894363e-04
#wealth_segment                      4.491470e-04
#postcode                            9.889129e-01
#past_3_years_bike_related_purchases 9.883023e-06
#owns_car                            9.716699e-03
#Value                               5.219772e-04

#Average coefficients for different model sizes:

  #1X           2Xs           3Xs
#Age                                 -1.053881e-05 -9.187868e-06 -7.831529e-06
#wealth_segment                      -4.177317e-02 -4.001670e-02 -3.827844e-02
#postcode                            -1.719414e-03 -1.718827e-03 -1.718238e-03
#past_3_years_bike_related_purchases  4.372315e-04  3.695399e-04  2.978070e-04
#owns_car                            -3.072496e-01 -3.003808e-01 -2.933809e-01
#Value                                8.068404e-02  7.601803e-02  7.165391e-02
#4Xs           5Xs           6Xs
#Age                                 -6.470144e-06 -5.104067e-06 -3.733656e-06
#wealth_segment                      -3.655630e-02 -3.484811e-02 -3.315161e-02
#postcode                            -1.717646e-03 -1.717052e-03 -1.716458e-03
#past_3_years_bike_related_purchases  2.220980e-04  1.424823e-04  5.903335e-05
#owns_car                            -2.862593e-01 -2.790251e-01 -2.716872e-01
#Value                                6.759811e-02  6.385685e-02  6.043606e-02

#
#
#
library(MASS)
step <- stepAIC(model_train, direction="both")

#Start:  AIC=94542.8
train_y ~ Age + wealth_segment + postcode + past_3_years_bike_related_purchases +
  owns_car + Value

Df Deviance    AIC
- past_3_years_bike_related_purchases  1   120652  94541
- Age                                  1   120658  94542
<none>                                     120652  94543
- wealth_segment                       1   120667  94543
- Value                                1   120669  94544
- owns_car                             1   121030  94605
- postcode                             1   164091 100846

Step:  AIC=94540.81
train_y ~ Age + wealth_segment + postcode + owns_car + Value

Df Deviance    AIC
- Age                                  1   120658  94540
<none>                                     120652  94541
- wealth_segment                       1   120667  94541
- Value                                1   120669  94542
+ past_3_years_bike_related_purchases  1   120652  94543
- owns_car                             1   121030  94603
- postcode                             1   164095 100845

Step:  AIC=94539.83
train_y ~ wealth_segment + postcode + owns_car + Value

Df Deviance    AIC
<none>                                     120658  94540
- wealth_segment                       1   120673  94541
+ Age                                  1   120652  94541
- Value                                1   120676  94541
+ past_3_years_bike_related_purchases  1   120658  94542
- owns_car                             1   121035  94602
- postcode                             1   164142 100848

Call:  glm(formula = train_y ~ wealth_segment + postcode + owns_car +
             Value, family = gaussian(), data = train)

Coefficients:
  (Intercept)  wealth_segment        postcode        owns_car           Value
13.077189       -0.033188       -0.001717       -0.271401        0.062480

Degrees of Freedom: 20503 Total (i.e. Null);  20499 Residual
Null Deviance:	    164700
Residual Deviance: 120700 	AIC: 94540


install.packages("class")
library(class)
knnModel <- knn(train = train, test = test, cl = train$property_valuation)
summary(knnModel)
#1   2   3   4   5   6   7   8   9  10  11  12
#41  35  42  46  56  67 119 181 149 136  64  64
plot(knnModel)



library(dplyr)
glimpse(predict_test)
#Comparing original property_valuation values with the predicted values
Table <- table(test$property_valuation, predict_test > 0.5)
#Miss calculation rate
miss_cal_rate <- 1-sum(diag(Table)/ sum(Table))
#Model for test
model_test <- glm(property_valuation  ~ Age + wealth_segment +
                   postcode + past_3_years_bike_related_purchases + owns_car +
                   Value, data = test, family = gaussian())
model_test
summary(model_test)
glimpse(model_test)
anova(model_test)

#Prediction
#This will then be for Test
prediction_var <- data.frame(test [,c("Age", "wealth_segment", "postcode",
                                      "past_3_years_bike_related_purchases",
                                      "owns_car", "Value")])
predict <- predict(model_test, prediction_var)
summary(predict)
glimpse(predict)
range(predict)
fivenum(predict)

model_2 <- glm(test_y ~ + wealth_segment + postcode +
                past_3_years_bike_related_purchases + owns_car + Value,
              data = test, family = gaussian())
model_2
summary(model_2)
glimpse(model_2)
#Prediction
predict_final <- predict(model_2, test, type = 'response')
summary(predict_final)
library(dplyr)
glimpse(predict_final)
final_predict <- as.data.frame(predict_final)
range(final_predict)
#Those to convert to factors and then intergers are: "gender", wealth_segment,
#deceased_indicator, owns_car, product_class,product_size#Prediction
summary(predict(model_2, test))

plot.stepfun(model_2, test)
