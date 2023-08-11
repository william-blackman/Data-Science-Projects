install.packages("tidyverse")
install.packages("reshape2")
install.packages("caret")
install.packages('randomForest')
library(tidyverse)
library(reshape2)
library(caret)

housing = read.csv('housing.csv')

head(housing)
summary(housing)

par(mfrow=c(2, 5))
colnames(housing)

#plot histograms for the count of all variables
ggplot(data = melt(housing), mapping = aes(x = value)) + geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

#fill nas in total bedrooms withe medians
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm =TRUE)

#create mean columns for bedrooms and rooms and drop total columns
housing$mean_bedrooms = housing$total_bedrooms / housing$households
housing$mean_rooms = housing$total_rooms / housing$households

drops = c('total_bedrooms', 'total_rooms')

housing = housing[ , !(names(housing) %in% drops)]

head(housing)

#use one hot encoding for ocean_proximity

dummy = dummyVars(" ~ .", data = housing)
housing = data.frame(predict(dummy, newdata=housing))
head(housing)
tail(housing)

#Simplify the Column names
colnames(housing)[8] = ">1H OCEAN"
colnames(housing)[9] = "INLAND"
colnames(housing)[10] = "ISLAND"
colnames(housing)[11] = "NEAR_BAY"
colnames(housing)[12] = "NEAR_OCEAN"
head(housing)

#put the categorical values into a new table
cat = housing[ ,8:12]
head(cat)

#scale the numerical independent variables
housing_num = housing[ , c("longitude", "latitude", "housing_median_age", "population", "households", "median_income", "mean_bedrooms", "mean_rooms")]
head(housing_num)
scaled_housing_num = scale(housing_num)
head(scaled_housing_num)

#merge the cleaned columns to one table
cleaned_housing = cbind(cat, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)

#create a test set of data
set.seed(42)
sample = sample.int(n=nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ]
test = cleaned_housing[-sample, ]

head(train)
#check to make sure we didn't lose any data
nrow(train) + nrow(test) == nrow(cleaned_housing)

#testing with the k-fold cross validation on a linear model using median income, mean rooms, and population
library('boot')
?cv.glm
#setting the glmfit argument to the linear model
glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
#running the model
k_fold_cv_error = cv.glm(cleaned_housing, glm_house, K=5)

k_fold_cv_error$delta
glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse
#prediction using this method is off by about $83,000

names(glm_house)
glm_house$coefficients

##RANDOM FOREST MODEL
library('randomForest')
?randomForest
names(train)

set.seed(42)
train_y = train[,'median_house_value']
train_x = train[,names(train) !='median_house_value']
rf_model = randomForest(train_x, y = train_y , ntree = 1000, importance = TRUE)
names(rf_model)
rf_model$importance
#out-of-bag error estimate
oob_prediction = predict(rf_model)
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
#shows that we're only off by about $48,810 when using our training data
#with 1000 decision trees it only got better by 3 dollars and took a lot longer to run. 
#Time to try another algorithm next time

#using the model with the test data & calculating error
test_y = test[,'median_house_value']
text_x = test[, names(test) !='median_house_value']
y_pred = predict(rf_model, text_x)
test_mse = mean((y_pred - test_y)^2)
test_rmse = sqrt(test_mse)
test_rmse
#improved by only about $50 with test data