# 1. Using the “airquality” dataset from R Datasets Package:
# a) Create a new object that considers the data as a time series.

## create a copy of airquality
df <- airquality

## Use Day and Month column values to create new column Date
df$Date <- as.Date(paste( df$Month, df$Day, sep = "." ), format = "%m.%d" )
  
## Create a new dataframe without Month and Day  
df1 <- df[,1:4]

## Set rownames of the new dataframe to Date
rownames(df1) <- df[,7]

## Create a timeseries out of the dataframe 
library(xts)
aq <- xts(df1, order.by=as.Date(df[,7], "%Y.%m.%d"))

# b) Create a new object from the time series where observations that have an NA in any of the variables have been removed.
# Call this new object airqualityFull.
airqualityFull <- na.exclude(aq)

# c) Create a sequence plot, a lag plot, and a histogram of the Temperature data from airqualityFull.
# What are your observations based on these plots?
plot(airqualityFull$Temp, main='Sequence plot of Temperature', col='blue', xlab='', ylab='Temperature')
hist(airqualityFull$Temp, main='Histogram of Temperature', xlab='Temperature', col='blue')
lag.plot(airqualityFull$Temp, lags = 1)


# d) Create a Q-Q plot and Q-Q line for comparison of temperature data with the normal distribution from airqualityFull. 
# What do you think about the distribution of data based on this plot?
qqnorm(airqualityFull$Temp)
qqline(airqualityFull$Temp)

# e) Create the autorocorrelation plot for temperatures in airqualityFull.
# Based on this plot, does temperature for one day depend on its temperature the day before?
?acf
acf(airqualityFull$Temp)

# f) Draw the box plots for temperature and wind variables in airqualityFull.
# Which one has outliers based on the box plot?
tdf <- data.frame(airqualityFull)
boxplot(tdf$Temp)
boxplot(tdf$Wind)

# g) Assuming the data to be normal, find the 95% confidence interval for the temperatures from airqualityFull.
e = qnorm(0.975) * sd(airqualityFull$Temp / sqrt( nrow(airqualityFull) ) )
left = mean(airqualityFull$Temp) - e
right = mean(airqualityFull$Temp) + e
print (e)

# h) Assuming the data to be normal, test to see whether if variances and means of the first 55 observations is different from the next 56 ones.
# Comment on findings for each test (are they different, why).

lapply(airqualityFull[1:55,], mean, na.rm = TRUE)
lapply(airqualityFull[1:55,], var, na.rm = TRUE)

lapply(airqualityFull[56:111,], mean, na.rm = TRUE)
lapply(airqualityFull[56:111,], var, na.rm = TRUE)

# i) Create a new object that has the same data as airqualityFull with outliers removed (using any tool you want).
outliers <- boxplot(tdf$Wind, plot=FALSE)$out
print (outliers)

## First you need find in which rows the outliers are
tdf[which(tdf$Wind %in% outliers),]

## Now you can remove the rows containing the outliers, one possible option is:
tdf <- tdf[-which(tdf$Wind %in% outliers),]

## If you check now with boxplot, you will notice that those pesky outliers are gone
boxplot(tdf$Wind)

# 2 
# Use the attached “activity2Sample.csv” dataset for this question. 
# This dataset has the accelerometer data on three axes (x, y, and z) and the actual activity taking place.
# Use KNN to create clusters in the data using the X, Y and Z variables. 
# Use the first 150 observation for training. 
# Create two models, one with k=5 and one with k=10. 
# What are the accuracies for these two models?

## read file
as <- read.csv(file = '/Users/parthasarathidas/Desktop/a2/Activity2Sample.csv')

## look at number of rows
nrow(as)

# Split into train and test 
as_train <- as[1:150,]
as_test <- as[151:190,]
as_train_labels <- as[1:150, 4]
as_test_labels <- as[151:190, 4]

as_test_pred <- knn(train=as_train, test=as_test, cl=as_train_labels, k=5)
cm = as.matrix(table(Actual=as_test_labels, Predicted=as_test_pred))
sum(diag(cm)) / length(as_test_labels)

as_test_pred <- knn(train=as_train, test=as_test, cl=as_train_labels, k=10)
cm = as.matrix(table(Actual=as_test_labels, Predicted=as_test_pred))
sum(diag(cm)) / length(as_test_labels)

# 3.
# Using the “iris” dataset:
# a) Create a KNN classifier to predict the Species of the flower based on the Sepal.Length and Sepal.Width variables. 
# Use the first 30 observations for each species in the dataset as training set, and the rest as the test set (90 observations for training, 60 observations for testing). 
# Use K=3.

## create a new dataframe with only Sepal.Length, Sepal.Width and Species
keeps <- c("Sepal.Length", "Sepal.Width", "Species")
niris <- iris[keeps]
niris

## change 'Species' to numeric
niris$Species <- as.numeric(niris$Species)

## randomize the dataset
niris$Species <- niris$Species[sample(nrow(niris))]

## import library for count() 
library(plyr)

## compute the frequency of different species
count(niris, 'Species')

## set first 30 rows as train
d <- by(niris, niris['Species'], head, n=30)

## combine to dataframe
itrain <- Reduce(rbind, d)

## reset rownames
rownames(itrain) <- 1:nrow(itrain)

## set last 20 rows as test
d <- by(niris, niris["Species"], tail, n=20)

## combine to dataframe
itest <- Reduce(rbind, d)

## reset rownames
rownames(itest) <- 1:nrow(itest)

## prepare data for model
itrain <- itrain[1:90,]
itest <- itest[1:60,]
itrain_labels <- itrain[1:90, 3]
itest_labels <- itest[1:60, 3]

## build the model
irisKnn3 <- knn(train=itrain, test=itest, cl=itrain_labels, k=3)

# b) Calculate the accuracy of the model for the predicted species in the test dataset.
cm = as.matrix(table(Actual=itest_labels, Predicted=irisKnn3))
accuracyScore <- sum(diag(cm)) / length(itest_labels)
accuracyScore

# c) Predict the species of flowers using the model from a, but changing K to take values from 1 to 6.
# Calculate accuracy of each model and plot the accuracies with respect to K. Which K has the best accuracy? 
# Is it OK to use this analysis to decide on the best-performing K? Why or why not?

## create a sequence of 1 to 6
x <- c(1,2,3,4,5,6)

## loop through k = 1,2,3,4,5,6
a = 1
for (val in x) {
  irisKnn3 <- knn(train=itrain, test=itest, cl=itrain_labels, k=val)
  cm = as.matrix(table(Actual=itest_labels, Predicted=irisKnn3))
  a[val] = sum(diag(cm)) / length(itest_labels)
}

## plot
plot(a, col='blue', xlab='k value', ylab='Accuracy', main='k vs Accuracy')
