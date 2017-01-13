# Business Analytics
# Data Mining Cup Introduction
#
# Please note, that this script only has the nature of proposal. It provides useful functions for the steps of data mining but does not cover all possibilities.

# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
library(lubridate)

#clear environment variables
rm(list=ls())

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


##############
# Functions
##############

##############
# Feature manipulation
##############ä

get_time_diff <- function(start, end){
  timediff = as.numeric(strptime(end,format="%H:%M:%S") - strptime(start,format="%H:%M:%S"))
  return(timediff)
}

get_weekday <- function(month, day){
  month = as.character(month)
  day = as.character(day)
  month = gsub("jan","01", month)
  month = gsub("feb","02", month)
  month = gsub("mar","03", month)
  month = gsub("apr","04", month)
  month = gsub("may","05", month)
  month = gsub("jun","06", month)
  month = gsub("jul","07", month)
  month = gsub("aug","08", month)
  month = gsub("sep","09", month)
  month = gsub("oct","10", month)
  month = gsub("nov","11", month)
  month = gsub("dec","12", month)
  convdate = ISOdate("2015", month, day)
  weekday = wday(convdate, label = TRUE)
  return(weekday)
}



######################################################
# 1. Build a Team in the DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Found or join a team (size: 1-4 students)


######################################################
# 2. Load & Explore the Training Data Set
# Import data
training_data <- read.csv("DMC1/training_yzCDTHz.csv")
test_data <- read.csv("DMC1/pub_ZaGS0Z2.csv")




######################################################
# 3. Data Preparation
# (using both training and test data)
# do NOT DELETE any instances in the test data

# Create new field duration (is duration of the phonecall) Bucketize?
timedifftrain = get_time_diff(training_data$CallStart, training_data$CallEnd)
timedifftest = get_time_diff(test_data$CallStart, test_data$CallEnd)

# Find weekdays

weekdaytrain = get_weekday(training_data$LastContactMonth, training_data$LastContactDay)
weekdaytest = get_weekday(test_data$LastContactMonth, test_data$LastContactDay)

# Add timediff and weekday
training_data = data.frame(training_data, "timediff"=timedifftrain, "weekday"=weekdaytrain)
test_data = data.frame(test_data, "timediff"=timedifftest, "weekday"=weekdaytest)


# Nominal attributes

Defaultlevels = c("NonDef", "Def")
training_data$Default = factor(training_data$Default, levels=0:1, labels=Defaultlevels)
test_data$Default = factor(test_data$Default, levels=0:1, labels=Defaultlevels)


# If a nominal or ordinal column in the test data set contains more levels thanwk  the corresponding column in the training data set, you can add levels to the column in the training data set manually using the following command:
#training_data$salutation = factor(training_data$salutation, levels=c(levels(training_data$salutation), "Family"))


HHlevels = c("NonIns", "Ins")
training_data$HHInsurance = factor(training_data$HHInsurance, levels=0:1, labels=HHlevels)
test_data$HHInsurance = factor(test_data$HHInsurance, levels=0:1, labels=HHlevels)

Carlevels = c("Nonloan", "Loan")
training_data$CarLoan = factor(training_data$CarLoan, levels=0:1, labels=Carlevels)
test_data$CarLoan = factor(test_data$CarLoan, levels=0:1, labels=Carlevels)


# Manual discretization of "delivery time"
training_data$delivery_time_discret = factor(rep("NA", nrow(training_data)), levels=c("NA", "<= 5d", "> 5d"))
test_data$delivery_time_discret = factor(rep("NA", nrow(test_data)), levels=c("NA", "<= 5d", "> 5d"))

training_data$delivery_time_discret[training_data$delivery_time <= 5] = "<= 5d"
test_data$delivery_time_discret[test_data$delivery_time <= 5] = "<= 5d"

training_data$delivery_time_discret[training_data$delivery_time > 5] = "> 5d"
test_data$delivery_time_discret[test_data$delivery_time > 5] = "> 5d"



# Binning/Discretization
# install.packages("arules")
library(arules)
# equal frequency binning
equal_frequency_cuts_delivery_time = discretize(training_data$delivery_time, categories=5, method="frequency", onlycuts=TRUE)
training_data$delivery_time_discret_ef = cut(training_data$delivery_time, breaks=equal_frequency_cuts_delivery_time, ordered_result=TRUE, right=FALSE)
test_data$delivery_time_discret_ef = cut(test_data$delivery_time, breaks=equal_frequency_cuts_delivery_time, ordered_result=TRUE, right=FALSE)
table(training_data$delivery_time_discret_ef, useNA="ifany")
str(training_data)
# equal width binning: with method "interval"


# Multicollinearity
numeric_columns = c("price","tax")
# these columns also contain N/A values --> the option "pairwise.complete.obs" should be used
numeric_columns_correlation = cor(training_data[, numeric_columns], use="pairwise.complete.obs")
numeric_columns_correlation
# works for non-N/A only (remove N/A rows or fill with mean, median, etc)
high_cor_columns = findCorrelation(numeric_columns_correlation)
high_cor_columns
# "price" and "tax" are perfectly correlated --> remove "tax" column
training_data$tax = NULL
test_data$tax = NULL

colSums(is.na(training_data))

# another package for binning/discretization would be the "discretization" package
# some classifiers use built-in supervised binning, i.e. entropy-based binning



# Feature Selection
#install.packages("FSelector")
library(FSelector)

# training_data$delivery_date=NULL

# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(return_shipment ~ ., data=training_data)
weights_info_gain
weights_gain_ratio = gain.ratio(return_shipment ~ ., data=training_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 7)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "return_shipment")
formula_with_most_important_attributes
#Create formula manually
# formula_with_most_important_attributes= return_shipment~delivery_time_discret_ef+state+size+salutation+order_date_weekday

######################################################
# 4. Training & Evaluation
# 3 x 5-fold cross validation
fitCtrl = trainControl(method="repeatedcv", number=5, repeats=3)

# information about decision tree parameters
getModelInfo()$J48$parameters

# training a decision tree with specific parameters using the metric "Accuracy"
modelDT = train(formula_with_most_important_attributes, data=training_data, method="J48",
                tuneGrid=data.frame(C=c(0.1, 0.2, 0.3),M=c(2,2,2)),na.action = na.pass)

# training a decision tree, one rule and boosting models using the metric "Accuracy"
modelDT = train(formula_with_most_important_attributes, data=training_data, method="J48", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)
modelOneR = train(formula_with_most_important_attributes, data=training_data, method="OneR", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)
modelBoost = train(formula_with_most_important_attributes, data=training_data, method="LogitBoost", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)

# Show results and metrics
modelOneR
modelOneR$results


# Show decision tree
modelDT$finalModel

# Compare results of different models
res = resamples(list(dt=modelDT,oneR=modelOneR, boost=modelBoost))
summary(res)

# Show confusion matrix (in percent)
confusionMatrix(modelDT)
confusionMatrix(modelBoost)
confusionMatrix(modelOneR)


######################################################
# 5. Predict Classes in Test Data
prediction_classes = predict.train(object=modelDT, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$ID, prediction=prediction_classes)
predictions


######################################################
# 6. Export the Predictions
write.csv(predictions, file="predictions_group_name_number.csv", row.names=FALSE)


######################################################
# 7. Upload the Predictions and the Corresponding R Script on DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Maxium number of submissions: 10
#
# Possible errors that could occur:
# - Wrong column names
# - Unknown IDs (if not in Test Data)
# - Missing IDs (if in Test Data but not in Predictions)
# - Wrong file format