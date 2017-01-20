# Business Analytics
# Data Mining Cup Introduction
#
# Please note, that this script only has the nature of proposal. It provides useful functions for the steps of data mining but does not cover all possibilities.

# The caret package is used (http://topepo.github.io/caret/index.html)
# install.packages("caret")
# install.packages("lubridate")
# install.packages("arules")
# install.packages("FSelector")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("magrittr")

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

library(FSelector)
library(arules)
library(caret)
library(lubridate)
library(data.table)
library(dplyr)
library(magrittr)
# library(doMC)
# registerDoMC(cores = 24)
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
  weekday = wday(convdate)
  return(weekday)
}


make_ordinal <- function(train, test, columns){
  for (col in columns){
    Levels = eval(parse(text=paste0("sort(unique(c(as.numeric(training_data$",col,"), as.numeric(test_data$",col,"))))")))
    eval(parse(text=paste0("train$", col, " = ordered(training_data$", col,", levels=",Levels,")")))
    eval(parse(text=paste0("test$", col, " = ordered(test_data$",col,", levels=",Levels,")")))
  }
  return(list(train, test))
}

make_nominal <- function(train, test, columns){
  for (col in columns){
    Levels = eval(parse(text=paste0("unique(c(as.numeric(training_data$",col,"), as.numeric(test_data$",col,")))")))
    eval(parse(text=paste0("train$", col, " = factor(training_data$", col,", levels=",Levels,", labels=",Levels,")")))
    eval(parse(text=paste0("test$", col, " = factor(test_data$",col,", levels=",Levels,", labels=",Levels,")")))
  }
  return(list(train, test))
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
training_data <- read.csv("DMC2/2_ba16_dmc2_vehicle_training_data_Ar2kCbl.csv")
test_data <- read.csv("DMC2/pub_nkAt59S.csv")

# Remove ID field
training_data = training_data[, -1]
training_data = training_data[, -1]

# test_data = test_data[, -1]


######################################################
# 3. Data Preparation
# (using both training and test data)
# do NOT DELETE any instances in the test data

# Nominal attributes

# to_nominalize = c("Default", "HHInsurance", "CarLoan")
# nomdata = make_nominal(training_data, test_data, to_nominalize)


training_data$engine_type = factor(training_data$engine_type)
test_data$engine_type = factor(test_data$engine_type)

training_data$vehicle_type = factor(training_data$vehicle_type)
test_data$vehicle_type = factor(test_data$vehicle_type)

# Ordinal attributes

# to_ordinaize = c("NoOfContacts", "Age", "PrevAttempts")
# orddata = make_ordinal(training_data, test_data, to_ordinaize)

# PassedDays = sort(unique(c(as.numeric(training_data$DaysPassed), as.numeric(test_data$DaysPassed))))
# training_data$DaysPassed = ordered(training_data$DaysPassed, levels=PassedDays)
# test_data$DaysPassed = ordered(test_data$DaysPassed, levels=PassedDays)


# Binning/Discretization

# equal frequency binning

# Timediff binning
# equal_frequency_cuts_td= discretize(as.numeric(training_data$timediff), categories=10, method="frequency", onlycuts=TRUE)
# training_data$td_discret_ef = cut(as.numeric(training_data$timediff), breaks=equal_frequency_cuts_td, ordered_result=TRUE, right=FALSE)
# test_data$td_discret_ef = cut(as.numeric(test_data$timediff), breaks=equal_frequency_cuts_td, ordered_result=TRUE, right=FALSE)
# table(training_data$td_discret_ef, useNA="ifany")
# str(training_data)

# 
# Multicollinearity
# numeric_columns = c("Age", "Balance")
# # these columns also contain N/A values --> the option "pairwise.complete.obs" should be used
# numeric_columns_correlation = cor(training_data[, numeric_columns], use="pairwise.complete.obs")
# numeric_columns_correlation
# # works for non-N/A only (remove N/A rows or fill with mean, median, etc)
# high_cor_columns = findCorrelation(numeric_columns_correlation)
# high_cor_columns
# # "price" and "tax" are perfectly correlated --> remove "tax" column
# training_data$tax = NULL
# test_data$tax = NULL
# 
# colSums(is.na(training_data))

# another package for binning/discretization would be the "discretization" package
# some classifiers use built-in supervised binning, i.e. entropy-based binning



# Feature Selection


# training_data$delivery_date=NULL


# DROP COLUMNS

training_data = training_data[-c(16:63)]
test_data$timediff=NULL



# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(defect ~ .^2 , data=training_data)
weights_info_gain
weights_gain_ratio = gain.ratio(defect ~ .^2 , data=training_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 20)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "CarInsurance")
formula_with_most_important_attributes
#Create formula manually
# formula_with_most_important_attributes= return_shipment~delivery_time_discret_ef+state+size+salutation+order_date_weekday

######################################################
# 4. Training & Evaluation
# 10 x 10-fold cross validation


cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 3, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 10*(10:100),
                        eta = seq(0.1, 0.9, by = 0.05),
                        max_depth = c(4,6,8),
                        subsample = 0.6,
                        min_child_weight=1,
                        gamma =  seq(0, 0.9, by = 0.1),
                        colsample_bytree = 0.8
                        
)
xgb_tune <-train(formula_with_most_important_attributes,
                 data=training_data,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="Accuracy",
                 nthread = 26,
                 na.action = na.pass
)


######################################################
# 5. Predict Classes in Test Data
prediction_classes = predict.train(object=xgb_tune, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$CarInsurance, prediction=prediction_classes)
predictions


######################################################
# 6. Export the Predictions
write.csv(predictions, file="prediction_dmc1_dataRtists", row.names=FALSE)


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