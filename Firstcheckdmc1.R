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
start_time = Sys.time()

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
training_data <- read.csv("DMC1/training_yzCDTHz.csv")
test_data <- read.csv("DMC1/pub_ZaGS0Z2.csv")

# Remove ID field
training_data = training_data[, -1]
# test_data = test_data[, -1]


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
training_data = data.frame(training_data, "timediff"=timedifftrain, "weekday"=as.factor(weekdaytrain))
test_data = data.frame(test_data, "timediff"=timedifftest, "weekday"=as.factor(weekdaytest))
# Combining columns
training_data = data.frame(training_data, "contactrattio"=training_data$PrevAttempts / training_data$NoOfContacts)
test_data = data.frame(test_data, "contactrattio"=training_data$PrevAttempts / training_data$NoOfContacts)


# Nominal attributes

# to_nominalize = c("Default", "HHInsurance", "CarLoan")
# nomdata = make_nominal(training_data, test_data, to_nominalize)

# 
CarInsLevel =c("null", "eins")
training_data$CarInsurance = factor(training_data$CarInsurance, labels=CarInsLevel)
# test_data$CarInsurance = factor(test_data$CarInsurance, labels=CarInsLevel)

# training_data$CarInsurance = factor(training_data$CarInsurance)

Defaultlevels = c("NonDef", "Def")
training_data$Default = factor(training_data$Default, levels=0:1, labels=Defaultlevels)
test_data$Default = factor(test_data$Default, levels=0:1, labels=Defaultlevels)

HHlevels = c("NonIns", "Ins")
training_data$HHInsurance = factor(training_data$HHInsurance, levels=0:1, labels=HHlevels)
test_data$HHInsurance = factor(test_data$HHInsurance, levels=0:1, labels=HHlevels)

Carlevels = c("Nonloan", "Loan")
training_data$CarLoan = factor(training_data$CarLoan, levels=0:1, labels=Carlevels)
test_data$CarLoan = factor(test_data$CarLoan, levels=0:1, labels=Carlevels)

# Ordinal attributes

# to_ordinaize = c("NoOfContacts", "Age", "PrevAttempts")
# orddata = make_ordinal(training_data, test_data, to_ordinaize)

NoContracts = sort(unique(c(as.numeric(training_data$NoOfContacts), as.numeric(test_data$NoOfContacts))))
training_data$NoOfContacts = ordered(training_data$NoOfContacts, levels=NoContracts)
test_data$NoOfContacts = ordered(test_data$NoOfContacts, levels=NoContracts)

# PassedDays = sort(unique(c(as.numeric(training_data$DaysPassed), as.numeric(test_data$DaysPassed))))
# training_data$DaysPassed = ordered(training_data$DaysPassed, levels=PassedDays)
# test_data$DaysPassed = ordered(test_data$DaysPassed, levels=PassedDays)
# 
# AgeLevels = sort(unique(c(as.numeric(training_data$Age), as.numeric(test_data$Age))))
# training_data$Age = ordered(training_data$Age, levels=AgeLevels)
# test_data$Age = ordered(test_data$Age, levels=AgeLevels)

# PrevLevels = sort(unique(c(as.numeric(training_data$PrevAttempts), as.numeric(test_data$PrevAttempts))))
# training_data$PrevAttempts = ordered(training_data$PrevAttempts, levels=PrevLevels)
# test_data$PrevAttempts = ordered(test_data$PrevAttempts, levels=PrevLevels)


# Binning/Discretization

# equal frequency binning
# Age Binning
equal_frequency_cuts_age= discretize(as.numeric(training_data$Age), categories=5, method="frequency", onlycuts=TRUE)
training_data$age_discret_ef = cut(as.numeric(training_data$Age), breaks=equal_frequency_cuts_age, ordered_result=TRUE, right=FALSE)
test_data$age_discret_ef = cut(as.numeric(test_data$Age), breaks=equal_frequency_cuts_age, ordered_result=TRUE, right=FALSE)

# Timediff binning
# equal_frequency_cuts_td= discretize(as.numeric(training_data$timediff), categories=10, method="frequency", onlycuts=TRUE)
# training_data$td_discret_ef = cut(as.numeric(training_data$timediff), breaks=equal_frequency_cuts_td, ordered_result=TRUE, right=FALSE)
# test_data$td_discret_ef = cut(as.numeric(test_data$timediff), breaks=equal_frequency_cuts_td, ordered_result=TRUE, right=FALSE)
# table(training_data$td_discret_ef, useNA="ifany")
# str(training_data)

# Passed Day binning
# equal_frequency_cuts_pd= discretize(as.numeric(training_data$DaysPassed), categories=10, method="frequency", onlycuts=TRUE)
# training_data$td_discret_pd = cut(as.numeric(training_data$DaysPassed), breaks=equal_frequency_cuts_pd, ordered_result=TRUE, right=FALSE)
# test_data$td_discret_pd = cut(as.numeric(test_data$DaysPassed), breaks=equal_frequency_cuts_pd, ordered_result=TRUE, right=FALSE)
# table(training_data$td_discret_pd, useNA="ifany")
# str(training_data)
# # equal width binning: with method "interval"
# 
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
training_data$CallStart=NULL
#test_data$CallStart=NULL
training_data$CallEnd=NULL
#test_data$CallEnd=NULL
# training_data$timediff=NULL
# test_data$timediff=NULL

# Make DaysPassed
training_data$DaysPassed[training_data$DaysPassed==-1] = NA
test_data$DaysPassed[test_data$DaysPassed==-1] = NA

# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(CarInsurance ~ poly(timediff + Outcome + Age + DaysPassed + contactrattio + 
                                       Communication + PrevAttempts + HHInsurance + LastContactMonth + 
                                       Balance + CarLoan + Default + weekday + Job + NoOfContacts + 
                                       LastContactDay + Marital + Education,2) , data=training_data)
weights_info_gain
weights_gain_ratio = gain.ratio(CarInsurance ~ CarInsurance ~ (timediff + Outcome + Age + DaysPassed + contactrattio + 
                                                                Communication + PrevAttempts + HHInsurance + LastContactMonth + 
                                                                Balance + CarLoan + Default + weekday + Job + NoOfContacts + 
                                                                LastContactDay + Marital + Education)^2 , data=training_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 18)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "CarInsurance")
formula_with_most_important_attributes
#Create formula manually
# formula_with_most_important_attributes= return_shipment~delivery_time_discret_ef+state+size+salutation+order_date_weekday

######################################################
# 4. Training & Evaluation
# 10 x 10-fold cross validation


cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 5, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)


# xgb.grid <- expand.grid(nrounds = 100,
#                         eta = 0.3,
#                         max_depth = 2,
#                         subsample = 1,
#                         min_child_weight=1,
#                         gamma =  0,
#                         colsample_bytree = 0.6
#                         
# )


# cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 5, 
#                         search = "random",
#                         #summaryFunction = twoClassSummary,
#                         classProbs = TRUE,
#                         allowParallel=T)


xgb.grid <- expand.grid(nrounds =  1000,
                        eta = 0.01,
                        max_depth = 4,
                        subsample = 1,
                        min_child_weight= 0.2,
                        gamma =  0,
                        colsample_bytree = 0.4
)

xgb_tune <-train(CarInsurance ~.^2,
                 data=training_data,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=1,
                 printEveryN = 100,
                 metric="Accuracy",
                 nthread = 26,
                 na.action = na.pass
)
print(xgb_tune$finalModel)
print(max(xgb_tune$results[8]))

######################################################
# 5. Predict Classes in Test Data
prediction_classes = predict.train(object=xgb_tune, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$Id, prediction=prediction_classes)
predictions$prediction = as.character(as.numeric(predictions$prediction)-1)


######################################################
# 6. Export the Predictions
write.csv(predictions, file="prediction_dmc1_dataRtists_5.csv", row.names=FALSE)

print(Sys.time() - start_time)
