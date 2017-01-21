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
#clear environment variables
rm(list=ls())

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


##############
# Functions
##############


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

######################################################
# 3. Data Preparation
# (using both training and test data)
# do NOT DELETE any instances in the test data

# Nominal attributes

training_data$engine_type = factor(training_data$engine_type)
test_data$engine_type = factor(test_data$engine_type)

training_data$vehicle_type = factor(training_data$vehicle_type)
test_data$vehicle_type = factor(test_data$vehicle_type)
# Binning/Discretization

# equal frequency binning


# Feature Selection

# DROP COLUMNS

# training_data = training_data[-c(16:63)]

# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(defect ~ .^2 , data=training_data)
weights_info_gain
weights_gain_ratio = gain.ratio(defect ~ .^2 , data=training_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 70)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "defect")
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
                 #tuneGrid=xgb.grid,
                 verbose=T,
                 metric="Accuracy",
                 nthread = 26,
                 na.action = na.pass
)

print(xgb_tune$finalModel)
print(max(xgb_tune$results[8]))

######################################################
# 5. Predict Classes in Test Data
prediction_classes = predict.train(object=xgb_tune, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$CarInsurance, prediction=prediction_classes)
predictions

######################################################
# 6. Export the Predictions
write.csv(predictions, file="prediction_dmc2_dataRtists.csv", row.names=FALSE)

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