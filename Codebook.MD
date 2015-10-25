---
title: "Codebook"
author: "Sanjay Sharma"
date: "October 26, 2015"
---

# run_analysis.R does the following :

1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement. 
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Setting Working Directory using 

Setwd()

#loading features and activity labels

features - contains the data from features.txt
activity - conatins the data from activity.txt
activity_levels - levels are set using a vector
activity_labels - these are read from activity dataset

#loading the test dataset and adding labels

test_sub - contains the test subjects and 1st column is renamed to Subject
test_lab - test labels are loaded and 1st column is renamed to Activity
test - test data is loaded and variables are named using data from features

#loading the training dataset and adding labels

train_sub - contains the train subjects and 1st column is renamed to Subject
train_lab - train labels are loaded and 1st column is renamed to Activity
train - train data is loaded and variables are named using data from features

# 1. Merge the training and the test sets to create one data set.

test_bind  - binds the variables test_sub,test_lab,test
train_bind - binds the variables train_sub,train_lab,train
total_data - binds the datasets test_bind,train_bind

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

total_data - get rid of any duplicate columns
only_sd_mean - using the contains() helper funtion of the select(dplyr) function we subset the data

# 3. Use descriptive activity names to name the activities in the data set

final_data - using mutate and cut() function we assign descriptive names to the Activity 

# 4. Appropriately label the data set with descriptive activity names.

names(final_data) - using gsub() function we assigned descriptive names to the dataset

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

tidy_data <- using aggregate() function we get the average of each variable for each activity and each subject. 
finally using write.table tidydata.txt dataset was created.
