## Coursera Getting and Cleaning Data Course Project
## Sanjay Sharma
# run_analysis.R does the following :

# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Setting Working Directory

setwd("C:/Users/sanjay/Documents/UCI HAR Dataset")

#loading features and activity labels


features <- read.table("features.txt")
activity <- read.table("activity_labels.txt")
activity_levels <- c(-Inf,1,2,3,4,5,6)
activity_labels <- activity[,2]

#loading the test dataset and adding labels

test_sub <- read.table("test/subject_test.txt",stringsAsFactors =FALSE)
test_sub <- rename(test_sub,Subject=V1)
test_lab <- read.table("test/y_test.txt",stringsAsFactors =FALSE)
test_lab <- rename(test_lab,Activity=V1)
test<- read.table("test/X_test.txt",stringsAsFactors =FALSE)
names(test) <- features[,"V2"]

#loading the training dataset and adding labels

train_sub <- read.table("train/subject_train.txt",stringsAsFactors =FALSE)
train_sub <- rename(train_sub,Subject=V1)
train_lab <- read.table("train/y_train.txt",stringsAsFactors =FALSE)
train_lab <- rename(train_lab,Activity=V1)
train<- read.table("train/X_train.txt",stringsAsFactors =FALSE)
names(train) <- features[,"V2"]

# 1. Merge the training and the test sets to create one data set.

test_bind <- cbind(test_sub,test_lab,test)
train_bind <- cbind(train_sub,train_lab,train)
total_data <- rbind(test_bind,train_bind)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

total_data <- total_data[,!duplicated(colnames(total_data))]
only_sd_mean <- select(total_data,Subject,Activity,contains("mean()"),contains("std()"))


# 3. Use descriptive activity names to name the activities in the data set
final_data <- mutate(only_sd_mean,Activity=cut(Activity,activity_levels,activity_labels))


# 4. Appropriately label the data set with descriptive activity names. 

names(final_data )<-gsub("^t", "time", names(final_data))
names(final_data)<-gsub("^f", "frequency", names(final_data))
names(final_data)<-gsub("Acc", "Accelerometer", names(final_data))
names(final_data)<-gsub("Gyro", "Gyroscope", names(final_data))
names(final_data)<-gsub("Mag", "Magnitude", names(final_data))
names(final_data)<-gsub("BodyBody", "Body", names(final_data))


# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

tidy_data <-aggregate(. ~Subject + Activity, final_data, mean)
tidy_data <-tidy_data[order(tidy_data$Subject,tidy_data$Activity),]
write.table(tidy_data, file = "tidydata.txt",row.name=FALSE)



