################################################################################
# run_analysis.R by aurelius2015 (github.com)
# Peer Assessments / Getting and Cleaning Data Course Project
################################################################################

# Raw Data source:
# https://d396qusza40orc.cloudfront.net/
#       getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# You should create one R script called run_analysis.R that does the following. 
# 1.	Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard deviation for 
#       each measurement. 
# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names. 
# 5.	From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.

#remove everything in the working environment
rm(list=ls())

################################################################################
# 1.	Merges the training and the test sets to create one data set.
################################################################################

# Download Raw Data source:
# https://d396qusza40orc.cloudfront.net/
#       getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# getdata_projectfiles_UCI HAR Dataset.zip is downloaded and put in the 
# following  directory: ~/R/3-Getting and Cleaning Data/Project
# After extracting the zip file, UCI HAR Dataset folder is put under the folder
# ~/R/3-Getting and Cleaning Data/Project
# 
# Folder structure is as follows:
# ~\R\3-Getting and Cleaning Data\Project\UCI HAR Dataset
# ~\R\3-Getting and Cleaning Data\Project\UCI HAR Dataset\test
# ~\R\3-Getting and Cleaning Data\Project\UCI HAR Dataset\test\Inertial Signals
# ~\R\3-Getting and Cleaning Data\Project\UCI HAR Dataset\train
# ~\R\3-Getting and Cleaning Data\Project\UCI HAR Dataset\train\Inertial Signals

################################################################################
# Set working directory to ~\R\3-Getting and Cleaning Data\Project
# \UCI HAR Dataset
setwd("~/R/3-Getting and Cleaning Data/Project/UCI HAR Dataset")

# read table from "activity_labels.txt" (for description of activities)
activity_labels <- read.table("activity_labels.txt", header = FALSE)
Numeral <- activity_labels$V1
Activity <- as.character(activity_labels$V2)

# read table from "features.txt" (for description of variables)
features <- read.table("features.txt", header = FALSE)

################################################################################
# 2.  Extracts only the measurements on the mean and standard deviation for 
#       each measurement.
################################################################################

# regular expression search for mean and Mean
mean_index <- sort(c(grep("mean", features$V2), grep("Mean", features$V2)))

# regular expression search for std and Std
std_index <- sort(c(grep("std", features$V2), grep("Std", features$V2)))


################################################################################

# Set working directory to ~\R\3-Getting and Cleaning Data\Project
# \UCI HAR Dataset\train
setwd("~/R/3-Getting and Cleaning Data/Project/UCI HAR Dataset/train")

# read table from "subject_train.txt"
subject_train <- read.table("subject_train.txt", header = FALSE)

# read table from "x_train.txt"
variable_train <- read.table("X_train.txt", header = FALSE)

# extract only mean and standard deviation columns of each measurement
mean_variable_train <- variable_train[,mean_index]
std_variable_train <- variable_train[,std_index]

# read table from "y_train.txt"
activity_train <- read.table("y_train.txt", header = FALSE)

# make a data frame to designate "train" type
type_train <- data.frame(y=1:(dim(activity_train)[1]),type="train")

################################################################################
# 3.  Uses descriptive activity names to name the activities in the data set
# Activities # and descriptions:
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING
# gsub each and every one of the activity code to descriptive names
################################################################################
#activity_train$V1 <- gsub(1, "WALKING", activity_train$V1)
#activity_train$V1 <- gsub(2, "WALKING_UPSTAIRS", activity_train$V1)
#activity_train$V1 <- gsub(3, "WALKING_DOWNSTAIRS", activity_train$V1)
#activity_train$V1 <- gsub(4, "SITTING", activity_train$V1)
#activity_train$V1 <- gsub(5, "STANDING", activity_train$V1)
#activity_train$V1 <- gsub(6, "LAYING", activity_train$V1)

for (item in Numeral)
{
    activity_train$V1 <- gsub(item, Activity [item], activity_train$V1)
}

################################################################################

# combine columns subject_train, X_train, y_train and assign to train
train <- cbind(Type=type_train[,2],subject_train, activity_train, 
               variable_train)
mean_train <- cbind(Type=type_train[,2],subject_train, activity_train, 
                    mean_variable_train)
std_train <- cbind(Type=type_train[,2],subject_train, activity_train, 
                   std_variable_train)

# clean up by removing subject_train, activity_train, variable_train from 
# Dataset in workspace
rm(type_train, subject_train, activity_train, variable_train)

################################################################################
# Set working directory to ~\R\3-Getting and Cleaning Data\Project
# \UCI HAR Dataset\test
setwd("~/R/3-Getting and Cleaning Data/Project/UCI HAR Dataset/test")

# read table from "subject_test.txt"
subject_test <- read.table("subject_test.txt", header = FALSE)

# read table from "x_test.txt"
variable_test <- read.table("X_test.txt", header = FALSE)

# extract only mean and standard deviation columns of each measurement
mean_variable_test <- variable_test[,mean_index]
std_variable_test <- variable_test[,std_index]

# read table from "y_test.txt"
activity_test <- read.table("y_test.txt", header = FALSE)

# make a data frame to designate "train" type
type_test <- data.frame(x=1:(dim(activity_test)[1]),"test")

################################################################################
# 3.  Uses descriptive activity names to name the activities in the data set
# Activities # and descriptions:
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING
# gsub each and every one of the activity code to descriptive names
################################################################################
#activity_test$V1 <- gsub(1, "WALKING", activity_test$V1)
#activity_test$V1 <- gsub(2, "WALKING_UPSTAIRS", activity_test$V1)
#activity_test$V1 <- gsub(3, "WALKING_DOWNSTAIRS", activity_test$V1)
#activity_test$V1 <- gsub(4, "SITTING", activity_test$V1)
#activity_test$V1 <- gsub(5, "STANDING", activity_test$V1)
#activity_test$V1 <- gsub(6, "LAYING", activity_test$V1)

for (item in Numeral)
{
    activity_test$V1 <- gsub(item, Activity[item], activity_test$V1)
}

################################################################################

# combine columns subject_test, activity_test, variable_test and assign to test
test <- cbind(Type=type_test[,2], subject_test, activity_test, variable_test)
mean_test <- cbind(Type=type_test[,2],subject_test, activity_test, 
                   mean_variable_test)
std_test <- cbind(Type=type_test[,2],subject_test, activity_test, 
                  std_variable_test)

# clean up by removing subject_test, X_test, y_test from Dataset in workspace
rm(type_test, subject_test, activity_test, variable_test)

# combine rows train, test and assign to merged
merged_data <- rbind(train, test)
mean_data <- rbind(mean_train, mean_test)
std_data <- rbind(std_train, std_test)

# clean up by removing more from Dataset in workspace
rm(train, test)
rm(mean_train, mean_test, std_train, std_test)
rm(mean_variable_train, mean_variable_test)
rm(std_variable_train, std_variable_test)

# extracts only the measurements on the mean
#mean_column<-combined[,mean_index]

# extracts only the measurements on the std
#std_column<-combined[,std_index]


################################################################################
# 4.	Appropriately labels the data set with descriptive variable names. 
################################################################################
# get column names of dataframe "merged_data", "mean_data" and "std_data"
names_merged_data <- colnames(merged_data)
names_mean_data <- colnames(mean_data)
names_std_data <- colnames(std_data)

# replace column name [2] of dataframe "merged" with "Subject"
names_merged_data[2] <- names_mean_data[2] <- names_std_data[2] <- "Subject"

# replace column name [3] of dataframe "merged" with "Activity"
names_merged_data[3] <- names_mean_data[3] <- names_std_data[3] <- "Activity"

# replace column name [4:564] of dataframe "merged" with 
# names in second rows in the transposed matrix "features"
names_merged_data[4:length(names_merged_data)] <- t(features)[2,]
names_mean_data[4:length(names_mean_data)] <- t(features)[2,mean_index]
names_std_data[4:length(names_std_data)] <- t(features)[2,std_index]

# set column names of dataframe "combined"
colnames(merged_data) <- names_merged_data
colnames(mean_data) <- names_mean_data
colnames(std_data) <- names_std_data

# clean up by removing more from Dataset in workspace
rm(activity_labels, features)
rm(mean_index, std_index)
rm(item, names_merged_data, names_mean_data, names_std_data)

################################################################################
# 5.	From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.
################################################################################
# load "dplyr"
library("dplyr")

# convert subjects as factor then converts the results to numeric
Personnel <- as.numeric(levels(as.factor(mean_data$Subject)))

# initialize data frames df_mean and df_std
df_mean <- data.frame()
df_std  <- data.frame()

# for loop to creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.
for (i in Activity)
{
    for (j in Personnel)
    {
        # filter mean_data with mean_data$Subject == j & mean_data$Activity == i
        mean_filtered <- filter(mean_data, mean_data$Subject == j & 
                                    mean_data$Activity == i)
        
        # calculate mean column means
        mean_mean_filtered <- colMeans(mean_filtered[,4:length(mean_filtered)])
        
        # assemble mean data frame
        df_mean <- rbind(df_mean, as.data.frame(c(mean_filtered[1,2:3], 
                                                  mean_mean_filtered)))
        
        
        # filter std_data with std_data$Subject == j & std_data$Activity == i
        std_filtered  <- filter(std_data, std_data$Subject == j & 
                                    std_data$Activity == i)

        # calculate std column means        
        mean_std_filtered  <- colMeans(std_filtered [,4:length(std_filtered)])
        
        # assemble std data frame
        df_std <- rbind(df_std, as.data.frame(c(std_filtered[1,2:3],  
                                                mean_std_filtered)))
    }
}

# final second data frame of tidy data
meanstd <- cbind(df_mean, df_std[3:length(df_std)])

# write to csv file
setwd("~/R/3-Getting and Cleaning Data/Project")
# write.csv(meanstd, file = "MeanStd.csv")
write.table(meanstd, file = "MeanStd.txt", row.name=FALSE)

# clean up by removing more from Dataset in workspace
rm(mean_filtered, std_filtered, mean_mean_filtered, mean_std_filtered)
rm(i, j, Numeral, Activity, Personnel)