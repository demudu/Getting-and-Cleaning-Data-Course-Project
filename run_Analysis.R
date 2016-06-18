# Peer Graded Assignment: Getting and Cleaning Data Course Project
### by Jeff Leek, PhD, Roger D. Peng, PhD, Brian Caffo, PhD
### Getting and Cleaning Data Course Project

### data for the project: 
### https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

### TASKs: 

### 1.You should create one R script called run_analysis.R that does the following. 
### 2. Merges the training and the test sets to create one data set.
### 3. Extracts only the measurements on the mean and standard deviation for each measurement. 
### 4. Uses descriptive activity names to name the activities in the data set
### 5. Appropriately labels the data set with descriptive variable names. 
### 6. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

### TASK 1,2:
CheckDir <- function(dirname) {
  if (!file.exists(dirname)) {  ## check to see if the directory exist
    dir.create(dirname)         ## will create a directory if it doesn't exist
  }
}

CheckDir("AccelerometerData")         ## create direcory called 'dataCousera' to download data from url given
setwd("./AccelerometerData")          ## set working directory to 'dataCousera' directory

                                ## obtain the url for downloading data from
url <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


destfile <- "./dataset.zip"     ## name of the file for the downloaded file to be stored

download.file(url, destfile) ## download the file to destination file


unzip(destfile, files = NULL, list = FALSE, overwrite = TRUE, ## unzip the ziped file to dataCousera directory
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

                                 ## Read the data from training set file
library(data.table)
train = read.table("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
train[,562] = read.table("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
train[,563] = read.table("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

                                 ## Read the data from testing set file
test = read.table("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
test[,562] = read.table("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
test[,563] = read.table("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

myData = rbind(train, test)  ## combined training and testing data into merged one data set

### TASK 4,5:

features <- read.table("./UCI HAR Dataset/features.txt",sep="") ## names of 561 variables
varnames <- features[,2]                                        ## extract names of the variable to varnames
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2] ## activity labels
colnames(one_dataset)<-c(as.character(varnames),"Activity", "Subject") ## naming variables testing data
one_dataset[,562] <- activity_labels[one_dataset[,562]]                ## assin the activity lables for each record

### TASK 3:
measurementInterested <- grep("mean\\(\\)|std\\(\\)", features[, 2])  ## choose variables concerning mean and std

cleandata <- one_dataset[, measurementInterested]                     ## create cleandata with variable that we are interested
cleandata$Activity <- one_dataset$Activity ## add the subject and activity lables to the cleandata
cleandata$Subject <-  one_dataset$Subject ## add the subject and activity lables to the cleandata


names(cleandata) <- gsub("\\(\\)", "", names(cleandata)) # brackets "()" removed from the columnames
names(cleandata) <- gsub("-", "", names(cleandata)) # dashes "-" removed from the columnames 
names(cleandata) <- gsub("mean", "Mean", names(cleandata)) # change mean to Mean
names(cleandata) <- gsub("std", "Std", names(cleandata)) # capitalize std to Std

### Task 5
require(plyr)
library(reshape2)
attach(cleandata)
##setting the id variable and measurable variable or non id variable
tidydata <- melt(cleandata, id=c("Subject","Activity"), measure.vars=names(cleandata[,1:66]) )

## taking means of each variable for each subject by activity                 
tidy <- ddply(cleandata, c("Subject", "Activity"), function(x) colMeans(x[,1:66]))

## writing the clean data to tidydata,txt
write.table(tidy, "tidydata.txt", row.name=FALSE) #write tidy data to tidydata.txt





