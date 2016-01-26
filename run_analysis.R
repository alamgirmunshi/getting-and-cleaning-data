############################################################################################################################
##
## Coursera Getting and Cleaning Data Course Project                                                                       
## Alamgir Munshi
## 2015-25-2015                                                                                                              
##
## run_Analysis.R File Description:
##
## This script will perform the following steps on the UCI HAR Dataset downloaded from 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## Task 1. Merge the training and the test sets to create one data set.
## Task 2. Extract only the measurements on the mean and standard deviation for each measurement. 
## Task 3. Use descriptive activity names to name the activities in the data set
## Task 4. Appropriately label the data set with descriptive activity names. 
## Task 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##
###########################################################################################################################

## Clean up workspace
rm(list=ls())

## Task 1. Merge the training and the test sets to create one data set.
## set working directory to the location where the UCI HAR Dataset was unzipped
setwd("E:/DataScience/Course3/Week4/UCI HAR Dataset")

## Read in the data from files
features     = read.table("./features.txt",header=FALSE) #imports features.txt
activityLabel = read.table("./activity_labels.txt",header=FALSE) #imports activity_labels.txt
subjectTrain = read.table("./train/subject_train.txt",header=FALSE) #imports subject_train.txt
xTrain       = read.table("./train/x_train.txt",header=FALSE) #imports x_train.txt
yTrain       = read.table("./train/y_train.txt",header=FALSE) #imports y_train.txt

## Assigin column names to the data imported from given text file
colnames(activityLabel)  = c("activityId","activityLabel")
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] 
colnames(yTrain)        = "activityId"

## Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)

## Read in the test data
subjectTest = read.table("./test/subject_test.txt",header=FALSE) #imports subject_test.txt
xTest       = read.table("./test/x_test.txt",header=FALSE) #imports x_test.txt
yTest       = read.table("./test/y_test.txt",header=FALSE) #imports y_test.txt

## Assign column names to the test data imported above
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "activityId"


## Create the final test set by merging the xTest, yTest and subjectTest data
testDataset = cbind(yTest,subjectTest,xTest)


## Combine training and test data to create a final data set
finalDataset = rbind(trainingData,testDataset)

## Create a vector for the column names from the finalDataset, which will be used
## to select the desired mean() & stddev() columns
colNames  = colnames(finalDataset) 

## Task 2. Extract only the measurements on the mean and standard deviation for each measurement. 
## Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

## Subset finalDataset table based on the logicalVector to keep only desired columns
finalDataset = finalDataset[logicalVector==TRUE]

## Task 3. Use descriptive activity names to name the activities in the data set
## Merge the finalDataset set with the acitivityType table to include descriptive activity names
finalDataset = merge(finalDataset,activityLabel,by="activityId",all.x=TRUE)

## Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalDataset) 

# Task 4. Appropriately label the data set with descriptive activity names. 
# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

## Reassigning the new descriptive column names to the finalDataset set
colnames(finalDataset) = colNames

## Task 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
## Create a new table, finalDatasetNoactivityLabel without the activityLabel column
finalDatasetNoactivityLabel  = finalDataset[,names(finalDataset) != "activityLabel"]

## Summarizing the finalDatasetNoactivityLabel table to include just the mean of each variable for each activity and each subject
tidyDataset    = aggregate(finalDatasetNoactivityLabel[,names(finalDatasetNoactivityLabel) != c("activityId","subjectId")],by=list(activityId=finalDatasetNoactivityLabel$activityId,subjectId = finalDatasetNoactivityLabel$subjectId),mean)

## Merging the tidyDataset with activityLabel to include descriptive acitvity names
tidyDataset    = merge(tidyDataset,activityLabel,by="activityId",all.x=TRUE)

## Export the tidyDataset set 
write.table(tidyDataset, "./tidyData.txt",row.names=TRUE,sep="\t")