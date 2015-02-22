#this script creates a tidy data set out of the files that have been
#unzipped from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#the program runs out of the working directory and needs the folder structure preservered in order to 
#work. This program has only been tested on windows and my not work on a unix opperating system
#because of different conventions for path names.
#This program needs to be run in the same directory as the "UCI HAR Dataset" folder is contianed.

#These are the libraries that we need to load into the environment in order to run
#this program
library(data.table)
library(dplyr)
library(reshape)

#main is the core function in run_analysis it is responsible for running the
#sub functions which drive each of the underlying tasks.
main <- function () {
    #these are the relative paths of each of the files
    X_test_file <- ".//UCI HAR Dataset//test//X_test.txt"
    y_test_file <- ".//UCI HAR Dataset//test//y_test.txt"
    subject_test_file <- ".//UCI HAR Dataset//test//subject_test.txt"
    X_train_file <- ".//UCI HAR Dataset//train//X_train.txt"
    y_train_file <- ".//UCI HAR Dataset//train//y_train.txt"
    subject_train_file <- ".//UCI HAR Dataset//train//subject_train.txt"
    activity_labels_file <- ".//UCI HAR Dataset//activity_labels.txt"
    features_file <- ".//UCI HAR Dataset//features.txt"
#these fucntion calls load all of the data that will be used during the program
    X_test  <- openFiles(X_test_file)
    y_test  <- openFiles(y_test_file)
    subject_test <- openFiles(subject_test_file) 
    X_train  <- openFiles(X_train_file)
    y_train <- openFiles(y_train_file)
    subject_train  <- openFiles(subject_train_file)
    activity_labels <- openFiles(activity_labels_file)
    features <- openFiles(features_file)
#this fucntion call combines all of the x, y and subject data tables into a single data table
#with a new column that describes if the data came from a test or a training source
    singleTable <- mergeDataFiles(X_test, y_test, subject_test, X_train, y_train, subject_train)
#this funcion call limits the number of columns the data has to the ones we are interested in
#and then applys column headers to the data table
    singleTable <- appropriateDataIdentification(singleTable, features)
#this function call relables the y values (the activitys) with a user friendly termonology
    singleTable <- relabeledData(singleTable, activity_labels)
#this function takes the tidy data table and summarizes it by subject and activity
    summarizedData <- summarizeData(singleTable)
#this function writes the two tables to a two files for further consumption
    output(singleTable, summarizedData)
}

#openFiles is a function that given a path will open a file that has no header and is
#tab delimited and return the data as a data table
openFiles <- function (filePath) {
    fileContents <- data.table(read.table(filePath, header=FALSE))
    fileContents
}

#mergeDataFiles is a function that merges all off the data files into a single table to be further
#analyzed. this function also creates new first column that identifies a record as "Test" or 
#"Training" so that the original datasets can be identified
mergeDataFiles <- function (X_test, y_test, subject_test, X_train, y_train, subject_train) {
    setnames(y_test,"V1", "Activity Code")
    setnames(y_train,"V1", "Activity Code")
    setnames(subject_test, "V1", "Subject")
    setnames(subject_train, "V1", "Subject")
    testID <- data.table(rep("Test", dim(subject_test)[1]))
    setnames(testID, "V1", "Group")
    trainingID <- data.table(rep("Training", dim(subject_train)[1]))
    setnames(trainingID, "V1", "Group")
    testdata <- cbind(testID, subject_test, y_test, X_test)
    trainingdata <- cbind(trainingID, subject_train, y_train, X_train)
    singleTable <- rbind(testdata, trainingdata)
    singleTable
}

#appropriateDataIdentification identifies or creates the appropriate columns to be brought forward
appropriateDataIdentification <- function (singleTable, features) {
    setnames(singleTable,names(singleTable),c("Group", "Subject", "Activity_Code", as.vector(features$V2)))
    singleTable <- select(singleTable, Group, Subject, Activity_Code, contains("-mean()", ignore.case=TRUE), contains("-std()", ignore.case=TRUE))
    singleTable
}

#relabledData provides friendly names to elements of the data
relabeledData <- function(singleTable, activity_labels) {
    setnames(activity_labels, names(activity_labels), c("Activity_Code", "Activity_Name"))
    singleTable <- left_join(singleTable, activity_labels, by = c("Activity_Code" = "Activity_Code"))
    singleTable
}

#summarizeData provides a copy of the data that is aggregrated to show the average mean and standard
#deviation for each metric for each subject doing each activity.
summarizeData <- function(singleTable) {
    singleTable <- melt(singleTable, id =c("Group","Subject", "Activity_Code", "Activity_Name"))
    names(singleTable) <- c("Group","Subject", "Activity_Code", "Activity_Name", "Measure_Name", "Value")
    singleTable <- group_by(singleTable, Activity_Name, Subject, Measure_Name)
    summarizedData <- summarize(singleTable, Mean_Value = mean(Value))
    summarizedData
}

#output generates the output for the program
output <- function(singleTable, summarizedData) {
    singleTableOutput <- ".//All Data.txt"
    summarizedDataOutput <- ".//Summarized UCI HAR Dataset.txt"
    write.table(singleTable, singleTableOutput, row.names=FALSE)
    write.table(summarizedData, summarizedDataOutput, row.names=FALSE)
}

main()