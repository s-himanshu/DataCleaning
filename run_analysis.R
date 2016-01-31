# 
# Author: Himanshu
###############################################################################

# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
# The goal is to prepare tidy data that can be used for later analysis. 
# You will be graded by your peers on a series of yes/no questions related to the project. 
# You will be required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, and 
#3) CodeBook.md: a code book that describes the variables, the data, and any transformations or work that you performed 
#   to clean up the data. 
#4) README.md in the repo with your scripts. 

#  This repo explains how all of the scripts work and how they are connected. 

#One of the most exciting areas in all of data science right now is wearable computing. 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# Create one R script called run_analysis.R that does the following. 
#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Good luck!

##############################

### preparing the material ###

setwd("C:/Coursera/data/DataCleaning")

# download data
library(httr) 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "UCIHARDataset.zip"
if(!file.exists(file)){
	print("Downloading getdata-projectfiles-UCI HAR Dataset.zip")
	download.file(url, file)
}

#unzip and create folders (if required)
datafolder <- "UCI HAR Dataset"
if(!file.exists(datafolder)){
	print("Unzip files")
	unzip(file, list = FALSE, overwrite = TRUE)
} 
resultsfolder <- "TidyData"
if(!file.exists(resultsfolder)){
	print(paste("Creating results folder: ",resultsfolder))
	dir.create(resultsfolder)
} 

#read text and convert to data frame
gettables <- function (filename,cols = NULL){
	print(paste("Getting table: ", filename))
	f <- paste(datafolder,filename,sep="/")
	data <- data.frame()
	if(is.null(cols)){
		data <- read.table(f,sep="",stringsAsFactors=F)
	} else {
		data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
	}
	data
}

# Get features list
features <- gettables("features.txt")

# function: read data and build database
getdata <- function(type, features){
	print(paste("Getting data", type))
	subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
	y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
	x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
	return (cbind(subject_data,y_data,x_data))
}

# Get Test and Training datasets
test <- getdata("test", features)
train <- getdata("train", features)

# function: save the resulting data in the indicated folder
saveresults <- function (data,name){
	print(paste("saving results", name))
	file <- paste(resultsfolder, "/", name,".csv" ,sep="")
	write.csv(data,file)
}

### required activities ###

#1) Merge the training and the test sets to create one data set.
library(plyr)
Sdata <- rbind(train, test)
Sdata <- arrange(Sdata, id, activity) # Sort by id and activity

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
SdataMSD <- Sdata[,c(1,2,grep("std", colnames(Sdata)), grep("mean", colnames(Sdata)))]
saveresults(SdataMSD,"SdataMSD")

#3) Uses descriptive activity names to name the activities in the data set
activity_labels <- gettables("activity_labels.txt")
saveresults(SdataMSD,"SdataMSD2")

#4) Appropriately labels the data set with descriptive variable names. 
SdataMSD$activity <- factor(SdataMSD$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
STidyData <- ddply(SdataMSD, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(STidyData)[-c(1:2)] <- paste(colnames(STidyData)[-c(1:2)], "_mean", sep="")
saveresults(STidyData,"Galaxy S Tidy Data")
