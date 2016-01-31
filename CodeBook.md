# CodeBook.md

## run_analysis.R 

* Downloads required data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
* Unzips the file if it has not been uncompressed.
* Creates results folder if it does not exist (all files are stored in this folder)
* Loads features.txt used for columns
* Loads X_train.txt, y_train.txt, subject_train.txt
* X_train contains the data using the feature data set as columns
* y_train contains the activity labels
* subject_train contains the ids
* Loads and appends test dataset using X_test.txt, y_test.txt, subject_test.txt
* X_test contains the data using the feature data set as columns
* y_test contains the activity labels
* subject_test contains the ids
* Merge train and test data, by binding rows since all columns are same in both datasets.
* Sort the merged dataset by id and Activity.
* Loads activity_labels.txt. This will replace the Activity numbers in the final dataset.
* Changes the data activity row to use the activity labels
* Saves the Mean and Standard Deviation values into SDataMSD.csv and SDataMSD2.csv (Activity labels)
* saves the tidy dataset into 'Galaxy S Tidy Data.csv'.

## SDataMSD.csv

* contains Mean and Standard Deviation Values for each sensor axes in csv format.

Variables:

*  id	: Volunteer id
*  activity	: 6 activities.

> X,Y,Z axis movements for each of the sensors including, Accelerometer, Gravity Sensor, and Gyroscope.
   
*  tBodyAcc.std...X	
*  tBodyAcc.std...Y	
*  tBodyAcc.std...Z	
*  tGravityAcc.std...X	
*  tGravityAcc.std...Y	
*  tGravityAcc.std...Z	
*  tBodyAccJerk.std...X	
*  tBodyAccJerk.std...Y	
*  tBodyAccJerk.std...Z	
*  tBodyGyro.std...X	
*  tBodyGyro.std...Y	
*  tBodyGyro.std...Z	
*  tBodyGyroJerk.std...X	
*  tBodyGyroJerk.std...Y	
*  tBodyGyroJerk.std...Z	
*  tBodyAccMag.std..	
*  tGravityAccMag.std..	
*  tBodyAccJerkMag.std..	
*  tBodyGyroMag.std..	
*  tBodyGyroJerkMag.std..	
*  fBodyAcc.std...X	
*  fBodyAcc.std...Y	
*  fBodyAcc.std...Z	
*  fBodyAccJerk.std...X	
*  fBodyAccJerk.std...Y	
*  fBodyAccJerk.std...Z	
*  fBodyGyro.std...X	
*  fBodyGyro.std...Y	
*  fBodyGyro.std...Z	
*  fBodyAccMag.std..	
*  fBodyBodyAccJerkMag.std..	
*  fBodyBodyGyroMag.std..	
*  fBodyBodyGyroJerkMag.std..	
*  tBodyAcc.mean...X	
*  tBodyAcc.mean...Y	
*  tBodyAcc.mean...Z	
*  tGravityAcc.mean...X	
*  tGravityAcc.mean...Y	
*  tGravityAcc.mean...Z	
*  tBodyAccJerk.mean...X	
*  tBodyAccJerk.mean...Y	
*  tBodyAccJerk.mean...Z	
*  tBodyGyro.mean...X	
*  tBodyGyro.mean...Y	
*  tBodyGyro.mean...Z	
*  tBodyGyroJerk.mean...X	
*  tBodyGyroJerk.mean...Y	
*  tBodyGyroJerk.mean...Z	
*  tBodyAccMag.mean..	
*  tGravityAccMag.mean..	
*  tBodyAccJerkMag.mean..	
*  tBodyGyroMag.mean..	
*  tBodyGyroJerkMag.mean..	
*  fBodyAcc.mean...X	
*  fBodyAcc.mean...Y	
*  fBodyAcc.mean...Z	
*  fBodyAcc.meanFreq...X	
*  fBodyAcc.meanFreq...Y	
*  fBodyAcc.meanFreq...Z	
*  fBodyAccJerk.mean...X	
*  fBodyAccJerk.mean...Y	
*  fBodyAccJerk.mean...Z	
*  fBodyAccJerk.meanFreq...X	
*  fBodyAccJerk.meanFreq...Y	
*  fBodyAccJerk.meanFreq...Z	
*  fBodyGyro.mean...X	
*  fBodyGyro.mean...Y	
*  fBodyGyro.mean...Z	
*  fBodyGyro.meanFreq...X	
*  fBodyGyro.meanFreq...Y	
*  fBodyGyro.meanFreq...Z	
*  fBodyAccMag.mean..	
*  fBodyAccMag.meanFreq..	
*  fBodyBodyAccJerkMag.mean..	
*  fBodyBodyAccJerkMag.meanFreq..	
*  fBodyBodyGyroMag.mean..	
*  fBodyBodyGyroMag.meanFreq..	
*  fBodyBodyGyroJerkMag.mean..	
*  fBodyBodyGyroJerkMag.meanFreq..

# Galaxy S Tidy Data.csv

contains 180 rows and 82 columns (including enumeration column) in a default csv format. The data is from 30 test subjects performing 6 activities while carrying a Galaxy S smartphone each.

variables:

*  id	: Volunteer id
*  activity	: 6 activities - WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING.

> X,Y,Z axis movements for each of the sensors including, Accelerometer, Gravity Sensor, and Gyroscope.
   
*  tBodyAcc.std...X_mean	
*  tBodyAcc.std...Y_mean	
*  tBodyAcc.std...Z_mean	
*  tGravityAcc.std...X_mean	
*  tGravityAcc.std...Y_mean	
*  tGravityAcc.std...Z_mean	
*  tBodyAccJerk.std...X_mean	
*  tBodyAccJerk.std...Y_mean	
*  tBodyAccJerk.std...Z_mean	
*  tBodyGyro.std...X_mean	
*  tBodyGyro.std...Y_mean	
*  tBodyGyro.std...Z_mean	
*  tBodyGyroJerk.std...X_mean	
*  tBodyGyroJerk.std...Y_mean	
*  tBodyGyroJerk.std...Z_mean	
*  tBodyAccMag.std.._mean	
*  tGravityAccMag.std.._mean	
*  tBodyAccJerkMag.std.._mean	
*  tBodyGyroMag.std.._mean	
*  tBodyGyroJerkMag.std.._mean	
*  fBodyAcc.std...X_mean	
*  fBodyAcc.std...Y_mean	
*  fBodyAcc.std...Z_mean	
*  fBodyAccJerk.std...X_mean	
*  fBodyAccJerk.std...Y_mean	
*  fBodyAccJerk.std...Z_mean	
*  fBodyGyro.std...X_mean	
*  fBodyGyro.std...Y_mean	
*  fBodyGyro.std...Z_mean	
*  fBodyAccMag.std.._mean	
*  fBodyBodyAccJerkMag.std.._mean	
*  fBodyBodyGyroMag.std.._mean	
*  fBodyBodyGyroJerkMag.std.._mean	
*  tBodyAcc.mean...X_mean	
*  tBodyAcc.mean...Y_mean	
*  tBodyAcc.mean...Z_mean	
*  tGravityAcc.mean...X_mean	
*  tGravityAcc.mean...Y_mean	
*  tGravityAcc.mean...Z_mean	
*  tBodyAccJerk.mean...X_mean	
*  tBodyAccJerk.mean...Y_mean	
*  tBodyAccJerk.mean...Z_mean	
*  tBodyGyro.mean...X_mean	
*  tBodyGyro.mean...Y_mean	
*  tBodyGyro.mean...Z_mean	
*  tBodyGyroJerk.mean...X_mean	
*  tBodyGyroJerk.mean...Y_mean	
*  tBodyGyroJerk.mean...Z_mean	
*  tBodyAccMag.mean.._mean	
*  tGravityAccMag.mean.._mean	
*  tBodyAccJerkMag.mean.._mean	
*  tBodyGyroMag.mean.._mean	
*  tBodyGyroJerkMag.mean.._mean	
*  fBodyAcc.mean...X_mean	
*  fBodyAcc.mean...Y_mean	
*  fBodyAcc.mean...Z_mean	
*  fBodyAcc.meanFreq...X_mean	
*  fBodyAcc.meanFreq...Y_mean	
*  fBodyAcc.meanFreq...Z_mean	
*  fBodyAccJerk.mean...X_mean	
*  fBodyAccJerk.mean...Y_mean	
*  fBodyAccJerk.mean...Z_mean	
*  fBodyAccJerk.meanFreq...X_mean	
*  fBodyAccJerk.meanFreq...Y_mean	
*  fBodyAccJerk.meanFreq...Z_mean	
*  fBodyGyro.mean...X_mean	
*  fBodyGyro.mean...Y_mean	
*  fBodyGyro.mean...Z_mean	
*  fBodyGyro.meanFreq...X_mean	
*  fBodyGyro.meanFreq...Y_mean	
*  fBodyGyro.meanFreq...Z_mean	
*  fBodyAccMag.mean.._mean	
*  fBodyAccMag.meanFreq.._mean	
*  fBodyBodyAccJerkMag.mean.._mean	
*  fBodyBodyAccJerkMag.meanFreq.._mean	
*  fBodyBodyGyroMag.mean.._mean	
*  fBodyBodyGyroMag.meanFreq.._mean	
*  fBodyBodyGyroJerkMag.mean.._mean	
*  fBodyBodyGyroJerkMag.meanFreq.._mean