#Code Book
for Getting and Cleaning Data Final Project

## Overview
This code book provides a brief overview of the data from the experiements and summarizes the data fields in tidyData.txt.

## About the Experiments
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity were captured at a constant rate of 50Hz. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

## The Data Package
The dataset includes the following files:
- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels. 
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

## Tidy Data Set Variables

### activity
The activity performed by each person:
- WALKING
- WALKING_UPSTAIRS
- WALKING_DOWNSTAIRS
- SITTING
- STANDING
- LAYING

### subjectid
The unique id for each of the 30 subjects in the experiments. Values are 1 to 30.

### signals
The remaining variables in tidyData.txt are mean and standard deviation of each of the 33 signals from the accelerometer and gyroscope (66 variables in total)

- timeBodyAcc-XYZ
- timeGravityAcc-XYZ
- timeBodyAccJerk-XYZ
- timeBodyGyro-XYZ
- timeBodyGyroJerk-XYZ
- timeBodyAccMag
- timeGravityAccMag
- timeBodyAccJerkMag
- timeBodyGyroMag
- timeBodyGyroJerkMag
- frequencyBodyAcc-XYZ
- frequencyBodyAccJerk-XYZ
- frequencyBodyGyro-XYZ
- frequencyBodyAccMag
- frequencyBodyAccJerkMag
- frequencyBodyGyroMag
- frequencyBodyGyroJerkMag

Note: '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
