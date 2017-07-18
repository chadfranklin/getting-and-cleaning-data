
zipfilename <- "getdata_dataset.zip"
filename <- "UCI HAR Dataset"

#Check if file exists
if (!file.exists(filename)) { 

    #Check if zip file was downloaded
    if (!file.exists(zipfilename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, zipfilename, method="curl")
    }
    
    unzip(zipfilename)
} else {
    print("File already exists")
}


## 1. Merge the training and the test sets to create one data set.

# Read files
activityLabels <- read.table(paste(filename, "activity_labels.txt", sep="/"), col.names = c("id", "activity"))
features <- read.table(paste(filename, "features.txt", sep="/"), col.names = c("id","feature"))

subjectTest <- read.table(paste(filename, "test", "subject_test.txt", sep="/"), col.names = c("subjectid"))
xTest <- read.table(paste(filename, "test", "x_test.txt", sep="/"), col.names = features$feature)
yTest <- read.table(paste(filename, "test", "y_test.txt", sep="/"), col.names = c("activityid"))

subjectTrain <- read.table(paste(filename, "train", "subject_train.txt", sep="/"), col.names = c("subjectid"))
xTrain <- read.table(paste(filename, "train", "x_train.txt", sep="/"), col.names = features$feature)
yTrain <- read.table(paste(filename, "train", "y_train.txt", sep="/"), col.names = c("activityid"))

# Merge data
mergedData <- rbind(cbind(subjectTrain, yTrain, xTrain), cbind(subjectTest, yTest, xTest))

## 2. Extract the mean and standard deviation for each measurement.
#     Note: add \\. to each measurement in grepl to only get mean and std (and not meanFreq for example)
extractedLogical <- grepl("mean\\.", colnames(mergedData)) | grepl("std\\.", colnames(mergedData)) | grepl("activity", colnames(mergedData)) | grepl("subjectid", colnames(mergedData))
extractedData <- mergedData[extractedLogical]

## 3. Add activity label to extracted data
extractedData <- merge(extractedData, activityLabels, by.x = "activityid", by.y = "id")


## 4. Appropriately labels the data set with descriptive variable names

# Lowercase everything
colnames(extractedData) <- tolower(colnames(extractedData))

#remove dots
colnames(extractedData) <- gsub("\\.", "", colnames(extractedData))

#expand prefixes
colnames(extractedData) <- gsub("^t", "time", colnames(extractedData))
colnames(extractedData) <- gsub("^f", "frequency", colnames(extractedData))

## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Aggregate by activity and subjectid over extractedData. Do not include first two columns (activityid and subjectid) and last column (activity)
finalData <- aggregate(extractedData[,3:(ncol(extractedData) - 1)], by=list(extractedData$activity, extractedData$subjectid), mean)

#rename group columns
colnames(finalData)[1:2] <- c("activity", "subjectid")

#write data set to file
write.table(tidyData, 'tidyData.txt',row.names=TRUE,sep='\t')
