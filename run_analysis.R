
#Coursera Getting and Cleaning Data
#Srikar Murali

library(plyr)


#1.Retrieve the data sets for training and test and then store them as variables

features <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/features.txt", header = FALSE)
activitylabels <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/activity_labels.txt", header = FALSE)
subTrain <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
xTr <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/train/X_train.txt", header = FALSE)
yTr <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/train/y_train.txt", header = FALSE)
subTest <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
xTest <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/test/X_test.txt", header = FALSE)
yTest <- read.table("C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/test/y_test.txt", header = FALSE)

#give column names to both test and training data sets
colnames(activitylabels) <- c("activityId", "activityType")
colnames(subTrain) <- "subjectId"
colnames(xTr) <- features[, 2]
colnames(yTr) <- "activityId"
colnames(subTest) <- "subjectId"
colnames(xTest) <- features[, 2]
colnames(yTest) <- "activityId"


#bind the training set together
trainData <- cbind(xTr, subTrain, yTr)

#bind the test set together
testData <- cbind(xTest, subTest, yTest)

#bind the train and test data together
endData <- rbind(trainData, testData)

#retrieve the column names
columnnames <- colnames(endData)

#2. Retrieve mean and standard deviation from each measurement in the end data set

#get the values for the id, mean and standard deviation columns and mark them as TRUE, while the rest are false
#mark the values where there is a mean and standard deviation for the Id, and mark the other values as false 
valuecol <- (grepl("activity..",columnnames) | grepl("subject..",columnnames) | grepl("-mean..",columnnames) & !grepl("-meanFreq..",columnnames) & !grepl("mean..-",columnnames) | grepl("-std..",columnnames) & !grepl("-std()..-",columnnames))


#remove columns where values are false, and keep the columns where values are true
endData <- endData[valuecol == TRUE]

#3. Establish names for each activity in the end data set
#merge endData with the activity labels to name each activity
endData <- merge(endData, activitylabels, by = "activityId", all.x = TRUE)

#again retrieve the column names
columnnames <- colnames(endData)

#4. Label data set with descriptive names

#use a loop to remove and clean up the variable names for each activity, so that each name accurately represents each activity
#use regex to remove each variable name when needed
for (i in 1:length(columnnames)) 
{
  columnnames[i] = gsub("\\()","",columnnames[i])
  columnnames[i] = gsub("-std$","StdDev",columnnames[i])
  columnnames[i] = gsub("-mean","Mean",columnnames[i])
  columnnames[i] = gsub("^(t)","time",columnnames[i])
  columnnames[i] = gsub("^(f)","freq",columnnames[i])
  columnnames[i] = gsub("([Gg]ravity)","Gravity",columnnames[i])
  columnnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnnames[i])
  columnnames[i] = gsub("[Gg]yro","Gyro",columnnames[i])
  columnnames[i] = gsub("AccMag","AccMagnitude",columnnames[i])
  columnnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columnnames[i])
  columnnames[i] = gsub("JerkMag","JerkMagnitude",columnnames[i])
  columnnames[i] = gsub("GyroMag","GyroMagnitude",columnnames[i])
}

#after cleaning the column variable names reassign them
colnames(endData) <- columnnames

#5. Create another tidy dataset

#create new table
endDatav2 <- endData[, names(endData) != "activityType"]

#get mean and put it into the final tidy set

theend <- aggregate(endDatav2[,names(endDatav2) != c('activityId','subjectId')],by=list(activityId=endDatav2$activityId,subjectId = endDatav2$subjectId),mean);

#create final data set by merging the labels with the theend data
theend <- merge(theend, activitylabels, by = "activityId", all.x = TRUE)

#Export
write.table(theend, 'C:/Users/Nathan/Documents/John Hopkins Data Science Coursera Projects/UCI HAR Dataset/tidyData.txt',row.names=TRUE,sep='\t')
