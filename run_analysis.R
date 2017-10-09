## set working directory
setwd("~/Desktop/course3-pro1")

##download and unzip datasete

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="Dataset.zip",method="curl")
unzip("Dataset.zip")

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('/Users/PeiLiu/Desktop/course3-pro1/UCI HAR Dataset/')

# read train data
trainSubjects <- read.table('./train/subject_train.txt',header=FALSE)
trainValues <- read.table('./train/X_train.txt',header=FALSE)
trainActivity <- read.table('./train/y_train.txt',header=FALSE)

# read test data
testSubjects <- read.table('./test/subject_test.txt',header=FALSE)
testValues <- read.table('./test/X_test.txt',header=FALSE)
testActivity <- read.table('./test/y_test.txt',header=FALSE)

# read feature and activity_labels data
features <- read.table('./features.txt',header=FALSE)
activity <- read.table('./activity_labels.txt',header=FALSE)

##Merges the train and the test sets to create one data set
dataSubject <- rbind(trainSubjects, testSubjects)
dataActivity<- rbind(trainActivity, testActivity)
dataValues<- rbind(trainValues, testValues)

# assign column names to dataset
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
names(dataValues)<- features[,2]

## Merge all data together to a dataset
dataCombine <- cbind(dataSubject, dataActivity)
My_Data1 <- cbind(dataValues, dataCombine)


## Extracts only the measurements on the mean and standard deviation for each measurement
subFeatureNames<-features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
selectedNames<-csub(as.character(subFeatureNames), "subject", "activity" )
My_Data2<-subset(My_Data1,select=selectedNames)

##Uses descriptive activity names to name the activities in activity dataset
dataActivity [,1] <- activity[dataActivity[,1],2]
# Uses descriptive activity names to name the activities in all dataset
My_Data2$activity <- activity[My_Data2$activity,2]

##Appropriately labels the data set with descriptive variable names
names(My_Data2)<-gsub("^t", "time", names(My_Data2))
names(My_Data2)<-gsub("^f", "frequency", names(My_Data2))
names(My_Data2)<-gsub("Acc", "Accelerometer", names(My_Data2))
names(My_Data2)<-gsub("Gyro", "Gyroscope", names(My_Data2))
names(My_Data2)<-gsub("Mag", "Magnitude", names(My_Data2))
names(My_Data2)<-gsub("BodyBody", "Body", names(My_Data2))

##creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)
My_Data3<-aggregate(. ~subject + activity, My_Data2, mean)
My_Data3<-My_Data3[order(My_Data3$subject,My_Data3$activity),]
write.table(My_Data3, file = "tidydata.txt",row.name=FALSE)

## another method
library(dplyr)
TidyDataMean <- My_Data2 %>%
group_by(activity, subject) %>% 
summarise_each(funs(mean))
write.table(TidyDataMean, file = "tidydata.txt",row.name=FALSE)
