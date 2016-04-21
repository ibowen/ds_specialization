#The data linked to from the course website represent data collected from the accelerometers from the 
#Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#You should create one R script called run_analysis.R that does the following.
    #Merges the training and the test sets to create one data set.   
    #Extracts only the measurements on the mean and standard deviation for each measurement.
    #Uses descriptive activity names to name the activities in the data set
    #Appropriately labels the data set with descriptive variable names.
    #From the data set in step 4, creates a second, independent tidy data set with the average 
        #of each variable for each activity and each subject.

#Download the data source into the workspace
if (!file.exists("./assignment")) {
    dir.create("./assignment")
}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = url, destfile = "./assignment/datasource.zip")

#unzip the data source within the assignment dir
unzip(zipfile = "./assignment/datasource.zip", exdir = "./assignment")

#list the data source files
list.files("./assignment", recursive = TRUE)

# as mentioned in the project, we only care the following files:
#'features_info.txt': Shows information about the variables used on the feature vector.
#'features.txt': List of all features.
#'activity_labels.txt': Links the class labels with their activity name.
#'train/X_train.txt': Training set.
#'train/y_train.txt': Training labels.
#'test/X_test.txt': Test set.
#'test/y_test.txt': Test labels.

# read activity files
parentpath <- "./assignment/UCI HAR Dataset"
act_test <- read.table(file.path(parentpath, "test", "Y_test.txt"), header = FALSE)
act_train <- read.table(file.path(parentpath, "train", "Y_train.txt"), header = FALSE)

#read subject files
subject_test <- act_test <- read.table(file.path(parentpath, "test", "subject_test.txt"), header = FALSE)
subject_train <- act_train <- read.table(file.path(parentpath, "train", "subject_train.txt"), header = FALSE)

#read feature files
feature_test <- read.table(file.path(parentpath, "test", "X_test.txt"), header = FALSE)
feature_train <- read.table(file.path(parentpath, "train", "X_train.txt"), header = FALSE)

#merge the training and test sets
act <- rbind(act_test, act_train)
subject <- rbind(subject_test, subject_train)
feature <- rbind(feature_test, feature_train)

#set column names
names(act) <- c("activity")
names(subject) <- c("subject")
feature_names <- read.table(file.path(parentpath, "features.txt"), header = FALSE)
names(feature) <- feature_names$V2


#merge all columns for all data
alldata <- cbind(subject, act, feature)

#extract the mean and std columns names
mean_and_std <- feature_names$V2[grepl("mean\\(\\)|std\\(\\)", feature_names$V2)]

#subset required data by names
selectcolumns <- c(as.character(mean_and_std), "subject", "activity")
data <- subset(alldata, select = selectcolumns)

#read descriptive activity names
act_labels <- read.table(file.path(parentpath, "activity_labels.txt"), header = FALSE)

#update the activity names according to activity labels
data$activity <- act_labels[match(data$activity, act_labels$V1), 2]

#Appropriately labels the data set with descriptive variable names.
names(data)<-gsub("^t", "time", names(data))
names(data)<-gsub("^f", "frequency", names(data))
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))

#From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject.
library(dplyr)
by_act_sub <- group_by(data, activity, subject)
result <- summarize_each(by_act_sub, funs = funs(mean))
arrange(result, desc(subject), desc(activity))

#write the final data
write.table(result, file = "tidy_data.txt", row.names = FALSE)
