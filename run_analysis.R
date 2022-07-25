
library(dplyr)
library(tidyr)

#read the activity labels used by the data set. we will use them later for having a more human readable dataset
activityLabels <- read.fwf(file="activity_labels.txt", widths = c(1,20))
names(activityLabels) <- c("id, activity")

#read all the features/variables contained in the HAR Dataset, which will be used to name the columns in the dataset
featuresLines <- readLines("features.txt")
features <- data.frame(x=featuresLines) %>%
            extract(x, c("id", "feature"), regex = "^(\\d+) (.*)$")

#
#read the subject who has performed the activity, the activity being performed and the measurements of the activity, both for the test and the training sets
#

#read the test set
subject_test <- data.frame(subject=readLines("./test/subject_test.txt"))
activity_test <- data.frame(activity=readLines("./test/Y_test.txt"))
measurements_test <- read.fwf(file="./test/X_test.txt", widths=rep(16, 561))
names(measurements_test) <- features$feature

#read the train set
subject_train <- data.frame(subject=readLines("./train/subject_train.txt"))
activity_train <- data.frame(activity=readLines("./train/Y_train.txt"))
measurements_train <- read.fwf(file="./train/X_train.txt", widths=rep(16, 561))
names(measurements_train) <- features$feature

#merge both data sets
measurements <- union_all(measurements_test, measurements_train)
subjects <- union_all(subject_test, subject_train)
activities <- union_all(activity_test, activity_train)

#create the data frame with subjects and activities. only extract the variables related with the mean and std deviation
measurements_df <- data.frame(subject=subjects, activity=activities)

#extract the variables related with the mean and std deviation
measurements_df <- cbind(measurements_df, measurements[grep("mean", features$feature, value=TRUE)])
measurements_df <- cbind(measurements_df, measurements[grep("std", features$feature, value=TRUE)])

#Uses descriptive activity names to name the activities in the data set
aaname <- function(x){activityLabels[x, 2]}
measurements_df$activity <- sapply(measurements_df$activity, aaname)

#Appropriately labels the data set with descriptive variable names. 
dd <- c(grep("mean", features$feature, value=TRUE), grep("std", features$feature, value=TRUE))
dd <- gsub("\\-","",dd)
dd <- gsub("\\(","",dd)
dd <- gsub("\\)","",dd)
ee <- c("subject", "activity", dd)
names(measurements_df) <- ee

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
measurements_gr <- measurements_df %>%
                   group_by(subject, activity) %>%
                   summarise(across(all_of(dd), mean))

#write those dataset fo csv files for later analysis
write.table(measurements_df, file="../measurements_dataset.txt", row.name=FALSE)
write.table(measurements_gr, file="../measurements_summarise_dataset.txt", row.name=FALSE)

#write.csv(measurements_df, file="../measurements_dataset.csv")
#write.csv(measurements_gr, file="../measurements_summarise_dataset.csv")