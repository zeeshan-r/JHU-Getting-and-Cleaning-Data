#' ---
#' title: "Getting and Cleaning Data Course Project"
#' output: html_notebook
#' ---
#' 
#' Loading and formatting data
#' 
## ---------------------------------------------------------------------------------------------

train_x=read.table("./data/UCI HAR Dataset/train/X_train.txt")
train_y=read.csv("./data/UCI HAR Dataset/train/Y_train.txt", header = F, sep = ' ')
train_sub=read.csv("./data/UCI HAR Dataset/train/subject_train.txt", header = F, sep = ' ')

features=read.csv('./data/UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features=as.character(features[,2])

train_data =data.frame(train_sub, train_y, train_x)
names(train_data) <- c(c('subject', 'activity'), features)

test_x=read.table("./data/UCI HAR Dataset/test/X_test.txt")
test_y=read.csv("./data/UCI HAR Dataset/test/Y_test.txt", header = F, sep = ' ')
test_sub=read.csv("./data/UCI HAR Dataset/test/subject_test.txt", header = F, sep = ' ')

test_data =data.frame(test_sub,test_y, test_x)
names(test_data) <- c(c('subject', 'activity'), features)


#' 
#' 
#' Merges the training and the test sets to create one data set.
#' 
## ---------------------------------------------------------------------------------------------
all_data=rbind(train_data,test_data)


#' 
#' 
#' Extracts only the measurements on the mean and standard deviation for each measurement.
#' 
#' 
## ---------------------------------------------------------------------------------------------

mean_std_data=grep('mean|std', features)

selected_data=all_data[,c(1,2,mean_std_data + 2)]

#' 
#' Uses descriptive activity names to name the activities in the data set
#' 
## ---------------------------------------------------------------------------------------------

act_labels <- read.table('./data/UCI HAR Dataset/activity_labels.txt', header = FALSE)
act_labels <- as.character(act_labels[,2])
selected_data$activity <- act_labels[selected_data$activity]


#' 
#' 
#' Appropriately labels the data set with descriptive variable names.
#' 
#' 
## ---------------------------------------------------------------------------------------------

new_names <- names(selected_data)

new_names <- gsub("[(][)]", "", new_names)
new_names <- gsub("^t", "TimeDomain_", new_names)
new_names <- gsub("^f", "FrequencyDomain_", new_names)

new_names <- gsub("Acc", "Accelerometer", new_names)
new_names <- gsub("Gyro", "Gyroscope", new_names)
new_names <- gsub("Mag", "Magnitude", new_names)

new_names <- gsub("-mean-", "_Mean_", new_names)
new_names <- gsub("-std-", "_StandardDeviation_", new_names)
new_names <- gsub("-", "_", new_names)

names(selected_data) <- new_names



#' 
#' From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#' 
## ---------------------------------------------------------------------------------------------
tidy_df = aggregate(selected_data[,3:81], by = list(activity = selected_data$activity, subject = selected_data$subject),FUN = mean)
write.table(x = tidy_df, file = "tidy_df.txt", row.names = F)



#' 
#' 
#' 
