library(dplyr)
library(data.table)
library(reshape2)

directory <- "C:\\Users\\Akash Poonja\\Desktop\\datasciencecoursera\\week4data\\UCI HAR Dataset\\"
directory_test <- "C:\\Users\\Akash Poonja\\Desktop\\datasciencecoursera\\week4data\\UCI HAR Dataset\\test\\"
directory_train <- "C:\\Users\\Akash Poonja\\Desktop\\datasciencecoursera\\week4data\\UCI HAR Dataset\\train\\"        

# read from features.txt file to name variable columns
setwd(directory)
features_names <- fread("features.txt")
features_names<- features_names[,-1]
features_data <- t(features_names)

#1-1 reading test data in txt files as vectors
##TEST DATA
setwd(directory_test)
subject_test <- fread("subject_test.txt")
subject_test <- setNames(subject_test, "Subject")

X_test <- fread("X_test.txt")
y_test <- fread("y_test.txt")
X_test <- setNames(X_test, c(features_data[, 1:561]))
y_test <- setNames(y_test, "Activity")

test_data <- cbind(subject_test, y_test, X_test)

#1-2 reading train data in txt files as vectors
##TRAIN DATA
setwd(directory_train)
subject_train <- fread("subject_train.txt")

subject_train <- setNames(subject_train, "Subject")

X_train <- fread("X_train.txt")
X_train <- setNames(X_train, c(features_data[, 1:561]))
y_train <- fread("y_train.txt")
y_train <- setNames(y_train, "Activity")

train_data <- cbind(subject_train, y_train, X_train)


#1-3 combining test and train data together

complete_data <- rbind(test_data, train_data)

#2 Filter by columns showing mean and standard deviation

complete_data_select <- select(complete_data, contains(c("Subject", "Activity", "mean()", "std()")))

#3 replace activity numbers with descriptive activity names
activity_list <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

for(i in 1:6){
        
  complete_data_select$Activity[complete_data_select$Activity == i] <- activity_list[i]

}

#4 create an independent tidy data set with averages of each variable 
#for each activity AND each subject, using reshape2 package

subject_mean_i <- data.frame()
subject_mean <- data.frame()

subject_number <- c(1:30)
tidy_data <- data.frame()
for (i in 1:30){
  
  reshape_i <- filter(reshape, reshape$Subject == i)
  subject_data <- dcast(reshape_i, Activity ~ variable, mean)
  subject_data <- cbind(subject_number[i], subject_data)
  tidy_data <- rbind(tidy_data, subject_data)

}
tidy_data <- rename(tidy_data, Subject = subject_number[i])
