#### Coursera Getting and Cleaning Data - Smartphone Activity Recognition Project ####

require(dplyr)


#### Gather feature labels ####
feature.labels <- read.table("features.txt",header=FALSE)


#### Gather activity labels ####
activity.labels <- read.table("activity_labels.txt",header=FALSE)
# provide better column names, "activity" column name will be used later in a table join
colnames(activity.labels) <- c("activity","activity_type")


#### Gather measured data for training and test ####
train.data <- read.table("train/X_train.txt",header=FALSE)
test.data <- read.table("test/X_test.txt",header=FALSE)


#### Gather activity classification data for training and test ####
train.activity <- read.table("train/y_train.txt",header=FALSE)
test.activity <- read.table("test/y_test.txt",header=FALSE)


#### Gather subject data ####
train.subject <- read.table("train/subject_train.txt",header=FALSE)
test.subject <- read.table("test/subject_test.txt",header=FALSE)


#### Combine all training and test data ####
combined.data <- rbind(train.data,test.data)
combined.activity <- rbind(train.activity,test.activity)
combined.subject <- rbind(train.subject,test.subject)


#### Make relevant column labels ####
colnames(combined.activity) <- "activity"
colnames(combined.subject) <- "subject"
colnames(combined.data) <- feature.labels$V2


#### Combine data sets ####
all.data <- cbind(combined.subject,combined.activity,combined.data)


#### Remove duplicate columns ####
all.data.trimmed <- all.data[, !duplicated(colnames(all.data))]


#### Extract only the measurements on mean and standard deviation ####
all.data.filtered <- all.data.trimmed %>% select(subject,activity,contains("mean"),contains("std"))


#### Add human readable activity labels ####
# perform table join to associate activity number with activity text, then remove the activity number
all.data.labeled <- all.data.filtered %>% left_join(activity.labels,by = "activity") %>% select(-activity)


#### Provide more detailed variable names and remove duplication ####
names(all.data.labeled) <- gsub("^t", "time", names(all.data.labeled))
names(all.data.labeled) <- gsub("^f", "frequency", names(all.data.labeled))
names(all.data.labeled) <- gsub("Acc", "Accelerometer", names(all.data.labeled))
names(all.data.labeled) <- gsub("Gyro", "Gyroscope", names(all.data.labeled))
names(all.data.labeled) <- gsub("Mag", "Magnitude", names(all.data.labeled))
names(all.data.labeled) <- gsub("BodyBody", "Body", names(all.data.labeled))


#### Group the final dataset by subject and activity type, and then summarize by the mean of each variable ####
all.data.mean <- all.data.labeled %>% group_by(subject,activity_type) %>% summarize_all(funs(mean))
