# 1. Merges the training and the test sets to create one data set.
activity_labels <- read.table("./activity_labels.txt", col.names = c("code", "activity"))
features <- read.table("./features.txt", col.names = c("n", "functions"))

X_train <- read.table("./train/X_train.txt", col.names = features$functions)
X_test <- read.table("./test/X_test.txt", col.names = features$functions)
X <- rbind(X_train, X_test)

y_train <- read.table("./train/y_train.txt", col.names = "code")
y_test <- read.table("./test/y_test.txt", col.names = "code")
y <- rbind(y_train, y_test)

subject_train <- read.table("./train/subject_train.txt", col.names = "subject")
subject_test <- read.table("./test/subject_test.txt", col.names = "subject")
subject <- rbind(subject_train, subject_test)

data <- cbind(subject, y, X)

# 2. Extracts only the measurements on the mean 
# and standard deviation for each measurement.
tidy_data <- data %>% select(subject, code, contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set
tidy_data$code <- activity_labels[tidy_data$code, 2]
names(tidy_data)[[2]] <- "activity"

# 4. Appropriately labels the data set with descriptive variable names.
names(tidy_data) <- gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data) <- gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data) <- gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data) <- gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data) <- gsub("^t", "Time", names(tidy_data))
names(tidy_data) <- gsub("^f", "Frequency", names(tidy_data))
names(tidy_data) <- gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data) <- gsub("-mean()", "Mean", names(tidy_data))
names(tidy_data) <- gsub("-std()", "STD", names(tidy_data))
names(tidy_data) <- gsub("-freq()", "Frequency", names(tidy_data))
names(tidy_data) <- gsub("angle", "Angle", names(tidy_data))
names(tidy_data) <- gsub("gravity", "Gravity", names(tidy_data))

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
res <- tidy_data %>%
    group_by(subject, activity) %>%
    summarise_all(mean)
write.table(res, "./res.txt", row.names = FALSE)
