
# Load required packages
detach(package:plyr)
library(dplyr)
library(reshape)
library(reshape2)

# read features - col names
features <- read.table("UCI HAR Dataset/features.txt")
mean_cols <- 
  std_cols <- grepl('*std', features$V2)

# finds cols index and names related to mean and std for later use
mean_std_cols_index <- grepl('*mean | *std', features$V2)
mean_std_cols_names <- features$V2[mean_std_cols_index]

# read activity labels 
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

# names activity labels
names(activity_labels) <- c("Activity_ID", "Activity_label")


# read training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")

# assign col names to training data
names(subject_train) <- "subject_ID"
names(Y_train) <- "Activity_ID"

# select mean and std cols from training data, and assign col names
X_train_mean_std <- X_train[,mean_std_cols_index]
names(X_train_mean_std) <- mean_std_cols_names

# merge training data
train_data <- cbind(subject_train, Y_train, X_train_mean_std)
train_data_final <- merge(train_data, activity_labels, by = "Activity_ID")


# read test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")

# assign col names to training data
names(subject_test) <- "subject_ID"
names(Y_test) <- "Activity_ID"

# select mean and std cols from training data, and assign col names
X_test_mean_std <- X_test[,mean_std_cols_index]
names(X_test_mean_std) <- mean_std_cols_names

# merge test data
test_data <- cbind(subject_test, Y_test, X_test_mean_std)
test_data_final <- merge(test_data, activity_labels, by = "Activity_ID")

# merge training and test data
all_data <- rbind(train_data_final, test_data_final)

# tidy data and estimate average for each variable for each activity for each subject
tidy_data <- all_data %>% 
  melt(id = c("subject_ID", "Activity_ID", "Activity_label")) %>%
  group_by(subject_ID, Activity_label, variable) %>%
  summarize(average = mean(value))

# write tidy data to txt
write.table(tidy_data, "tidy_data.txt", row.name = FALSE)

