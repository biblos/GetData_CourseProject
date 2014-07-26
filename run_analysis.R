# Course Project
# Script run_analysis.R

if (!file.exists("UCI HAR Dataset")){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                destfile="UCI_HAR_Dataset.zip")
  unzip("UCI_HAR_Dataset.zip")
  file.remove("UCI_HAR_Dataset.zip")
}

# Extracts only the measurements on the mean and standard deviation for each measurement. 
col_desc <- read.table(file="UCI HAR Dataset/features.txt", sep=" ", 
                       col.names=c("Col", "Desc"), colClasses=c(Desc="character"))
ind_meanstd_col <- grep("-mean\\(\\)|-std\\(\\)", col_desc$Desc)
col_widths <- c()
for (i in seq_along(ind_meanstd_col)) {
  if (i>1 && ind_meanstd_col[i] > ind_meanstd_col[i-1]+1) {
    columns2skip <- ind_meanstd_col[i] - ind_meanstd_col[i-1] - 1
    col_widths <- c(col_widths, - columns2skip*16)
  }
  col_widths <- c(col_widths, 16)
}

# Reads the training and test sets. 
# Appropriately labels the data set with descriptive variable names. 
name_meanstd_col <- col_desc$Desc[ind_meanstd_col]
name_meanstd_col <- gsub('\\(\\)', '', name_meanstd_col)
remove(col_desc)
s_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("Subject"))
X_train <- read.fwf("UCI HAR Dataset/train/X_train.txt", widths=col_widths, 
                    col.names=name_meanstd_col, check.names=FALSE)
Y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names=c("ActivityID"))
s_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("Subject"))
X_test <- read.fwf("UCI HAR Dataset/test/X_test.txt", widths=col_widths, 
                   col.names=name_meanstd_col, check.names=FALSE)
Y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names=c("ActivityID"))

# Merges the training and the test sets to create one data set.
train <- cbind(s_train, X_train, Y_train)
test <- cbind(s_test, X_test, Y_test)
remove(s_train, X_train, Y_train, s_test, X_test, Y_test)
data_set <- rbind(train, test)
remove(train, test)

# Uses descriptive activity names to name the activities in the data set
act_labels <- read.table(file="UCI HAR Dataset/activity_labels.txt", sep=" ", 
                       col.names=c("ActivityID", "Activity"), colClasses=c(Activity="character"))
data_set <- merge(x=data_set, y=act_labels, by.x="ActivityID", by.y="ActivityID")
data_set$ActivityID <- NULL
remove(act_labels)

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
numeric_cols_ds <- !names(data_set) %in% c("Subject", "Activity")
avg_data_set <- aggregate(data_set[numeric_cols_ds], by=list(data_set$Subject, data_set$Activity), FUN=mean)
names(avg_data_set)[names(avg_data_set)=="Group.1"] <- "Subject"
names(avg_data_set)[names(avg_data_set)=="Group.2"] <- "Activity"
numeric_cols_ads <- !names(avg_data_set) %in% c("Subject", "Activity")
names(avg_data_set)[numeric_cols_ads] <- unlist(lapply(names(avg_data_set)[numeric_cols_ads], paste, "average", sep="_"))
avg_data_set <- avg_data_set[order(avg_data_set$Subject),]

# Write tidy data set in a file.
write.csv(avg_data_set, file="UCI HAR Dataset/tidy_data_set.txt", row.names=FALSE) 
