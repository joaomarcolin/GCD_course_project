# script 2020 08 27


# 1 reading the raw data into R -------------------------------------------
library(tidyverse) # i found it easier to read the raw files and rename the columns using the tidyverse package

x_test <- read_table2("raw data/test/X_test.txt", 
                      col_names = FALSE)
y_test <- read_table2("raw data/test/y_test.txt", 
                      col_names = FALSE)
subject_test <- read_csv("raw data/test/subject_test.txt", 
                         col_names = FALSE)
x_train <- read_table2("raw data/train/X_train.txt", 
                       col_names = FALSE)
y_train <- read_csv("raw data/train/y_train.txt", 
                    col_names = FALSE)
subject_train <- read_csv("raw data/train/subject_train.txt", 
                          col_names = FALSE)

# first i bind all test data in a table named "test", and bind all train data in a table named "train"
#
# to avoid multiple columns named "X1", I rename the columns in y_test, subject_test, y_train and subject_train with the name of the original file

test <- cbind(rename(y_test, test_label = X1),
              rename(subject_test, subject_test = X1),
              x_test)

train <- cbind(rename(y_train, train_label = X1),
               rename(subject_train, subject_train = X1),
               x_train)

rm(y_test, y_train, subject_test, subject_train, x_test, x_train)

# i then read the features data, that contains the names of the variables that up until now we've been callin X1, X2, ..., X561
features <- read_table2("raw data/features.txt", 
                        col_names = FALSE)

# now I use the "features" table to rename the variables in the "test" and "train" tables

names(test) <- c("label", "subject", features$X2)

names(train) <- c("label", "subject", features$X2)

rm(features)

# 2 merges the training and the test sets ---------------------------------
# since both data sets have the same column names and differ only by the subjects included in each set,
# i merely bind both tables in a larger data set that contains all subjects

data <- rbind(test, train)
rm(test,train)

# 3 extracts only the measurements on the mean and sd ---------------------
# first I search for the names of the variables that we want to keep
columns_to_keep <- grepl("label|subject|mean|std", names(data))
# now I subset the table, keeping just the variables selected above
data <- data[, columns_to_keep]
rm(columns_to_keep)

# 4 sets descriptive activity names ---------------------------------------

# first I read the "activity_labels" table into R to find which labels correspond to which activities

activity_labels <- read_table2("raw data/activity_labels.txt", 
                               col_names = FALSE)

# now I use the information in the "activity_labels" table
# to susbtitute the numbers in data$label for activity names

for (i in 1:length(data$label)) {
        if (data$label[i] == 1) {data$label[i] <- "WALKING"}
        else if (data$label[i] == 2) {data$label[i] <- "WALKING_UPSTAIRS"}
        else if (data$label[i] == 3) {data$label[i] <- "WALKING_DOWNSTAIR"}
        else if (data$label[i] == 4) {data$label[i] <- "SITTING"}
        else if (data$label[i] == 5) {data$label[i] <- "STANDING"}
        else {data$label[i] <- "LAYING"}
}

rm(i, activity_labels) 

# i then rename the column "label" to give it a more descriptive name
data <- data %>% rename(activity = label)
View(data)

# 5 labels the data set with descriptive variable names -------------------
# first I create a vector of column names and remove special characters
names <- gsub("[\\(\\)-]", "", names(data))


# then I substitute abbreviations for explicit names
names <- gsub("^f", "FrequencyDomain", names)
names <- gsub("^t", "TimeDomain", names)
names <- gsub("Acc", "Accelerometer", names)
names <- gsub("Gyro", "Gyroscope", names)
names <- gsub("Mag", "Magnitude", names)
names <- gsub("Freq", "Frequency", names)
names <- gsub("mean", "Mean", names)
names <- gsub("std", "StandardDeviation", names)

# finally, I set the new column names of the data table
names(data) <- names

rm(names)

# 6 creates tidy data set -------------------------------------------------
# this data set has the average of each variable for each activity and each subject

tidy_data <- data %>% 
        group_by(subject, activity) %>%
        summarise_each(funs(mean))

# write tidy_data in .csv

write_csv(tidy_data, "tidy_data.csv")
