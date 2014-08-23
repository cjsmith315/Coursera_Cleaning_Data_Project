tidyset<-function(){
  ##Load the data assuming that we are in the same wd as our data
training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Load features in and get rid of '-' signs....random signs could cause some issues
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# Combine test and training data
main_data <- rbind(training, testing)

# Pull the mean and standard deviation out
z <- grep(".*Mean.*|.*Std.*", features[,2])
# Subset features table
features <- features[z,]
# Adds subject and activity
z <- c(z, 562, 563)
# Subset main_data on z to get final_data
final_data <- main_data[,z]
# Rewrite column names
colnames(final_data) <- c(features$V2, "Activity", "Subject")
colnames(final_data) <- tolower(colnames(final_data))
##Set activity to be a sequential numbering system
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  final_data$activity <- gsub(currentActivity, currentActivityLabel, final_data$activity)
  currentActivity <- currentActivity + 1
}

final_data$activity <- as.factor(final_data$activity)
final_data$subject <- as.factor(final_data$subject)

tidy = aggregate(final_data, by=list(activity = final_data$activity, subject=final_data$subject), mean)
tidy[,90] = NULL
tidy[,89] = NULL
write.table(tidy, "tidy.txt", sep="|")
}