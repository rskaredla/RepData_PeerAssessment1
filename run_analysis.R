# 18 Feb 2015
# Here are the data for the project: 
#  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
### download files - one time only####
#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="Dataset.zip")

### read training data ####
train.data <- read.table("UCI HAR Dataset/train/X_train.txt")
train.volunteer <- read.table("UCI HAR Dataset/train/subject_train.txt")
train.act.label <- read.table("UCI HAR Dataset/train/Y_train.txt")
for (i in 1:dim(train.act.label)[1]){
  if (train.act.label[i,1]=="1") {
    train.act.label[i,1]="WALKING"
  } else if(train.act.label[i,1]=="2") {
    train.act.label[i,1]="WALKING_UPSTAIRS"
  } else if(train.act.label[i,1]=="3") {
    train.act.label[i,1]="WALKING_DOWNSTAIRS"
  } else if(train.act.label[i,1]=="4") {
    train.act.label[i,1]="SITTING"
  } else if(train.act.label[i,1]=="5") {
    train.act.label[i,1]="STANDING"
  } else if(train.act.label[i,1]=="6") {
    train.act.label[i,1]="LAYING"
  }
}


### read test data ####
test.data <- read.table("UCI HAR Dataset/test/X_test.txt")
test.volunteer <- read.table("UCI HAR Dataset/test/subject_test.txt")
test.act.label <- read.table("UCI HAR Dataset/test/Y_test.txt",colClasses="character")
for (i in 1:dim(test.act.label)[1]){
  if (test.act.label[i,1]=="1") {
    test.act.label[i,1]="WALKING"
  } else if(test.act.label[i,1]=="2") {
    test.act.label[i,1]="WALKING_UPSTAIRS"
  } else if(test.act.label[i,1]=="3") {
    test.act.label[i,1]="WALKING_DOWNSTAIRS"
  } else if(test.act.label[i,1]=="4") {
    test.act.label[i,1]="SITTING"
  } else if(test.act.label[i,1]=="5") {
    test.act.label[i,1]="STANDING"
  } else if(test.act.label[i,1]=="6") {
    test.act.label[i,1]="LAYING"
  }
}

### compare vectors in test & training to see if they are same ###
dim(test.data) == dim(train.data)
dim(test.volunteer) == dim(train.volunteer)

### Prepare for binding ###
names(train.data)[1] <- "V1"
names(train.act.label) <- "Activity"
names(train.volunteer) <- "Volunteer"

names(test.data)[1] <- "V1"
names(test.act.label) <- "Activity"
names(test.volunteer) <- "Volunteer"

### bind training & test data
train.join <- cbind(train.act.label,train.volunteer,train.data)
test.join <- cbind(test.act.label,test.volunteer,test.data)
train.test <- rbind(train.join,test.join)

### Extract Mean & std() ####
# As specified by the TA David Hoop in the forum -  "David's Course Project FAQ" :
#    There are no specific marking criteria on the number of columns. 
#    It is up to you to make a decision and explain what you did to the data.
#    Make it easy for people to give you marks by explaining your reasoning.
# I have taken the first 20 mean & their stds
# Specify the mean positions & titles
pos.mean.std <- c(1,2,         #activities & Volunteers
              1,2,3,       #location of mean & std starts here : ids as in features
              4,5,6,
              
              41,42,43,
              44,45,46,
              
              81,82,83,
              84,85,86,
              
              121,122,123,
              124,125,126,
              
              161,162,163,
              164,165,166,
              
              201,
              202,
              
              214,
              215,
              
              227,
              228,
              
              240,
              241,
              
              253,
              254)
#  shift the column by 2 due to addition of activities & Volunteers title
for (i in 3:length(pos.mean.std)){
  pos.mean.std[i] = pos.mean.std[i] + 2
}
title.mean.std <-c("Activity","Volunteer",
              "tBodyAcc-mean-X","tBodyAcc-mean-Y","tBodyAcc-mean-Z",
               "tBodyAcc-std-X","tBodyAcc-std-Y","tBodyAcc-std-Z",
               
               "tGravityAcc-mean-X","tGravityAcc-mean-Y","tGravityAcc-mean-Z",
               "tGravityAcc-std-X","tGravityAcc-std-Y","tGravityAcc-std-Z",
               
               "tBodyAccJerk-mean-X","tBodyAccJerk-mean-Y","tBodyAccJerk-mean-Z",
               "tBodyAccJerk-std-X","tBodyAccJerk-std-Y","tBodyAccJerk-std-Z",
               
               "tBodyGyro-mean-X","tBodyGyro-mean-Y","tBodyGyro-mean-Z",
               "tBodyGyro-std-X","tBodyGyro-std-Y","tBodyGyro-std-Z",
               
               "tBodyGyroJerk-mean-X","tBodyGyroJerk-mean-Y","tBodyGyroJerk-mean-Z",
               "tBodyGyroJerk-std-X","tBodyGyroJerk-std-Y","tBodyGyroJerk-std-Z",
               
               "tBodyAccMag-mean",
               "tBodyAccMag-std",
               
               "tGravityAccMag-mean",
               "tGravityAccMag-std",
               
               "tBodyAccJerkMag-mean",
               "tBodyAccJerkMag-std",
               
               "tBodyGyroMag-mean",
               "tBodyGyroMag-std",
               
               "tBodyGyroJerkMag-mean",
               "tBodyGyroJerkMag-std")

# Subset the Mean & Std variables
subset.mean.std = train.test[,pos.mean.std]
# Set Names 
names(subset.mean.std) <- title.mean.std
#  average of each variable for each activity and each subject
mean.of.meanstd <- aggregate(subset.mean.std[,3:42],by=list(subset.mean.std$Activity,subset.mean.std$Volunteer),mean)

write.table(mean.of.meanstd,file="out_project_g-c_data.csv",row.name=FALSE)