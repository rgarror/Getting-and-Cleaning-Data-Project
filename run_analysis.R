####################################################################################
#Getting and Cleaning Data Course Project
#Ricardo Garro

#The data for the project is taken from:
  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#This R script called run_analysis.R, does the following: 

#     1.Merges the training and the test sets to create one data set.
#     2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#     3.Uses descriptive activity names to name the activities in the data set
#     4.Appropriately labels the data set with descriptive variable names. 
#     5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#####################################################################################

# Clean up workspace
rm(list=ls())

# Set working directory 
setwd('C:/Users/Admin/Dropbox/Coursera/Data Science Specialization/03 - Cleaning and Obtaining Data/Project/Data/UCI HAR Dataset')
getwd()

# 1. Merge the training and the test sets to create one data set.


# Read in the identifying information 
features = read.table('./features.txt',header=FALSE); #imports features.txt
activity_labels = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt

#Read in the Train Data
subject_train = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
x_train = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
y_train = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Setting column names to Train data
colnames(activity_labels) = c('Activity_Id','Activity_Type');
colnames(subject_train) = "Subject_Id";
colnames(x_train) = features[,2];
colnames(y_train) = "Activity_Id";

# Merge dataframes to create training set table 
training_data = cbind(y_train,subject_train,x_train);

# Read in the test data
subject_test = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
x_test = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
y_test = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Setting column names to Test data
colnames(subject_test) = "Subject_Id";
colnames(x_test) = features[,2];
colnames(y_test) = "Activity_Id";

# Merge dataframes to create test set table 
test_data = cbind(y_test,subject_test,x_test);


# Combine training and test data 
finalData = rbind(training_data,test_data);

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("Activity..",colnames(finalData)) | grepl("Subject..",colnames(finalData)) | grepl("-mean..",colnames(finalData)) & !grepl("-meanFreq..",colnames(finalData)) & !grepl("mean..-",colnames(finalData)) | grepl("-std..",colnames(finalData)) & !grepl("-std()..-",colnames(finalData)));
                      
# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData$Activity_Type <- ifelse(finalData$Activity_Id==1, "WALKING",
               ifelse(finalData$Activity_Id==2, "WALKING_UPSTAIRS",
                      ifelse(finalData$Activity_Id==3, "WALKING_DOWNSTAIRS",
                             ifelse(finalData$Activity_Id==4, "SITTING",
                                    ifelse(finalData$Activity_Id==5, "STANDING",
                                           ifelse(finalData$Activity_Id==6, "LAYING",
                                    NA  )))))) # all other values map to NA


      #finalData = merge(finalData,activity_labels,by='Activity_Id',all=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames = colnames(finalData);

# 4. Appropriately label the data set with descriptive activity names.

# Cleaning up the variable names
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType = finalData[,names(finalData) != 'Activity_Type'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('Activity_Id','Subject_Id')],by=list(Activity_Id=finalDataNoActivityType$Activity_Id,Subject_Id = finalDataNoActivityType$Subject_Id),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData$Activity_Type <- ifelse(tidyData$Activity_Id==1, "WALKING",
                                  ifelse(tidyData$Activity_Id==2, "WALKING_UPSTAIRS",
                                         ifelse(tidyData$Activity_Id==3, "WALKING_DOWNSTAIRS",
                                                ifelse(tidyData$Activity_Id==4, "SITTING",
                                                       ifelse(tidyData$Activity_Id==5, "STANDING",
                                                              ifelse(tidyData$Activity_Id==6, "LAYING",
                                                                     NA  )))))) # all other values map to NA


      #tidyData = merge(tidyData,activityType,by='Activity_Id',all.x=TRUE);

# Export the tidyData set
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');
