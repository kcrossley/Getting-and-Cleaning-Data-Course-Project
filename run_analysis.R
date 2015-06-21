## ---#---#---#---#---# Coursera Getting and Cleaning Data Course Project  ---#---#---#---#---#---#

## Kelly Crossley
## Date: 06/15/15
#
# runAnalysis.r File Description:
#
# This script will perform the following actions:
#
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#
# Data is downloaded from  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
## ---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#

# 1. Merge the training and the test sets to create one data set.

    #set working director
    setwd('/ML/GettingAndCleaningCP');

    ## ---#---#  TRAINING Set Data #---#---# 
    
        # Read in the data from files
          # import the X training set
            xTrain       = read.table('./train/x_train.txt',header=FALSE);
          # import the X training set
            yTrain       = read.table('./train/y_train.txt',header=FALSE);
          # Import the Activity Labels file
            activityType = read.table('./activity_labels.txt',header=FALSE);
          # Import the subject file
            subjectTrain = read.table('./train/subject_train.txt',header=FALSE);
          # Import features.txt
            features     = read.table('./features.txt',header=FALSE);
        
        # Assign column names from the data imported
          columnNames(yTrain)        = "activityId";
          columnNames(subjectTrain)  = "subjectId";
          columnNames(xTrain)        = features[,2]; 
          columnNames(activityType)  = c('activityId','activityType');
              
        # Create the final training set by merging the subject_test, xtest and ytest files
          trainingData = cbind(yTrain,subjectTrain,xTrain);
    
    ## ---#---#  End TRAINING Set Data #---#---# 

    ## ---#---#  TEST Set Data #---#---# 

    # Read in the data from the flat files
      xTest       = read.table('./test/x_test.txt',header=FALSE);
      yTest       = read.table('./test/y_test.txt',header=FALSE);
      subjectTest = read.table('./test/subject_test.txt',header=FALSE);
     
    # Assign column names to the test data imported above
        # Import the subject file
          columnNames(subjectTest) = "subjectId";
        # import the X Test set
        columnNames(xTest)       = features[,2]; 
        # import the Y Test set
        columnNames(yTest)       = "activityId";
       
    # Create the final test set by merging the xTest, yTest and subjectTest data
        testData = cbind(yTest,subjectTest,xTest);
        
    # Merge the 2 datasets to create a single dataset
        MergedData = rbind(trainingData,testData);
    
    # Create a vector for the column names from the MergedData , which will be used
    # to select the desired mean() & stddev() columns
      columnNames  = columnNames(MergedData); 

    ## ---#---#  TEST Set Data #---#---# 

# __!__!__!  End of Section 1  __!__!__!
# ---------------------------------------------------------------------------------------------------------------

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
    
    # Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
    logicalVector = (grepl("activity..",columnNames) | grepl("subject..",columnNames) | grepl("-mean..",columnNames) & !grepl("-meanFreq..",columnNames) & !grepl("mean..-",columnNames) | grepl("-std..",columnNames) & !grepl("-std()..-",columnNames));
    
    # Subset MergedData table based on the logicalVector to keep only desired columns
    MergedData = MergedData[logicalVector==TRUE];

# __!__!__!  End of Section 2  __!__!__!
# ---------------------------------------------------------------------------------------------------------------

# 3. Use descriptive activity names to name the activities in the data set

    # Merge the MergedData set with the acitivityType table to include descriptive activity names
    MergedData = merge(MergedData,activityType,by='activityId',all.x=TRUE);
    
    # Updating the columnNames vector to include the new column names after merge
    columnNames  = columnNames(MergedData); 


# __!__!__!  End of Section 3  __!__!__!
# ---------------------------------------------------------------------------------------------------------------


# 4. Appropriately label the data set with descriptive activity names. 

    # Cleaning up the variable names
    for (i in 1:length(columnNames)) 
    {
      columnNames[i] = gsub("\\()","",columnNames[i])
      columnNames[i] = gsub("-std$","StdDev",columnNames[i])
      columnNames[i] = gsub("-mean","Mean",columnNames[i])
      columnNames[i] = gsub("^(t)","time",columnNames[i])
      columnNames[i] = gsub("^(f)","freq",columnNames[i])
      columnNames[i] = gsub("([Gg]ravity)","Gravity",columnNames[i])
      columnNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnNames[i])
      columnNames[i] = gsub("[Gg]yro","Gyro",columnNames[i])
      columnNames[i] = gsub("AccMag","AccMagnitude",columnNames[i])
      columnNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columnNames[i])
      columnNames[i] = gsub("JerkMag","JerkMagnitude",columnNames[i])
      columnNames[i] = gsub("GyroMag","GyroMagnitude",columnNames[i])
    };
    
    # Reassigning the new descriptive column names to the MergedData set
    columnNames(MergedData) = columnNames;

# __!__!__!  End of Section 4  __!__!__!
# ---------------------------------------------------------------------------------------------------------------


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

    # Create a new table, MergedDataNoActivityType without the activityType column
    MergedDataNoActivityType  = MergedData[,names(MergedData) != 'activityType'];
    
    # Summarizing the MergedDataNoActivityType table to include just the mean of each variable for each activity and each subject
    tidyData    = aggregate(MergedDataNoActivityType[,names(MergedDataNoActivityType) != c('activityId','subjectId')],
                            by=list(activityId=MergedDataNoActivityType$activityId,subjectId = MergedDataNoActivityType$subjectId),mean);
    
    # Merging the tidyData with activityType to include descriptive acitvity names
    tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);
    
    # Export the tidyData set 
    write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

# __!__!__!  End of Section 5  __!__!__!
# ---------------------------------------------------------------------------------------------------------------
