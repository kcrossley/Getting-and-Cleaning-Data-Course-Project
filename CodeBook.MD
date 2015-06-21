## Getting and Cleaning Data Project

Kelly Crossley

### Description
Additional information about the variables, data and transformations in the Johns Hopkins Getting and Cleaning Data course project script.

### Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smart-phone (Samsung Galaxy S II) on the waist. 
Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The experiments have been video-recorded to label the data manually. 
The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Section 1. Merge the training and the test sets to create one data set.
After setting the source directory for the files, read into tables the data located in each of the directories

Training Data -- Processes:
- Read in the data from files
- Import the X training set
- Import the X training set
- Import the Activity Labels file
- Import the subject file
- Import features.txt 
- Assign column names from the data imported             
- Create the final training set by merging the subject_test, xtest and ytest files

Training Data -- Variables:
- xTrain = x_train.txt
- yTrain = y_train.txt
- features  = ./features.txt 
- subjectTrain = subject_train.txt
- activityType = activity_labels.txt
- trainingData = the merged dataset

End of training data
	
	Test data
		--  Read in the data from the flat files
		  -- xTest = x_test.txt
		  -- yTest = y_test.txt
		  -- subjectTest = subject_test.txt		 
		--  Assign column names to the test data imported above
			--  Import the subject file
			--  import the X Test set
			--  import the Y Test set
		   
		--  Create the final test set by merging the xTest, yTest and subjectTest data
			-- testData = final test set			
		--  Merge the 2 datasets to create a single dataset
			-- MergedData = combined data		
		--  Create a vector for the column names from the MergedData , which will be used
	End of Test Data
	
### Section 2. Extract only the measurements on the mean and standard deviation for each measurement. 
    --   Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
    --   Subset MergedData table based on the logicalVector to keep only desired columns	
	
### Section 3. Use descriptive activity names to name the activities in the data set
	--   Merge the MergedData set with the acitivityType table to include descriptive activity names
	--   Update the columnNames vector to include the new column names after merge

### Section 4. Appropriately label the data set with descriptive activity names.
	--   Updating the columnNames vector to include the new column names after merge
	--   Reassign the new descriptive column names to the MergedData set

### Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
Per the project instructions, we need to produce only a data set with the average of each variable for each activity and subject

--   Create a new table, MergedDataNoActivityType without the activityType column
--   Summarizing the MergedDataNoActivityType table to include just the mean of each variable for each activity and each subject
--   Merging the tidyData with activityType to include descriptive activity names
--   Export the tidyData set 

Variables
	--  MergedDataWithoutActivityType
	--  tidyData
	