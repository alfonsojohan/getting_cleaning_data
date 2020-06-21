# Codebook

## Introduction

The source data of this analysis comes from the experiment Human Activity 
Recognition Using Smartphones Data Set. You can read the details from:

<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

## Data Dictionary

There are 68 columns in the output of this analysis. 2 are factors and remaining 
66 are variables.

There are 180 rows (observations) in the resulting analysis.

The 2 factors are describe below.

1. subject - Unique identifier of the subject doing the activity
2. activity - One of the following activity:
    + WALKING
    + WALKING_UPSTAIRS
    + WALKING_DOWNSTAIRS
    + SITTING
    + STANDING
    + LAYING

### Understanding Variable Names

There are 66 variables in the result. Each variable is named in a specific format
to clearly identify the measurement. Each key word is seperated by an
underscore ( _ ) for legibility. The keywords are:

1. time - Prefix. Denotes a measurement in the time domain signals
2. freq - Prefix. Fast Fourier Transform has been applied to the measurement signal
3. body - Body acceleration signals
4. accel - Accelerometer sensor signal measurement
5. gyro - Gyroscope sensor signal measurement
6. gravity - Sensor acceleration signal gravitational component
7. jerk - Jerk signal derived from body linear acceleration and angular velocity
8. mag - Magnitude of the three-dimensional signals calculated using the Euclidean norm
9. mean - Estimated mean value of the signal
10. sd - Estimated standard deviation of the signal
11. x,y,z - Suffix (Optional). Denoting the axis of the signal.

## Data Transformations & Cleaning

To arrive at the analysis results the following actions was done on the data set.

1. Download the data set from the given url
2. Extract the data set to the sub folder: `data`
3. Load the test and training data
4. Merge the test and training data
5. Load the measurement details (variables)
6. Subset for those related to mean and standard deviation only
7. Load the list of activity levels
8. Assign as factor with labels from activity levels to the activity column 
in the dataset 
9. Load the list of activity labels for the data set
10. Process the activity labels to a consistent format (see Codebook for detailed
explanation)
11. Update the variable column names with the meaningful name
12. Create a summary of the resulting data to calculate the mean for each 
variable by subject and by activity. 
13. Save the result from step 12 to text file called *tidy_data.txt*. 

    **NB: If there is an existing file with the name *tidy_data.txt* in the working directory the function 
    will exit with error. Please delete or move the file to allow it to finish**

