# Data Science - Getting & Cleaning Data
#### Peer Graded Assignment #2

## README

### Introduction

The file `run_analysis.R` contains all the functions needed to complete the
assignment. The tasks set out in the assignment are:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each 
   measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with 
the average of each variable for each activity and each subject.

### Understanding the Script

The primary function in the script that you need to run to complete all the 
above steps mentioned is called `main()`

#### Function `main()`

This is the only function that you need to run. This function will call all 
the other functions to complete the analysis. The steps taken are:

1. Download the data set from the given url
2. Extract the data set to the sub folder: `data`
3. Load the test and training data
4. Merge the test and training data

**This completes #1 of the assignment**

5. Load the measurement details (variables)
6. Subset for those related to mean and standard deviation only

**This completes #2 of the assignment**

7. Load the list of activity levels
8. Assign as factor with labels from activity levels to the activity column 
in the dataset 

**This completes #3 of the assignment**

9. Load the list of activity labels for the data set
10. Process the activity labels to a consistent format (see Codebook for detailed
explanation)
11. Update the variable column names with the meaningful name

**This completes #4 of the assignment**

12. Create a summary of the resulting data to calculate the mean for each 
variable by subject and by activity. 
13. Save the result from step 12 to text file called *tidy_data.txt*. **NB: If there
is an existing file with the name *tidy_data.txt* in the working directory the function 
will exit with error. Please delete or move the file to allow it to finish**

**This completes #5 of the assignment**

### Description of other functions in the file by alphabetical order

#### Function `append_rows(df1, df2)`

Combine 2 data frame rows and returns the combined data frame.
 
#### Function `download_data()`

Download the data for analysis from <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

#### Function `extract_data(zipfile = "har_dataset.zip", dest = datafolder)`

Extracts the data and places it in the sub folder called *data* in the current
working directory

#### Function `find_mean_std(features, column_name = "feature")`

Function to find list of columns where the measure is "mean()" or "std()"
We pass the list of features loaded from the function load_features

#### Function `load_activity_factors(data_folder = datafolder, src = "activity_labels.txt")`

Read the activity list from *.\data\activity_labels.txt* and return it as a factor

#### Function `load_features(data_folder = datafolder, src = "features.txt")`

Load the list of measurement labels as defined by the experiment

#### Function `load_test <- function(data_folder = datafolder, folder = "test", var_prefix = prefix)`

Loads the test data set into a data frame

#### Function `load_train(data_folder = datafolder, folder = "train", var_prefix = prefix)`

Loads the training data set into a data frame

#### Function `rename_variables(features)`

Renames the variables to consistent and meaningful format. We use the 
following format:

1. All characters must be lower case
2. Words are separated by underscore ("_")
3. t/f prefix is replaced by time and freq(short for frequency)
4. Suffix is added to show the function used either mean or sd (for standard deviation)
5. Suffix is also added to show the axis involved either X, Y, or Z. If no axis
   is involved then there will be no _x, _y or _z suffix

#### Function `subject_activity_summary(src_data)`

Summarizes the data by subject, by activity