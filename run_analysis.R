# Load all needed libraries
library(dplyr)

# Location of the data folder. We set it here to simplify the location so that
# we only need to change a single line if we want to change the folder name
datafolder <- "data"

# Prefix to assign to the variables to ensure consistency across all functions
prefix <- "fv"

# Download the data for analysis
download_data <- function() {
  src = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
  dest = "har_dataset.zip"
  
  cat("Downloading", src, "to", dest, "...\n\n")

    # Check if the file already exists, if yes do nothing
  if (!file.exists(dest)) {
    download.file(src, dest, method = "curl")
  }
  
  cat("Downloaded file saved as:", dest, "\n")
  dest
}

# Extracts the data and places it in the folder called "data"
extract_data <- function(zipfile = "har_dataset.zip", dest = datafolder) {
  
  srcfolder = "UCI HAR Dataset"
  
  # if the data folder exists do nothing
  if (!file.exists(dest)) {
    cat("\nExtracting", zipfile, "to data...\n")

    unzip(zipfile)
    
    if (!file.exists(srcfolder)) {
      stop("Unable to find the extracted data folder: ", zipfile)
    }
    
    file.rename(srcfolder, dest)
  }
  cat("Data extracted into folder:", dest, "\n")
  dest
}

# Loads the test data set into a data frame
load_test <- function(data_folder = datafolder, 
                      folder = "test",
                      var_prefix = prefix) {
  
  # create OS independent path
  path <- file.path(data_folder, folder)
  cat("Loading test data from folder:", path, "\n")
  
  # load test subjects, x data, y data
  subjects <- file.path(path, "subject_test.txt") %>% read.table(col.names = c("subject"))
  x <- file.path(path, "X_test.txt") %>% read.table
  y <- file.path(path, "Y_test.txt") %>% read.table(col.names = c("activity"))
  
  # Rename to x columns to fv1, fv2 ... 
  colcount <- ncol(x)
  vars <- paste(var_prefix, 1:colcount, sep = "")
  colnames(x) <- vars
  
  # return the combined data frame
  data.frame(subjects, y, x)
}

# Loads the training data set into a data frame
load_train <- function(data_folder = datafolder, 
                       folder = "train",
                       var_prefix = prefix) {
  
  # create OS independent path
  path = file.path(data_folder, folder)
  cat("Loading training data from folder:", path, "\n")
  
  # load test subjects, x data, y data
  subjects <- file.path(path, "subject_train.txt") %>% read.table(col.names = c("subject"))
  
  x <- file.path(path, "X_train.txt") %>% read.table
  y <- file.path(path, "Y_train.txt") %>% read.table(col.names = c("activity"))
  
  # Rename to x columns to fv1, fv2 ... 
  colcount <- ncol(x)
  vars <- paste(var_prefix, 1:colcount, sep = "")
  colnames(x) <- vars
  
  # return the combined data frame
  data.frame(subjects, y, x)
}

# Combine 2 data frame rows
append_rows <- function(df1, df2) {
  rbind(df1, df2)
}

# Read the activity list and return it as a factor
load_activity_factors <- function(data_folder = datafolder, 
                                  src = "activity_labels.txt") {
  
  cat("Loading list of activity levels from:", src, "\n")
  read.table(file.path(data_folder, src), col.names = c("level", "label"))
}

# Load the list of measurement labels as defined by the experiment
load_features <- function(data_folder = datafolder, src = "features.txt") {
  
  # create OS independent path
  path = file.path(data_folder, src)
  cat("Loading list of measurement names from:", path, "\n")

  df <- read.table(path, colClasses = c("numeric", "character"))
  colnames(df) <- c("index", "feature")
  
  df
}

# Function to find list of columns where the measure is "mean()" or "std()"
# NB: we need to escape the '(' with 2 backslashes '\\' or else the 
# parentheses is considered a grep capture group
# We pass the list of features loaded from the function load_features
find_mean_std <- function(features, column_name = "feature") {
  cat("Finding variables with the terms mean() or std() in their names...\n")
  indexes <- grep("mean\\(\\)|std\\(\\)", features[[column_name]])
  features[indexes, ]
}

# Renames the variables to consistent and meaningful format. We use the 
# following format:
# - All characters must be lower case
# - Words are separated by underscore ("_")
# - tf prefix is replaced by time and freq(short for frequency)
# - Suffix is added to show the function used either mean or sd (for standard deviation)
# - Suffix is also added to show the axis involved either X, Y, or Z. If no axis
#   is involved then there will be no _x, _y or _z suffix
rename_variables <- function(features) {
  
  cat("Renaming variables to be more meaningful...\n")
  
  # use grep to replace the variable names   
  features %>% 
    sub(pattern = "^t", replacement =  "time_") %>% 
    sub(pattern = "^f", replacement = "freq_") %>%
    gsub(pattern = "Body", replacement = "body_") %>%
    gsub(pattern = "Acc", replacement = "accel_") %>%
    gsub(pattern = "Gyro", replacement = "gyro_") %>%
    gsub(pattern = "Gravity", replacement = "gravity_") %>%
    gsub(pattern = "Jerk", replacement = "jerk_") %>%
    gsub(pattern = "Mag", replacement = "mag_") %>%
    sub(pattern = "-mean\\(\\)", replacement = "mean") %>%
    sub(pattern = "-std\\(\\)", replacement = "sd") %>%
    sub(pattern = "\\-X", replacement = "_x") %>%
    sub(pattern = "\\-Y", replacement = "_y") %>%
    sub(pattern = "\\-Z", replacement = "_z")
  
  # result
}

# Summarizes the data by subject, by activity
subject_activity_summary <- function(src_data) {
  
  cat("Creating summary for each subject and activity per subject...\n")
  
  # convert to dplyr table
  tbl <- tbl_df(src_data)
  
  # group by subject, activity
  subj_activity_group <- tbl %>% group_by(subject, activity)
  
  # summarize by subject, activity and calculate the mean
  subj_activity_group %>% summarise_all(mean)
  
}

# This is the main function of the script. It will automatically execute all
# the other functions. 
main <- function() {
  
  # download and extract data
  download_data()
  extract_data()
  
  # load test and training datda
  test_data <- load_test()
  training_data <- load_train()
  
  # combine test and training data
  cat("Combine rows in test & training data\n")
  merged_data <- append_rows(test_data, training_data) 
  # *** COMPLETE #1 OF ASSIGNMENT ***
  
  # load the list of variables
  features <- load_features()
  
  # get list of columns indexes with mean / sd . we then use this to 
  # subset from our merged ata
  mean_std_columns <- find_mean_std(features)
  
  # subset the columns to only select subject, activity and columns returned
  # from find_mean_std
  cols <- c("subject", "activity", paste(prefix, mean_std_columns[[1]], 
                                         sep = ""))
  mean_std_data <- merged_data[, cols]
  # *** COMPLETE #2 OF ASSIGNMENT ***
  
  # load the list of activity as factors
  activity_factors <- load_activity_factors()
  
  # convert the activity column to factor with the necessary label
  mean_std_data["activity"] <-factor(mean_std_data[["activity"]], 
                                     activity_factors$level, 
                                     activity_factors$label)
  # *** COMPLETE #3 OF ASSIGNMENT ***
  
  # process the data from mean_std_columns to give meaningful variable names
  m <- rename_variables(mean_std_columns$feature)

  # update the column names for the table
  colnames(mean_std_data) <- c("subject", "activity", m)
  # *** COMPLETE #4 OF ASSIGNMENT ***
  
  # create the summary
  summarized_data <- subject_activity_summary(mean_std_data)
  
  # write the result to disk
  outfile <- "tidy_data.txt"
  
  # remove any existing result file
  if (file.exists(outfile)) {
    stop("Please remove existing ", outfile, " from ", getwd(),
         " to be able to save the results of the analysis\n")
  }
  write.table(summarized_data, outfile, row.names = FALSE)
  
  cat("Analysis complete\n\n")
  
}