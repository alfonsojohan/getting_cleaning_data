# Load all needed libraries
library(dplyr)

# Location of the data folder
datafolder <- "data"

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
                      folder = "test" ) {
  
  # create OS independent path
  path <- file.path(data_folder, folder)
  cat("Loading test data from folder:", path, "\n")
  
  # load test subjects, x data, y data
  subjects <- file.path(path, "subject_test.txt") %>% read.table(col.names = c("subject"))
  x <- file.path(path, "X_test.txt") %>% read.table
  y <- file.path(path, "Y_test.txt") %>% read.table(col.names = c("activity"))
  
  # Rename to x columns to fv1, fv2 ... 
  colcount <- ncol(x)
  vars <- paste("fv", 1:colcount, sep = "")
  colnames(x) <- vars
  
  # return the combined data frame
  data.frame(subjects, y, x)
}

# Loads the training data set into a data frame
load_train <- function(data_folder = datafolder, folder = "train") {
  
  # create OS independent path
  path = file.path(data_folder, folder)
  cat("Loading training data from folder:", path, "\n")
  
  # load test subjects, x data, y data
  subjects <- file.path(path, "subject_train.txt") %>% read.table(col.names = c("subject"))
  
  x <- file.path(path, "X_train.txt") %>% read.table
  y <- file.path(path, "Y_train.txt") %>% read.table(col.names = c("activity"))
  
  # Rename to x columns to fv1, fv2 ... 
  colcount <- ncol(x)
  vars <- paste("fv", 1:colcount, sep = "")
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
  lbls <- read.table(file.path(data_folder, src))
  factor(lbls)
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

# Function to find list of columns where the measure is "mean" or "std"
# We pass the list of features loaded from the function load_features
find_mean_std <- function(features, column_name = "feature") {
  cat("Finding variables with the terms mean() or std() in their names...\n")
  indexes <- grep("mean\\(\\)|std\\(\\)", features[[column_name]])
  features[indexes, ]
}

main <- function() {
  
  download_data()
  extract_data()
  
  activity_factors <- load_activity_factors()
  test_data <- load_test()
  training_data <- load_train()
  
  cat("Combine rows in test & training data\n")
  merged_data <- append_rows(test_data, training_data) 
  
  features <- load_features()
  mean_std_columns <- find_mean_std(features)
  
  # load as dplyr table
  data <- tbl_df(merged_data)
  

  cat("Analysis complete\n\n")
  
}