#0. Get data from url
getData <- function ()
{
  URL <-
    "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  destfile <- "dataset.zip"
  
  if (!file.exists(destfile))
  {
    download.file(URL, destfile)
    file <- unzip(destfile)
    filename <- unlist(strsplit(file[1], "/"))[2]
  }
  else
    "UCI HAR Dataset"
}
#1. Merges the training and the test sets to create one data set.
MergeData <- function()
{
  foldername <- getData()
  
  #read features
  features <- read.table(paste0(foldername, "/features.txt"))
  
  #read train dataset and set column names
  train_data <-
    read.table(paste0(foldername, "/train/X_train.txt"), col.names = features$V2)
  train_labels <-
    read.table(paste0(foldername, "/train/y_train.txt"), col.names = "activity")
  train_subject <-
    read.table(paste0(foldername, "/train/subject_train.txt"), col.names = "subjectID")
  
  #read test dataset and set column names
  test_data <-
    read.table(paste0(foldername, "/test/X_test.txt"), col.names = features$V2)
  test_labels <-
    read.table(paste0(foldername, "/test/y_test.txt"), col.names = "activity")
  test_subject <-
    read.table(paste0(foldername, "/test/subject_test.txt"), col.names = "subjectID")
  
  #merge each type
  merge_data <- rbind(train_data, test_data)
  merge_labels <- rbind(train_labels, test_labels)
  merge_subject <- rbind(train_subject, test_subject)
  
  #combine 3 types
  mergeAll <- cbind(merge_subject, merge_labels, merge_data)
  
  
}
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
data_mean_std <-
  mergeAll[, c(grepl("[Mm]ean.*\\(|[Ss]td.*\\(", features$V2), TRUE, TRUE)]

#3. Uses descriptive activity names to name the activities in the data set
foldername <- getData()
activity_label <-
  read.table(paste0(foldername, "/activity_labels.txt"))
data_mean_std$activity_label <-
  factor(data_mean_std$activity,
         levels = activity_label$V1,
         labels = activity_label$V2)

#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
CombinedData.melted <- melt(CombinedData, id = c("subject", "activity"))

CombinedData.mean <- dcast(CombinedData.melted, 
                           subject + activity ~ variable, mean)