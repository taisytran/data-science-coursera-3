
getDataOnline <- function (URL, destfile)
{
  print("Download Data")
  if (!file.exists(destfile))                             # Check if the file already exists
  {
    download.file(URL, destfile)             # Download data
    file <- unzip(destfile)                         # Unzip the file
    folder <- unlist(strsplit(file[1],"/"))[2]      # Gets the folder's name of the unzipped file
  }
  else "UCI HAR Dataset"                                  # Returns the folder name
}

