#Loading required libraries
library(dplyr)

#Downloading the data from the given source and unzipping it
#data will be downloaded and unzipped in the working directory
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "data.zip")
unzip("data.zip")

#From the unzipped file, importing all of the data files in different variables
dffeatures <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
dfactivitylabel <- read.table("UCI HAR Dataset/activity_labels.txt", 
                              stringsAsFactors = FALSE)
dfsubtrain <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                         stringsAsFactors = FALSE)
dfxtrain <- read.table("UCI HAR Dataset/train/X_train.txt", stringsAsFactors = FALSE)
dfytrain <- read.table("UCI HAR Dataset/train/Y_train.txt", stringsAsFactors = FALSE)
dfsubtest <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                        stringsAsFactors = FALSE)
dfxtest <- read.table("UCI HAR Dataset/test/X_test.txt", stringsAsFactors = FALSE)
dfytest <- read.table("UCI HAR Dataset/test/Y_test.txt", stringsAsFactors = FALSE)

#Merging the subject, X, and Y, train and test tables in one data frame 
#Merging is done by column binding all data tables of train and same for data tables
# of test, then row binding both to create one merged data frame 
mergeddata <- rbind(cbind(dfsubtrain, dfytrain, dfxtrain), cbind(dfsubtest, dfytest, 
                                                                 dfxtest))

#Removing all variables that won't be needing again to free up memory
rm(list = ls()[!ls() %in% c("mergeddata", "dffeatures", "dfactivitylabel")])

#From merged data, extracting columns which represents data for mean and 
#standard deviation along with subject and activity columns
datatobeextracted <- c(1,2,grep("mean\\(\\)|std\\(\\)", dffeatures[[2]]) + 2)
extracteddata <- mergeddata[, datatobeextracted]

#Labelling the activities in activity column of extracted data from activity label
#data frame
extracteddata[[2]] <- factor(extracteddata[[2]], levels = dfactivitylabel[[1]],
                             labels = dfactivitylabel[[2]])

#Labelling the colummns of extracted data frame
variablenames <- dffeatures[,2][datatobeextracted[-c(1,2)] - 2]
names(extracteddata) <- c("subject", "activity", variablenames)

#Making a tidy data set that has mean of each variable for each subject and each 
#activity
tidy.data <- extracteddata %>%
              group_by(subject, activity) %>%
              summarise_all(funs(mean)) %>%
              ungroup()

#Renaming the columns of tidy data by adding average in the initial of each variable
names(tidy.data)[-c(1,2)] <- paste("Average-", names(tidy.data)[-c(1,2)], sep = "")

#Creating the tidy data set table with name tidy_data.txt in working directory
write.table(tidy.data, "tidy_data.txt")

#Removing all variables
rm(list = ls())
