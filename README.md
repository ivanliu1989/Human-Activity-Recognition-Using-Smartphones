###README: Human-Activity-Recognition-Using-Smartphones
#####This analysis performs a data cleansing process and generate tidy datasets at the end. Specifically, it follows following steps:

#####1. Merges the training and the test sets to create one data set.


```r
#acquire url and name of raw dataset
harus.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
harus.file <- basename(harus.url)

#download dataset if it cannot be find at local drive
if(!(file.exists(harus.file))){
    download.file(harus.url,destfile=harus.file,method="auto")        
}

#obtain a list of all files containing in unziped folder
harus.file.list <- unzip(harus.file)

#read the required dataset accordingly
features <- read.table(file=harus.file.list[2])
feature.names <- features[,2]
x.test <- read.table(file=harus.file.list[15])
y.test <- read.table(file=harus.file.list[16])
subject.test <- read.table(file=harus.file.list[14])
x.train <- read.table(file=harus.file.list[27])
y.train <- read.table(file=harus.file.list[28])
subject.train <- read.table(file=harus.file.list[26])

#merge obtained dataset into one major dataset    
test.merged <- cbind(x.test,y.test,subject.test)
train.merged <- cbind(x.train,y.train,subject.train)
harus.data <- rbind(train.merged,test.merged)
colnames(harus.data)<-c(as.vector(feature.names),"y","subject")

#print some basic information re produced dataset
colna <- head(names(harus.data),n=3)
rownum <- sum(table(harus.data[,1]))
```
There are 10299 observations in the dataset created in step 1, and the first variables are tBodyAcc-mean()-X, tBodyAcc-mean()-Y, tBodyAcc-mean()-Z. 

#####2. Extracts only the measurements on the mean and standard deviation for each measurement. 

```r
#obtain the entire list of variables of dataset
harus.col <- names(harus.data)

#claim the key measurements (mean, std) we want to keep in new dataset
measure.mean <- "mean()"
measure.std <- "std()"

#identify the columns we want to keep and the columns we want to eliminate
#and implement these modifications to dataset
a <- grepl(measure.mean,harus.col,fixed=T)
b <- grepl(measure.std,harus.col,fixed=T)
c <- a | b
harus.subset <- harus.data[,c]

#print the remaining measurements in subsetted dataset
names(harus.subset)
```

```
##  [1] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
##  [3] "tBodyAcc-mean()-Z"           "tBodyAcc-std()-X"           
##  [5] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
##  [7] "tGravityAcc-mean()-X"        "tGravityAcc-mean()-Y"       
##  [9] "tGravityAcc-mean()-Z"        "tGravityAcc-std()-X"        
## [11] "tGravityAcc-std()-Y"         "tGravityAcc-std()-Z"        
## [13] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
## [15] "tBodyAccJerk-mean()-Z"       "tBodyAccJerk-std()-X"       
## [17] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
## [19] "tBodyGyro-mean()-X"          "tBodyGyro-mean()-Y"         
## [21] "tBodyGyro-mean()-Z"          "tBodyGyro-std()-X"          
## [23] "tBodyGyro-std()-Y"           "tBodyGyro-std()-Z"          
## [25] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
## [27] "tBodyGyroJerk-mean()-Z"      "tBodyGyroJerk-std()-X"      
## [29] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
## [31] "tBodyAccMag-mean()"          "tBodyAccMag-std()"          
## [33] "tGravityAccMag-mean()"       "tGravityAccMag-std()"       
## [35] "tBodyAccJerkMag-mean()"      "tBodyAccJerkMag-std()"      
## [37] "tBodyGyroMag-mean()"         "tBodyGyroMag-std()"         
## [39] "tBodyGyroJerkMag-mean()"     "tBodyGyroJerkMag-std()"     
## [41] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
## [43] "fBodyAcc-mean()-Z"           "fBodyAcc-std()-X"           
## [45] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
## [47] "fBodyAccJerk-mean()-X"       "fBodyAccJerk-mean()-Y"      
## [49] "fBodyAccJerk-mean()-Z"       "fBodyAccJerk-std()-X"       
## [51] "fBodyAccJerk-std()-Y"        "fBodyAccJerk-std()-Z"       
## [53] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
## [55] "fBodyGyro-mean()-Z"          "fBodyGyro-std()-X"          
## [57] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
## [59] "fBodyAccMag-mean()"          "fBodyAccMag-std()"          
## [61] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyAccJerkMag-std()"  
## [63] "fBodyBodyGyroMag-mean()"     "fBodyBodyGyroMag-std()"     
## [65] "fBodyBodyGyroJerkMag-mean()" "fBodyBodyGyroJerkMag-std()"
```

#####3. Uses descriptive activity names to name the activities in the data set

```r
#add two new columns representing activity and subject
harus.subset$y <- harus.data[,562]
harus.subset$Subject <- harus.data[,563]

#remove special characters like '()' in column names to make them more readable
names(harus.subset) <- gsub("\\()","", names(harus.subset))

#load the activity_labels.txt file and make its column names be identical to the names in main dataset
act.name <- read.table(harus.file.list[1])
colnames(act.name)<-c("y","Activity")

#load plyr package to do the merging process that adds activity labels to each observation in dataset 
library(plyr)
harus.all <- arrange(join(harus.subset, act.name),y)
```

```
## Joining by: y
```

```r
#print the activity distribution by subject
table(harus.all$y, harus.all$Activity)
```

```
##    
##     LAYING SITTING STANDING WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS
##   1      0       0        0    1722                  0                0
##   2      0       0        0       0                  0             1544
##   3      0       0        0       0               1406                0
##   4      0    1777        0       0                  0                0
##   5      0       0     1906       0                  0                0
##   6   1944       0        0       0                  0                0
```

#####4. Appropriately labels the data set with descriptive variable names. 

```r
#remove duplicated variable
harus.tidy <- harus.all[,-67]

#remove special characters ("-") in column names and transfer the names to lowercases
names(harus.tidy) <- gsub("-","",tolower(names(harus.tidy)))

#transfer subject and activity to factors for the convinience of further analysis
harus.tidy$subject <- as.factor(harus.tidy$subject)
harus.tidy$activity <- as.factor(harus.tidy$activity)
```

#####5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

```r
#remove the unnecessary std related variables in dataset
d <- grepl("std",names(harus.tidy),fixed=T)    
harus.tidy <- harus.tidy[,!d]

#use reshape package to melt the dataset with activity and subject as ids and the rest are variables
library(reshape)
harus.molten <- melt(harus.tidy, id.vars=c("activity","subject"))

#restructure the dataset into the one with the average of each variable by each activity and subject
harus.cast <- cast(subject + variable ~ activity, data = harus.molten, fun = mean)

#print the first 5 rows of constructed dataset
head(harus.cast,n=5)
```

```
##   subject         variable   LAYING   SITTING STANDING  WALKING
## 1       1    tbodyaccmeanx  0.22160  0.261238  0.27892  0.27733
## 2       1    tbodyaccmeany -0.04051 -0.001308 -0.01614 -0.01738
## 3       1    tbodyaccmeanz -0.11320 -0.104544 -0.11060 -0.11115
## 4       1 tgravityaccmeanx -0.24888  0.831510  0.94295  0.93522
## 5       1 tgravityaccmeany  0.70555  0.204412 -0.27298 -0.28217
##   WALKING_DOWNSTAIRS WALKING_UPSTAIRS
## 1           0.289188          0.25546
## 2          -0.009919         -0.02395
## 3          -0.107566         -0.09730
## 4           0.931874          0.89335
## 5          -0.266610         -0.36215
```

```r
#output the tidy main dataset and tidy average dataset
write.table(harus.tidy, "./tidy_dataset.csv", sep=",")
write.table(harus.cast, "./tidy_dataset_avg.csv", sep=",")
```
Thank you for reading!
Ivan Liu
