## This analysis performs a data cleansing process and generate tidy datasets at the end. Specifically, it follows following steps:

#### 1. Merges the training and the test sets to create one data set.


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










2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
