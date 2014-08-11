run_analysis <- function(){
    
    setwd("C:/Documents and Settings/Macro/Desktop/Ivandata/Human-Activity-Recognition-Using-Smartphones/")
    
    # 1.Merges the training and the test sets to create one data set.
    harus.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    harus.file <- basename(harus.url)
    if(!(file.exists(harus.file))){
        download.file(harus.url,destfile=harus.file,method="auto")        
    }
    harus.file.list <- unzip(harus.file)
    ##test.list <- 5:13
    ##train.list <- 17:25
    x.test <- read.table(file=harus.file.list[15])
    y.test <- read.table(file=harus.file.list[16])
    subject.test <- read.table(file=harus.file.list[14])
    x.train <- read.table(file=harus.file.list[27])
    y.train <- read.table(file=harus.file.list[28])
    subject.train <- read.table(file=harus.file.list[26])
    
    test.merged <- cbind(x.test,y.test,subject.test)
    train.merged <- cbind(x.train,y.train,subject.train)
    harus.data <- rbind(train.merged,test.merged)
    names(harus.data)
    sum(table(harus.data[,1]))
    # 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
    
    # 3.Uses descriptive activity names to name the activities in the data set.
    
    # 4.Appropriately labels the data set with descriptive variable names. 
    
    # 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
    
    
    
    
    
}