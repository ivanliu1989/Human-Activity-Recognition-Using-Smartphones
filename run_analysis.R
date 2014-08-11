run_analysis <- function(){
    
    setwd("C:/Documents and Settings/Macro/Desktop/Ivandata/Human-Activity-Recognition-Using-Smartphones/")
    
# 1.Merges the training and the test sets to create one data set.
    harus.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    harus.file <- basename(harus.url)
    if(!(file.exists(harus.file))){
        download.file(harus.url,destfile=harus.file,method="auto")        
    }
    ##harus.file.list <- basename(unzip(harus.file))
    harus.file.list <- unzip(harus.file)
    ##test.list <- 5:13
    ##train.list <- 17:25
    features <- read.table(file=harus.file.list[2])
    feature.names <- features[,2]
    x.test <- read.table(file=harus.file.list[15])
    y.test <- read.table(file=harus.file.list[16])
    subject.test <- read.table(file=harus.file.list[14])
    x.train <- read.table(file=harus.file.list[27])
    y.train <- read.table(file=harus.file.list[28])
    subject.train <- read.table(file=harus.file.list[26])
    
    test.merged <- cbind(x.test,y.test,subject.test)
    train.merged <- cbind(x.train,y.train,subject.train)
    harus.data <- rbind(train.merged,test.merged)
    colnames(harus.data)<-c(as.vector(feature.names),"y","subject")
    names(harus.data)
    sum(table(harus.data[,1]))
    
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
    harus.col <- names(harus.data)
    measure.mean <- "mean()"
    measure.std <- "std()"
    a <- grepl(measure.mean,harus.col,fixed=T)
    b <- grepl(measure.std,harus.col,fixed=T)
    
    c <- a | b
    ##harus.data.mean <- harus.data[,grep(measure.mean,harus.col)]
    ##harus.data.std <- harus.data[,grep(measure.std,harus.col)]
    ##harus.data.sub <- cbind(harus.data.mean,harus.data.std)
    harus.subset <- harus.data[,c]
        
# 3.Uses descriptive activity names to name the activities in the data set.
    harus.subset$y <- harus.data[,562]
    harus.subset$Subject <- harus.data[,563]
    names(harus.subset) <- gsub("\\()","", names(harus.subset))
    act.name <- read.table(harus.file.list[1])
    colnames(act.name)<-c("y","Activity")
    library(plyr)
    harus.all <- arrange(join(harus.subset, act.name),y)
    table(harus.all$y, harus.all$Activity)
    
# 4.Appropriately labels the data set with descriptive variable names. 
    harus.tidy <- harus.all[,-67]    
    names(harus.tidy) <- gsub("-","",tolower(names(harus.tidy)))
    harus.tidy$subject <- as.factor(harus.tidy$subject)
    harus.tidy$activity <- as.factor(harus.tidy$activity)
    ##apply(harus.tidy,2,class)
    
# 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
    #lapply(harus.tidy[,1:66],as.numeric)
    d <- grepl("std",names(harus.tidy),fixed=T)    
    harus.tidy <- harus.tidy[,!d]
    ##harus.tidy.2 <- ddply(harus.tidy[,1:68], .(activity, subject), mean)
    library(reshape)
    harus.molten <- melt(harus.tidy, id.vars=c("activity","subject"))
    harus.cast <- cast(subject + variable ~ activity, data = harus.molten, fun = mean)
    ##head(harus.cast)
    write.table(harus.tidy, "./tidy_dataset.csv", sep=",")
    write.table(harus.cast, "./tidy_dataset_avg.csv", sep=",")
}