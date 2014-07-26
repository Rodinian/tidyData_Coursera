fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "getdata-projectfiles-UCI HAR Dataset.zip", method = "auto")
unzip("getdata-projectfiles-UCI HAR Dataset.zip")
setwd("./UCI HAR Dataset")


### reading the file to R
    label<-read.table("./train/subject_train.txt")
    x_train<-read.table("./train/X_train.txt")
    y_train<-read.table("./train/y_train.txt")
    label_test<-read.table("./test/subject_test.txt")
    x_test<-read.table("./test/X_test.txt")
    y_test<-read.table("./test/y_test.txt")
    activity_labels<-read.table("./activity_labels.txt",stringsAsFactors= FALSE)


## figuring out how to put the data together
    dim(label)
    dim(x_train)
    dim(y_train)
    dim(label_test)
    dim(x_test)
    dim(y_test)

## Merges the training and the test sets to create one data set.
    dataX<-  rbind(x_train,x_test)
    dataY<- rbind(y_train,y_test)## this is the activity_labels of train and test
    dataSubject<-rbind(label,label_test)


## pick the  measurements on the mean and standard deviation for each measurement
    name<-read.table("./features.txt",stringsAsFactors= FALSE)
    k<-name[2]
    names1<-name[sapply(k,grepl,pattern = "mean"),1]
    names2<-name[sapply(k,grepl,pattern = "std"),1]
    n<-sort(c(names1,names2))

##Extracts only the measurements on the mean and standard deviation for each measurement.
    dataX<-dataX[,n]
    data<- cbind(dataY,dataSubject,dataX) ## one data set
    data2<- data

###Step3  Uses descriptive activity names to name the activities in the data set
    data[1]<-gsub(1,activity_labels[1,2],data[,1])
    data[1]<-gsub(2,activity_labels[2,2],data[,1])
    data[1]<-gsub(3,activity_labels[3,2],data[,1])
    data[1]<-gsub(4,activity_labels[4,2],data[,1])
    data[1]<-gsub(5,activity_labels[5,2],data[,1])
    data[1]<-gsub(6,activity_labels[6,2],data[,1])




##Step4  Appropriately labels the data set with descriptive variable names. 
    names_1<- name[n,2]
    names_1<-c("activity_labels","subject",names_1)
    colnames(data)<-names_1## the one data set 
    colnames(data2)<-names_1


#### step5 Creates a tidy data set with the average of each variable for each activity and each subject

    aggdata <-aggregate(data2, by=list(data2$activity_labels,data2$subject), FUN=mean)
    tidydata<- aggdata[,3:83]
    tidydata[1]<-gsub(1,activity_labels[1,2],tidydata[,1])
    tidydata[1]<-gsub(2,activity_labels[2,2],tidydata[,1])
    tidydata[1]<-gsub(3,activity_labels[3,2],tidydata[,1])
    tidydata[1]<-gsub(4,activity_labels[4,2],tidydata[,1])
    tidydata[1]<-gsub(5,activity_labels[5,2],tidydata[,1])
    tidydata[1]<-gsub(6,activity_labels[6,2],tidydata[,1])

    write.table(tidydata, file = "tidydata.csv", sep = ", ")

