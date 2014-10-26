#--------QUESTION 1:  Merges the training and the test sets to create one data set.--------

data_train<-read.table("X_train.txt", sep="", header=FALSE, na.strings="N/A")  #Import training data
data_test<-read.table("X_test.txt", sep="", header=FALSE, na.strings="N/A") #Import test data 
label_train<-read.table("y_train.txt", sep="", header=FALSE, na.strings="N/A")  #Import training labels
label_test<-read.table("y_test.txt", sep="", header=FALSE, na.strings="N/A")  #Import test labels
subject_train<-read.table("subject_train.txt", sep="", header=FALSE, na.strings="N/A")  #Import training subjects
subject_test<-read.table("subject_test.txt", sep="", header=FALSE, na.strings="N/A")  #Import test subjects
activity_labels<-read.table("activity_labels.txt", sep="", header=FALSE, na.strings="N/A")  #Import activity labels
features<-read.table("features.txt", sep="", header=FALSE, na.strings="N/A")  #Import feature list


data_all<-rbind(data_train,data_test)  #Merge training and test data (appending test to train dataset)
label_all<-rbind(label_train,label_test)  #Merge training and test labels (appending test to train dataset)
subject_all<-rbind(subject_train,subject_test)  #Merge training and test subjects (appending test to train dataset)
rm(data_train) #Remove the "data_train" object
rm(data_test) #Remove the "data_test" object
rm(label_train)  #Remove the "label_train" object
rm(label_test)  #Remove the "label_test" object
rm(subject_train)  #Remove the "subject_train" object
rm(subject_test)  #Remove the "subject_test" object


#--------QUESTION 2:  Extracts only the measurements on the mean and standard deviation for each measurement.--------

library(sqldf) #Load the sqldf library

#Find the measures to extract from the features list
measures_to_extract<-sqldf("select * from features where lower(V2) like '%-mean%' OR lower(V2) like '%-std%'") 
        #'%-mean%' and '%-std%' seemed appropriate filters based on my evaluation.  
        # I deliberately chose to ignore the angle measurements since those are calculated from the other mean values (i.e. not means of angle measurements).

#Build SQL query for data extraction
measures_to_extract$filter<-paste("V",measures_to_extract$V1, sep="")  #Add additional column with full variable names
aa<-paste(measures_to_extract$filter,',', sep="")  #create vector of colum names to include in SQL select statement (with commas)
bb<-paste(measures_to_extract$filter, sep="")  #create an additional vector similar to previous, but without commas
aa[length(aa)]<-bb[length(bb)]  #replace the last vector element in aa with one from bb, since the last SQL select field doesn't need a comma
rm(bb)  #Remove bb
aa<-paste(aa, collapse=" ")  #collapse aa into a string
aa<-paste(c("select ",aa," from data_all"), collapse=" ")  #add remaining elements to aa so that it's a proper SQL query

extract_mean_std_data_all<-sqldf(aa)  #Create extract of only the mean() and the std() measurements
rm(aa)  #Remove the "aa" object


#--------QUESTION 3:  Uses descriptive activity names to name the activities in the data set.--------

extract_mean_std_data_all<-cbind(extract_mean_std_data_all,subject_all)#Add column at end of extracted data set with subject ID's
colnames(extract_mean_std_data_all)[80]<-"subject_id"  #Rename column to "subject_id"
extract_mean_std_data_all<-cbind(extract_mean_std_data_all,label_all) #Add column at end of extracted data set with activity ID's
colnames(extract_mean_std_data_all)[81]<-"activity_id"  #Rename column to "activity_id"

activity_data_names<-sqldf("select case
                                when activity_id=1 then 'WALKING'
                                when activity_id=2 then 'WALKING_UPSTAIRS'
                                when activity_id=3 then 'WALKING_DOWNSTAIRS'
                                when activity_id=4 then 'SITTING'
                                when activity_id=5 then 'STANDING'
                                when activity_id=6 then 'LAYING'
                                else NULL end as activity_name
                           from extract_mean_std_data_all")  # Query to create row by row activity_data_names

extract_mean_std_data_all<-cbind(extract_mean_std_data_all,activity_data_names) #Add column at end of extracted data set with activity ID's
colnames(extract_mean_std_data_all)[82]<-"activity_name"  #Rename column to "activity_name"
extract_mean_std_data_all$activity_id<-NULL  #Remove "activity_id" column from "extract_mean_std_data_all", since the dataset now has the activity names.
rm(activity_data_names)  #Remove the "activity_data_names" object


#--------QUESTION 4:  Appropriately labels the data set with descriptive variable names.--------

temp_column_names<-sqldf("select V2 from measures_to_extract")  #Retrieve variable names from "measures_to_extract" (in correct order)
temp_column_names<-paste(temp_column_names$V2, sep=" ")  #Transform "temp_column_names" from data frame to character class
temp_column_names[length(temp_column_names)+1]<-"subject_id"  #Append "subject_id" to "temp_column_names"
temp_column_names[length(temp_column_names)+1]<-"activity_name"  #Append "activity_name" to "temp_column_names"
temp_column_names<-gsub("\\()","",temp_column_names)  #Removes "()" from variable names
temp_column_names<-gsub("\\-","_",temp_column_names)  #Replaces "-" in variable names with "_"

colnames(extract_mean_std_data_all)<-temp_column_names  #Assign correct variable names to "extract_mean_std_data_all" from "temp_column_names"
rm(measures_to_extract)  #Remove the "measures_to_extract" object
rm(temp_column_names)  #Remove the "temp_column_names" object


#--------QUESTION 5:  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.--------

#Remove all data objects except "extract_mean_std_data_all" created in previous steps
rm(activity_labels)  #Remove the "activity_labels" object
rm(data_all)  #Remove the "data_all" object
rm(features)  #Remove the "features" object
rm(label_all)  #Remove the "label_all" object
rm(subject_all)  #Remove the "subject_all" object


#Build SQL query for data aggregation
temp_data<-extract_mean_std_data_all  #Create temporary dataset.  Will be used to remove last columns and store column names.
col_names<-colnames(temp_data)  #Store variable names in "col_names" for use in building SQL
rm(temp_data)  #Remove the "temp_data" object
aa<-paste("avg(",col_names,")"," avg_",col_names,",")  #create vector of colum names to include in SQL select statement (with commas)
rm(col_names)  #Remove the "col_names" object
aa<-gsub("\\( ","\\(",aa) #Delete any spaces in text matching pattern
aa<-gsub(" \\) ","\\)",aa) #Delete any spaces in text matching pattern
aa<-gsub("_ ","_",aa) #Delete any spaces in text matching pattern
aa[80]<-"subject_id,"
aa[81]<-"activity_name"
aa<-paste(aa, collapse="")  #Collapse aa into a string
aa<-paste(c("select ",aa," from extract_mean_std_data_all group by subject_id, activity_name"), collapse=" ")  #add remaining elements to aa so that it's a proper SQL query


final_data<-sqldf(aa)  #Create the final aggregated dataset using SQL created above
rm(aa)  #Remove the "aa" object
write.table(final_data, file="final_data.txt")  #Write out the final dataset