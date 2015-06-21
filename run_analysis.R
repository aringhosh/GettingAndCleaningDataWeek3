packages <- c("data.table", "reshape2", "dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
projectDataPath <- file.path(path, "project_data")
fileCount <- length(list.files(projectDataPath, recursive=TRUE))
if (fileCount != 28) {
  stop("Please use setwd() to the root of the cloned repository.")
}

# Read in the 'Subject' 'Measurements' AND 'Acitivity' data
dtTrainingSubjects <- fread(file.path(projectDataPath, "train", "subject_train.txt"))
dtTestSubjects  <- fread(file.path(projectDataPath, "test" , "subject_test.txt" ))
dtTrainingActivity <- fread(file.path(projectDataPath, "train", "Y_train.txt"))
dtTestActivity  <- fread(file.path(projectDataPath, "test" , "Y_test.txt" ))
dtTrainingMeasures <- data.table(read.table(file.path(projectDataPath, "train", "X_train.txt")))
dtTestMeasures  <- data.table(read.table(file.path(projectDataPath, "test" , "X_test.txt")))

# Row merge the Training and Test Subjects
dtSubjects <- rbind(dtTrainingSubjects, dtTestSubjects)
setnames(dtSubjects, "V1", "subject")

dtActivities <- rbind(dtTrainingActivity, dtTestActivity)
setnames(dtActivities, "V1", "activityNumber")

dtMeasures <- rbind(dtTrainingMeasures, dtTestMeasures) # Merge the Training and Test 'Measurements' data

dtSubjectActivities <- cbind(dtSubjects, dtActivities)
dtSubjectAtvitiesWithMeasures <- cbind(dtSubjectActivities, dtMeasures)

setkey(dtSubjectAtvitiesWithMeasures, subject, activityNumber)

dtAllFeatures <- fread(file.path(projectDataPath, "features.txt"))
setnames(dtAllFeatures, c("V1", "V2"), c("measureNumber", "measureName"))

# Use grepl to just get features/measures related to mean and std
dtMeanStdMeasures <- dtAllFeatures[grepl("(mean|std)\\(\\)", measureName)]

dtMeanStdMeasures$measureCode <- dtMeanStdMeasures[, paste0("V", measureNumber)]

columnsToSelect <- c(key(dtSubjectAtvitiesWithMeasures), dtMeanStdMeasures$measureCode)
dtSubjectActivitesWithMeasuresMeanStd <- subset(dtSubjectAtvitiesWithMeasures, select = columnsToSelect)

dtActivityNames <- fread(file.path(projectDataPath, "activity_labels.txt"))
setnames(dtActivityNames, c("V1", "V2"), c("activityNumber", "activityName"))


dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtActivityNames, by = "activityNumber", 
                                               all.x = TRUE)

setkey(dtSubjectActivitesWithMeasuresMeanStd, subject, activityNumber, activityName)

dtSubjectActivitesWithMeasuresMeanStd <- data.table(melt(dtSubjectActivitesWithMeasuresMeanStd, 
                                                         id=c("subject", "activityName"), 
                                                         measure.vars = c(3:68), 
                                                         variable.name = "measureCode", 
                                                         value.name="measureValue"))

dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtMeanStdMeasures[, list(measureNumber, measureCode, measureName)], 
                                               by="measureCode", all.x=TRUE)

dtSubjectActivitesWithMeasuresMeanStd$activityName <- factor(dtSubjectActivitesWithMeasuresMeanStd$activityName)
dtSubjectActivitesWithMeasuresMeanStd$measureName <- factor(dtSubjectActivitesWithMeasuresMeanStd$measureName)

measureAvgerages <- dcast(dtSubjectActivitesWithMeasuresMeanStd, 
                          subject + activityName ~ measureName, 
                          mean, 
                          value.var="measureValue")

write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t")
