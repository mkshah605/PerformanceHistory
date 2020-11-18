### NY Phil Archive ###
### Data Cleaning ###
install.packages('tidyr')
install.packages('splitstackshape')
install.packages('dplyr')
install.packages('data.table')
install.packages('utils')
install.packages('widyr')



library(tidyr)
library(splitstackshape)
library(dplyr)
library(data.table)
library(utils)
library(widyr)

# load raw data
rawdata <- read.csv("/Users/mkshah605/Documents/GitHub/PerformanceHistory/Analysis/nyphilarchive_raw.csv")

### Edit Season Value ###
# We want just the first year to represent the season
rawdata$season <- substr(rawdata$season, 1, 4)

### Edit Date Value ###
# We want to convert the date into a proper date format
rawdata$date<- as.Date(rawdata$date)

### Split Work_ID Column & Add Movement ID Column ###
rawdata <- cSplit(rawdata, "work_id", sep = "*", drop = TRUE)

# rename columns
colnames(rawdata)[colnames(rawdata)=="work_id_1"] <- "Work_ID" 
colnames(rawdata)[colnames(rawdata)=="work_id_2"] <- "Mvmt_ID" 

### Reorder Columns ###
rawdata <- rawdata[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 19, 12, 20, 13, 15, 14, 16, 17, 18)]

### Change Intermission Column to Binary ###
rawdata$interval <- ifelse(rawdata$interval=="Intermission", 1, 0)

### Take out rows with NA for composer name ###
rawdata <- rawdata[is.na(rawdata$composer_name)==FALSE]

### Create a unique identifier for every composer ###
rawdata$composer_id <- rawdata %>% group_indices(composer_name)

### Create a unique identifier for each work-composer combination ###
rawdata$work_composer_id <- rawdata %>% group_indices(composer_id,Work_ID)

### Create a unique identifier for each program-date combination ###
rawdata$program_date_id <- rawdata %>% group_indices(program_id,date)

### Create a unique identifier for each work-composer-date combination ###
rawdata$work_composer_date_id <- rawdata %>% group_indices(work_composer_id,date)

### Create a unique identifier for each program-work-date combination ###
rawdata$program_work_date_id <- rawdata %>% group_indices(program_id,date, Work_ID)

### Reorder Columns ###
rawdata <- rawdata[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 21, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24, 25)]

write.csv(rawdata, file = "/Users/mkshah605/Documents/GitHub/PerformanceHistory/Analysis/nyphilarchive_cleaned.csv")

# isolate just data from 2000 onwards

raw2000 <- rawdata[rawdata$date >= '2000-01-01']
write.csv(raw2000, file = "/Users/mkshah605/Documents/GitHub/PerformanceHistory/Analysis/nyphilarchive2000_cleaned.csv")


#####################################
#Out of 30375 total combos

# Appearing together 32 times
# correlation: 0.969
gephi_data$label[gephi_data$Work_ID==53780] # romeo and juliet suite no.2
gephi_data$label[gephi_data$Work_ID==53778] # romeo and juliet suite no. 1

# Appearing together 29 times
# correlation: 0.3801
gephi_data$label[gephi_data$Work_ID==8902] # hungarian dances (Brahms)
gephi_data$label[gephi_data$Work_ID==51467] # larlsienne suite no. 2 (Bizet)

# Appearing together 20 times
gephi_data$label[gephi_data$Work_ID==51467] # larlsienne suite no. 2 (Bizet)
gephi_data$label[gephi_data$Work_ID==5646] # slavonic dances (Dvorak)

# Appearing together 15 times
gephi_data$label[gephi_data$Work_ID==8999] # Lohengrin (Wagner)
gephi_data$label[gephi_data$Work_ID==52863] # symphony #5 in C# minor (Mahler)

# Appearing together 15 times
gephi_data$label[gephi_data$Work_ID==5646] # slavonic dances (Dvorak)
gephi_data$label[gephi_data$Work_ID==2877] # symphony #9 new world (Dvorak)


