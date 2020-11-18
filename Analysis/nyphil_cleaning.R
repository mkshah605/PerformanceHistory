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

###################################################################################################

### Prepare for Gephi ###
gephi_data <- raw2000

### Get rid of ID and X columns ###
gephi_data <- gephi_data[,3:length(gephi_data)]

### Rename Columns (id and label) ###
# The id column is for each program-work-date combination
# repeats beyond this are for movements or soloists- information we don't need right now
colnames(gephi_data)[colnames(gephi_data)=="program_work_date_id"] <- "id" 

# The label column will be work titles
colnames(gephi_data)[colnames(gephi_data)=="work_title"] <- "label" 

### Get rid of duplicate rows for movements. We are only interested in works for now ###
gephi_data <- gephi_data %>% distinct(id, .keep_all = TRUE)

### Get Rid of NA and make them blank instead ###
gephi_data <- sapply(gephi_data, as.character)
gephi_data[is.na(gephi_data)] <- " "
gephi_data <- as.data.frame(gephi_data)


write.csv(gephi_data, file = "/Users/mkshah605/Desktop/Music Project/gephidata.csv")

### Create CSV of Node Table in Format for Gephi ###
node_table <- gephi_data[,c(10, 11, 12)]
node_table <- unique(node_table)
colnames(node_table)[colnames(node_table)=="Work_ID"] <- "ID" # Work_ID

write.csv(node_table, file = "/Users/mkshah605/Desktop/Music Project/node_table.csv", row.names = FALSE)

### Create CSV of Edge Table in Format for Gephi ###

# Isolate just IDs of programs and works
program_and_works <- gephi_data[,c(11, 21)]

# Find how often works appear together
# upper = FALSE deletes duplicates (doesn't include upper triangle)
program_and_works <- gephi_data[,c(11, 21)]

pairwisecount <- pairwise_count(program_and_works, Work_ID, program_date_id, upper = FALSE)
colnames(pairwisecount)[colnames(pairwisecount)=="item1"] <- "Source" # Work_ID1
colnames(pairwisecount)[colnames(pairwisecount)=="item2"] <- "Target" # Work_ID2
colnames(pairwisecount)[colnames(pairwisecount)=="n"] <- "Weight" # number of times it appears together

write.csv(pairwisecount, file = "/Users/mkshah605/Desktop/Music Project/edge_table.csv", row.names = FALSE)

pairwisecor <- pairwise_cor(program_and_works, Work_ID, program_date_id, upper = FALSE, method = c("pearson", "kendall", "spearman"),
               use = "everything")




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


