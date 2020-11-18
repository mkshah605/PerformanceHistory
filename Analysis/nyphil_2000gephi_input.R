# Prepare File for Gephi Import

###################################################################################################
raw2000 <- read.csv("/Users/mkshah605/Documents/GitHub/PerformanceHistory/Analysis/nyphilarchive2000_cleaned.csv")

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



