### NY Phil Archive ###

### Questions to ask:

# isolate since 2000

# Most performed composers?
# Do certain works appear on programs together than others?

install.packages("RColorBrewer")
install.packages("igraph")
install.packages("networkD3")
install.packages("magrittr")
install.packages("dplyr")
library(magrittr)
library(networkD3)
library(RColorBrewer) 
library(igraph)
library(dplyr)
library(forcats)

# load raw data
cleaned_data <- read.csv("/Users/mkshah605/Documents/GitHub/PerformanceHistory/Analysis/nyphilarchive_cleaned.csv")


# Isolate rows 2000s data
recent_data <- cleaned_data[cleaned_data[, "season"] >= 2000, ]

### Top Composers based on Work_Composer_ID ###

# Isolate unique works regardless of performance date or # of times performed
recent_workcomposer <- recent_data[!duplicated(recent_data$work_composer_id), ]
# 4081 unique works

# Count how many times each composer's works were played
composer_count_works <- recent_workcomposer %>% count(composer_name)
# Mozart - 126 unique works played (3.09%) (makes sense bc very prolific)
# Traditional - 125 unique works played (3.06%)

################################################################################################

### Top Composers based on Work_Composer_Date_ID ###

# Isolate unique performances of works
recent_workcomposerdate <- recent_data[!duplicated(recent_data$work_composer_date_id), ]
# 10591 unique performances

# Count how many times each composer's works were played
composer_count_performance <- recent_workcomposerdate %>% count(composer_name)
# Beethoven works performed 495 times (4.67%) popularity?
# Mozart works performed 435 times (4.11%)
# Tchaikovsky works performed 405 times (3.82%)


### Top Works based on Work_Composer_Date_ID ###

# Count how many times each work was played
work_count_performance <- recent_workcomposerdate %>% count(work_title)
# Hungarian Dances (Brahms) performed 86 times (0.81%)
# National Anthem performed 77 times (0.73%)
# L/Arlessienne Suite #2 (Bizet) works performed 62 times (0.59%)


### Most popular soloists
soloinstrument_count_performance <- recent_workcomposerdate[recent_workcomposerdate$soloist_role==('S'),]
soloinstrument_count_performance$soloist_instrument <- fct_explicit_na(soloinstrument_count_performance$soloist_instrument)
soloinstrument_count_performance <- soloinstrument_count_performance %>% count(soloist_instrument)


################################################################################################
# Isolate unique performances of programs on dates
recent_programdate <- recent_data[!duplicated(recent_data$program_date_id), ]
# 10591 unique performances

### Most popular performance time
perftime_count_performancetime <- recent_programdate %>% count(time)
# 7:30PM


################################################################################################

# Calculate pairwise ttest
pairwise.t.test()

