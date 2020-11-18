### NY Phil Program Analysis ###
### Get Data from XML into a dataframe/CSV File ###


install.packages('XML')
install.packages('xml2')
install.packages('methods')
install.packages('plyr')
install.packages('tidyverse')
install.packages('data.table')

library(XML)
library(xml2)
library(methods)
library(plyr)
library(tidyverse)
library(data.table)


xmlfile <- xmlParse(file="/Users/mkshah605/Desktop/Music Project/PerformanceHistory-master/Programs/xml/complete.xml", useInternalNodes = TRUE)

xmlread<- read_xml("/Users/mkshah605/Desktop/Music Project/PerformanceHistory-master/Programs/xml/complete.xml")
xml_name(xmlread) #programs
xml_children(xmlread)
child <- xml_child(xmlread, 1)
xml_attrs(xmlread)
childlist <- as_list(child)


fileurl <- "/Users/mkshah605/Desktop/Music Project/PerformanceHistory-master/Programs/xml/complete.xml"
doc <- xmlParse(fileurl,useInternalNodes = TRUE) ### xmlParse()- is to parse the xml content, the parsed content is stored into doc
doc

xL <- xmlToList(doc) ###is to convert xml doc into List



archive <- data.frame(id=NA, program_id=NA, orchestra=NA, season=NA, event_type=NA, location=NA, venue=NA, date=NA, time=NA, composer_name=NA, 
                      work_title=NA, movement = NA, conductor_name=NA, work_id=NA, interval=NA, soloist_name=NA,  soloist_instrument=NA, soloist_role=NA)
# create df and add top level (program info)
i <- 1
makeNA <- function(x){ifelse(is.null(x), NA, x)}

for (programnum in 1:(length(xL))){
  for (worknum in 1:(length(xL[programnum]$program$worksInfo))){
    if (is.character(xL[programnum]$program$worksInfo) ||  
        is.null(xL[programnum]$program$worksInfo[worknum]$work$soloists)){
      S <- 1
    } else {
      S <- length(xL[programnum]$program$worksInfo[worknum]$work$soloists)
    }
    for (solonum in 1:S){
      archive$id[i] <- makeNA(xL[programnum]$program$id)
      archive$program_id[i] <- makeNA(xL[programnum]$program$programID)
      archive$orchestra[i] <- makeNA(xL[programnum]$program$orchestra)
      archive$season[i] <- makeNA(xL[programnum]$program$season)
      archive$event_type[i] <- makeNA(xL[programnum]$program$concertInfo$eventType)
      archive$location[i] <- makeNA(xL[programnum]$program$concertInfo$Location)
      archive$venue[i] <- makeNA(xL[programnum]$program$concertInfo$Venue)
      archive$date[i] <- makeNA(xL[programnum]$program$concertInfo$Date)
      archive$time[i] <- makeNA(xL[programnum]$program$concertInfo$Time)
      if (!is.character(xL[programnum]$program$worksInfo)){
        archive$composer_name[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$composerName)
        archive$work_title[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$workTitle)
        archive$movement[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$movement)
        archive$conductor_name[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$conductorName)
        archive$work_id[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$.attrs)
        archive$interval[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$interval)
        if (!is.null(xL[programnum]$program$worksInfo[worknum]$work$soloists)){
          archive$soloist_name[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$soloists[solonum]$soloist$soloistName)
          archive$soloist_instrument[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$soloists[solonum]$soloist$soloistInstrument)
          archive$soloist_role[i] <- makeNA(xL[programnum]$program$worksInfo[worknum]$work$soloists[solonum]$soloist$soloistRoles)
        }
      }
      i <- i + 1
      archive[i, ] <- NA
     }
  }
  print(paste("program ", programnum))
}

archive2 <- archive

archive2$work_title<- unlist(archive2$work_title, recursive = TRUE, use.names = TRUE)
archive2$movement<- unlist(archive2$movement, recursive = TRUE, use.names = TRUE)


write.csv(archive2, file = "/Users/mkshah605/Desktop/Music Project/nyphilarchive_raw.csv")
