##Lets try and do some stuff##
##Uses the Car Package for some commands##
##Uses the dplyr package##
library(car)
library(dplyr)
setwd(dir = "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016/" )

drugs <- read.table(file = "antimicrobial.csv" , header = TRUE,sep = ",")
drugs <- subset(drugs, delivered_by!="Inpatient Team")
drugs <- subset(drugs, delivered_by!="")

####Generate days per prescription###
drugs$start <- as.Date(drugs$start_date)
drugs$end <- as.Date(drugs$end_date)
drugs$duration <- drugs$end - drugss$start

###Lets Make some cleaned up PID Data###
setwd(dir = "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016/" )
PID <- read.table(file = "opat_outcome.csv" , header = TRUE,sep = ",")
PID$time1 <- substring(PID$created,1,10)
PID$time2 <- substring(PID$updated,1,10)
PID$time1 <- ifelse (PID$time1=="None" & PID$time2!="None", PID$time2,PID$time1)
PID$year <- substr(PID$time1,1,4)
PID$month <- substr(PID$time1,start = 6,7)
PID$quarter <- as.numeric(PID$month)
PID$month <- PID$time1 <- PID$time2 <- PID$created_by_id <- PID$updated_by_id <- NULL
PID$quarter <- recode(PID$quarter,"1:3=1;4:6=2;7:9=3;10:12=4")
PID$reportingperiod <- paste(PID$year, PID$quarter, sep="_")
PID <- subset(PID, outcome_stage=="Completed Therapy")
PID <- arrange(PID, episode_id, created)
PID <- group_by(PID, episode_id)
PID <- mutate(PID, n = row_number())
PID <- subset(PID, n==1)  

