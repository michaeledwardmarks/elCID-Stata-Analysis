##Lets try and do some stuff##
##Uses the Car Package for some commands##
##Uses the dplyr package##
##install dependencies##
#install.packages("car")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("doBy")
###Lets see what packages we need###
library(car)
library(dplyr)
library(plyr)
library(data.table)
library(doBy)
###Set a working Directory##
setwd(dir = "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016/" )

###Find drugs administered by OPAT###
drugs <- read.csv(file = "antimicrobial.csv" , header = TRUE,sep = ",")
drugs <- subset(drugs, delivered_by!="Inpatient Team")
drugs <- subset(drugs, delivered_by!="")

####Generate days per prescription###
drugs$start <- as.Date(drugs$start_date)
drugs$end <- as.Date(drugs$end_date)
drugs$duration <- drugs$end - drugs$start

###Lets Make some cleaned up PID Data###
setwd(dir = "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016/" )
PID <- read.csv(file = "opat_outcome.csv" , header = TRUE,sep = ",")

###Make a reporting period based on Year and Quarter###
PID$time1 <- substring(PID$created,1,10)
PID$time2 <- substring(PID$updated,1,10)
PID$time1 <- ifelse (PID$time1=="None" & PID$time2!="None", PID$time2,PID$time1)
PID$year <- substr(PID$time1,1,4)
PID$month <- substr(PID$time1,start = 6,7)
PID$quarter <- as.numeric(PID$month)
PID$month <- PID$time1 <- PID$time2 <- PID$created_by_id <- PID$updated_by_id <- NULL
PID$quarter <- recode(PID$quarter,"1:3=1;4:6=2;7:9=3;10:12=4")
PID$reportingperiod <- paste(PID$year, PID$quarter, sep="_")

###Only look at outcomes at end of IV Therapy as that is what NORS want###
PID <- subset(PID, outcome_stage=="Completed Therapy")

###Get rid of double clicked entries###
PID <- arrange(PID, episode_id, created)
PID$n <- with(PID, ave(episode_id,episode_id,FUN = seq_along))
PID <- subset(PID, PID$n==1)  


####*Merge the PID Data to the Drugs*
PID_Drugs <- merge(PID, drugs, by.x = "episode_id", by.y = "episode_id")

###Summate the number of days per drug per diagnosis per quarter###
PID_Drugs <- arrange(PID_Drugs, drug, infective_diagnosis,reportingperiod)
PID_Drugs$totaldays <- ave (PID_Drugs$duration,PID_Drugs$drug, PID_Drugs$infective_diagnosis,PID_Drugs$reportingperiod,FUN = sum)
setDT(PID_Drugs)[, count:=.N, by = .(drug, infective_diagnosis, reportingperiod)]

PID_Drugs$reportingperiod <- as.factor(PID_Drugs$reportingperiod)

period <- levels(PID_Drugs$reportingperiod)
numberperiods <- length(period)

summarydata <- summaryBy(duration + count ~ drug + infective_diagnosis + reportingperiod, FUN=c(max), data=PID_Drugs)

for (i in 1:numberperiods){
  
u <- data.frame(subset(summarydata,reportingperiod=period[i]))
j <- paste("mydata",period[i],".csv")
write.table (u,j, sep=",")
}
