####OPAT NORS: This takes a data-output from elCID OPAT Module and generates NORS compliant reporting data####

####I have the following packages which are used code####
##install dependencies##
#install.packages("car")
#install.packages("dplyr")
#install.packages("doBy")
#### 0.1 Turn on the packages we need ####
library(dplyr)
library(data.table)
library(doBy)
library(car)

#### 1. The first set of manipulations in section 1 generate data of value several times over the course of the NORS reporting experience####

###Set a working Directory##
setwd(dir = "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016/" )

####Sometimes we need to fill NA gaps in duration with a 0 to make summation work####
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#### 1.1: Making a set of drugs used in OPAT ####

###Find drugs administered by OPAT###
drugs <- read.csv(file = "antimicrobial.csv" , header = TRUE,sep = ",")
drugs <- subset(drugs, delivered_by!="Inpatient Team")
drugs <- subset(drugs, delivered_by!="")

####Generate days per prescription###
drugs$start <- as.Date(drugs$start_date)
drugs$end <- as.Date(drugs$end_date)
drugs$duration <- drugs$end - drugs$start
drugs$duration <-na.zero (drugs$duration)
#### 1.2 Getting Data on Outcomes at end of IV Therapy ####
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

###1.3 Sometimes we need data by referring team
referral <- read.csv(file = "location.csv" , header = TRUE,sep = ",")

####1.4 In general we want to report by Quarter####

###We want to report data by quarters###
PID$reportingperiod <- as.factor(PID$reportingperiod)

##We divide the period of time up we are interested in
period <- levels(PID$reportingperiod)
numberperiods <- length(period)

####2.1 The Aim of this Manipulation is to give the Number of days of each drug for each indication in each quarter####

####*Merge the PID Data to the Drugs*
PID_Drugs <- merge(PID, drugs, by.x = "episode_id", by.y = "episode_id")

###Summate the number of days per drug per diagnosis per quarter###
PID_Drugs <- arrange(PID_Drugs, drug, infective_diagnosis,reportingperiod)
PID_Drugs$totaldays <- ave (PID_Drugs$duration,PID_Drugs$drug, PID_Drugs$infective_diagnosis,PID_Drugs$reportingperiod,FUN = sum)
setDT(PID_Drugs)[, count := uniqueN(episode_id), .(drug, infective_diagnosis, reportingperiod)]

##Collapse this data down so we just have the summary data for NORS
summarydata_drugs_indication <- summaryBy(duration + count ~ drug + infective_diagnosis + reportingperiod, FUN=c(max), data=PID_Drugs)

###We output the data we want###
for (i in 1:numberperiods){
  
u <- data.frame(subset(summarydata_drugs_indication,reportingperiod==period[i]))
j <- paste("drugs_by_indication_in_",period[i],".csv")
write.table (u,j, sep=",")
}

####3.1 Patient Outcomes / OPAT Outcomes by Referrer and Infective Diagnosis####

##Link it to the Reporting Period##
nors_outcomes <-merge(PID, referral, by.x = "episode_id", by.y = "episode_id")

###We want to know the sum of each outcome type by Referring Team##
setDT(nors_outcomes)[, count := .N, .(opat_referral_team, patient_outcome, reportingperiod)]
nors_outcomes$rtpo <- nors_outcomes$count
setDT(nors_outcomes)[, count := .N, .(opat_referral_team, opat_outcome, reportingperiod)]
nors_outcomes$rtoo <- nors_outcomes$count

###We want to know the sum of each outcome type by Infective Diagnosism##
setDT(nors_outcomes)[, count := .N, .(infective_diagnosis, patient_outcome, reportingperiod)]
nors_outcomes$pidpo <- nors_outcomes$count
setDT(nors_outcomes)[, count := .N, .(infective_diagnosis, opat_outcome, reportingperiod)]
nors_outcomes$pidoo <- nors_outcomes$count

###Next we will want some kind of summary spreadsheet one for data by referring team and one for data by Primary Infective Diagnosis###
summarydata_nors_outcomes_referrer <- summaryBy(rtpo + rtoo   ~ opat_referral_team + reportingperiod, FUN=c(max), data=nors_outcomes)

summarydata_nors_outcomes_PID <- summaryBy(pidpo + pidoo   ~ infective_diagnosis + reportingperiod, FUN=c(max), data=nors_outcomes)

###Now we export the referring team data###
for (i in 1:numberperiods){

u <- data.frame(subset(summarydata_nors_outcomes_referrer,reportingperiod==period[i]))
j <- paste("outcomes_by_referrer_",period[i],".csv")
write.table (u,j, sep=",")
}
###Repeat for the Primary Infective Diagnosis Data###

for (i in 1:numberperiods){
  
  u <- data.frame(subset(summarydata_nors_outcomes_PID,reportingperiod==period[i]))
  j <- paste("outcomes_by_diagnosis_",period[i],".csv")
  write.table (u,j, sep=",")
}

####4.1 Work out the adverse line eventse####
linedata <- read.csv(file = "line.csv" , header = TRUE,sep = ",")

##Link it to a Reporting Period##
line_adverse_data <- merge(PID, linedata, by.x = "episode_id", by.y = "episode_id")
##Count the number of each type of complication by period##
setDT(line_adverse_data)[, count := .N, .(complications, reportingperiod)]

###Next we will want some kind of summary spreadsheet showing complications per period###
summarydata_line_adverse_data <- summaryBy(count  ~ complications + reportingperiod, FUN=c(max), data=line_adverse_data)

##One output CSV per quarter##


for (i in 1:numberperiods){
  
  u <- data.frame(subset(summarydata_line_adverse_data,reportingperiod==period[i]))
  j <- paste("line_adverse_events_",period[i],".csv")
  write.table (u,j, sep=",")
}

#####5.1 What adverse events did we have for drugs#####

drugs_adverse_events <- merge(PID, drugs, by.x = "episode_id", by.y = "episode_id")

##Count the number of each type of adverse event by period##
setDT(drugs_adverse_events)[, count := .N, .(adverse_event, reportingperiod)]

###Next we will want some kind of summary spreadsheet showing complications per period###
summarydata_drug_adverse_data <- summaryBy(count  ~ adverse_event + reportingperiod, FUN=c(max), data=drugs_adverse_events)

##One output CSV per quarter##


for (i in 1:numberperiods){
  
  u <- data.frame(subset(summarydata_drug_adverse_data,reportingperiod==period[i]))
  j <- paste("drug_adverse_events_",period[i],".csv")
  write.table (u,j, sep=",")
}

####6.1 Calculate the total number of IV treatment days and episodes by PID####

iv_drugs <- drugs
iv_drugs <- subset(drugs,route!="Oral")
iv_drugs <- subset(drugs,route!="PO")

##Now we summate all the IV drugs for anyone person - this is the amount of OPAT per person
iv_drugs_summary <- summaryBy(duration  ~ episode_id, FUN=c(sum), data=iv_drugs)

###We need both OPAT days by PID and by Referring Team##

###Link the summary of overall OPAT Days per person to a PID Value###
iv_drugs_summary_PID <- merge (PID, iv_drugs_summary, by.x = "episode_id", by.y = "episode_id")


## Count Unique Occurences of Episode_ID within PID and Reporting Period##
iv_drugs_summary_PID <- arrange(iv_drugs_summary_PID, infective_diagnosis, reportingperiod)

setDT(iv_drugs_summary_PID)[, count := uniqueN(episode_id), .(infective_diagnosis, reportingperiod)]

##Work out total OPAT Days per PID##
iv_drugs_summary_PID$totalopat <- ave (iv_drugs_summary_PID$duration,iv_drugs_summary_PID$infective_diagnosis,iv_drugs_summary_PID$reportingperiod,FUN = sum)

##Make a summary sheet of OPAT Days by PID for NORS##

opat_days_PID <- summaryBy(count + totalopat ~ infective_diagnosis + reportingperiod, FUN=c(max), data=iv_drugs_summary_PID)

##One output CSV per quarter##

for (i in 1:numberperiods){
  
  u <- data.frame(subset(opat_days_PID,reportingperiod==period[i]))
  j <- paste("opat_days_by_referrer_",period[i],".csv")
  write.table (u,j, sep=",")
}


###Link the summary of overall OPAT Days per person to a Referrer Value###
##NB We need the PID data too because that has the Reporting Period In##
iv_drugs_summary_referrer <- merge (referral, iv_drugs_summary, by.x = "episode_id", by.y = "episode_id")
iv_drugs_summary_referrer <- merge (iv_drugs_summary_referrer, PID, by.x = "episode_id", by.y = "episode_id")

iv_drugs_summary_referrer <- arrange(iv_drugs_summary_referrer, opat_referral_team, reportingperiod)

setDT(iv_drugs_summary_referrer)[, count := uniqueN(episode_id), .(opat_referral_team, reportingperiod)]

##Work out total OPAT Days per Referrer##
iv_drugs_summary_referrer$totalopat <- ave (iv_drugs_summary_referrer$duration,iv_drugs_summary_referrer$opat_referral_team,iv_drugs_summary_referrer$reportingperiod,FUN = sum)


##Make a summary sheet of OPAT Days by Referral Team for NORS##

opat_days_referrer <- summaryBy(count + totalopat ~ opat_referral_team + reportingperiod, FUN=c(max), data=iv_drugs_summary_referrer)

##One output CSV per quarter##

for (i in 1:numberperiods){
  
  u <- data.frame(subset(opat_days_PID,reportingperiod==period[i]))
  j <- paste("opat_days_by_referrer_",period[i],".csv")
  write.table (u,j, sep=",")
}


####7.1 We are finished ! ! !####
