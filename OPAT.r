###OPAT NORS: This takes a data-output from the elCID OPAT Module and generates NORS compliant reporting data####

####I have the following packages which are used code####
##install dependencies##
#install.packages("car")
#install.packages("dplyr")
#install.packages("doBy")
#install.packages("ggplot2")
#### 0.1 Turn on the packages we need ####
library(dplyr)
library(data.table)
library(doBy)
library(car)
library(ggplot2)
#### 1. The first set of manipulations in section 1 generate data of value several times over the course of the NORS reporting experience####

###Set a working Directory###
setwd(dir = "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016/" )

####Sometimes we need to fill NA gaps in duration with a 0 to make summation work####
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#####1.1 The location file provides both start of OPAT-episode data and referring team####
referral <- read.csv(file = "location.csv" , header = TRUE,sep = ",")
referral$opat_acceptance <- as.Date(referral$opat_acceptance, format = "%Y-%m-%d")
referral$opat_referral <- as.Date(referral$opat_referral, format = "%Y-%m-%d")

####1.2 We might at times want data on rejected patients####
rejected <- read.csv(file = "opat_rejection.csv" , header = TRUE,sep = ",")
rejected$rejected <- 1

#### 1.3: Making a set of drugs used in OPAT ####

###Find drugs administered by OPAT###
drugs <- read.csv(file = "antimicrobial.csv" , header = TRUE,sep = ",")
drugs <- subset(drugs, delivered_by!="Inpatient Team")
drugs <- subset(drugs, delivered_by!="")

####Generate days per prescription - we add one day because we are doing inclusive counting###
drugs$start <- as.Date(drugs$start_date, format = "%Y-%m-%d")
drugs$end <- as.Date(drugs$end_date, format = "%Y-%m-%d")
drugs$duration <- drugs$end - drugs$start +1
drugs$duration <-na.zero (drugs$duration)

###For any Episode we only want drugs prescribed in that episode###
###This is because when someone goes from Follow-Up back to IV we copy the drugs over to make it easier for the Clinical Team to see. But we only want drugs for this specific epsiode
opat_acceptance <- referral[c("episode_id","opat_acceptance","opat_referral")]
drugs <- merge(drugs,opat_acceptance, by.x = "episode_id", by.y = "episode_id")

###This line tells us the days between Acceptance and Starting a Drug###
###Minus Numbers and Zero are fine. They tell us the drug was started on/after the day that patient entered OPAT care###
drugs$previousepisodedrug <- drugs$opat_acceptance - drugs$start
drugs$beforeOPAT <- drugs$previousepisodedrug
drugs$beforeOPAT <- recode(drugs$beforeOPAT,"lo:0=0")

###We then Adjust Duration so that if the drug was started before OPAT the duration for this specific episode reflects merely the days within that episode###
drugs$duration <- drugs$duration - drugs$beforeOPAT 
drugs$duration <- recode(drugs$duration,"lo:0=0")

###Get rid of any drugs where the Duration == 0 because they were not given in this Episode)

drugs_clean <- subset(drugs, drugs$duration!=0)  

#### 1.4 Getting Data on Outcomes at end of IV Therapy ####
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

###Get rid of double clicked entries###
PID <- arrange(PID, episode_id, created)
PID$n <- with(PID, ave(episode_id,episode_id,outcome_stage,FUN = seq_along))
PID <- subset(PID, PID$n==1)  

###Remove people who were rejected by OPAT - obviously they won't have an outcome###
PID <- merge(PID, rejected, by.x = "episode_id", by.y = "episode_id", all=TRUE)
PID$rejected <-na.zero (PID$rejected)
PID <- subset(PID,PID$rejected==0)

##Where the Diagnosis is Free Text call it Other##
####LOOK AT THIS LINE####
PID$infective_diagnosis[PID$infective_diagnosis_ft!=""] <- "Other - Free Text"


###Only look at outcomes at end of IV Therapy as that is what NORS want###
PID_clean <- subset(PID, outcome_stage=="Completed Therapy")

###We want to report data by quarters###
PID_clean$reportingperiod <- as.factor(PID_clean$reportingperiod)


##We divide the period of time up we are interested in
period <- levels(PID_clean$reportingperiod)
numberperiods <- length(period)

####2.1 The Aim of this Manipulation is to give the Number of days of each drug for each indication in each quarter####

####*Merge the PID Data to the Drugs*
PID_Drugs <- merge(PID_clean, drugs_clean, by.x = "episode_id", by.y = "episode_id")

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

##PO by Ref
nors_outcomes_po_ref <-merge(PID_clean, referral, by.x = "episode_id", by.y = "episode_id")
nors_outcomes_po_ref$opat_outcome <-NULL
setDT(nors_outcomes_po_ref)[, count := .N, .(opat_referral_team, patient_outcome, reportingperiod)]
nors_outcomes_po_ref <- summaryBy(count ~ opat_referral_team + patient_outcome + reportingperiod, FUN=c(max), data=nors_outcomes_po_ref)

##OO by Ref
nors_outcomes_oo_ref <-merge(PID_clean, referral, by.x = "episode_id", by.y = "episode_id")
nors_outcomes_oo_ref$patient_outcome <-NULL
setDT(nors_outcomes_oo_ref)[, count := .N, .(opat_referral_team, opat_outcome, reportingperiod)]
nors_outcomes_oo_ref <- summaryBy(count ~ opat_referral_team + opat_outcome + reportingperiod, FUN=c(max), data=nors_outcomes_oo_ref)


##PO by PID
nors_outcomes_po_pid <-merge(PID_clean, referral, by.x = "episode_id", by.y = "episode_id")
nors_outcomes_po_pid$opat_outcome <-NULL
setDT(nors_outcomes_po_pid)[, count := .N, .(infective_diagnosis, patient_outcome, reportingperiod)]
nors_outcomes_po_pid <- summaryBy(count ~ infective_diagnosis + patient_outcome + reportingperiod, FUN=c(max), data=nors_outcomes_po_pid)

##OO by PID
nors_outcomes_oo_pid <-merge(PID_clean, referral, by.x = "episode_id", by.y = "episode_id")
nors_outcomes_oo_pid$patient_outcome <-NULL
setDT(nors_outcomes_oo_pid)[, count := .N, .(infective_diagnosis, opat_outcome, reportingperiod)]
nors_outcomes_oo_pid <- summaryBy(count ~ infective_diagnosis + opat_outcome + reportingperiod, FUN=c(max), data=nors_outcomes_oo_pid)

###Now we export the referring team data###

for (i in 1:numberperiods){
  
  u <- data.frame(subset(nors_outcomes_po_ref,reportingperiod==period[i]))
  j <- paste("patient_outcomes_by_referrer_",period[i],".csv")
  write.table (u,j, sep=",")
}

for (i in 1:numberperiods){
  
  u <- data.frame(subset(nors_outcomes_oo_ref,reportingperiod==period[i]))
  j <- paste("opat_outcomes_by_referrer_",period[i],".csv")
  write.table (u,j, sep=",")
}

###Repeat for the Primary Infective Diagnosis Data###

for (i in 1:numberperiods){
  
  u <- data.frame(subset(nors_outcomes_po_pid,reportingperiod==period[i]))
  j <- paste("patient_outcomes_by_diagnosis_",period[i],".csv")
  write.table (u,j, sep=",")
}

for (i in 1:numberperiods){
  
  u <- data.frame(subset(nors_outcomes_oo_pid,reportingperiod==period[i]))
  j <- paste("opat_outcomes_by_diagnosis_",period[i],".csv")
  write.table (u,j, sep=",")
}

####4.1 Work out the adverse line eventse####
linedata <- read.csv(file = "line.csv" , header = TRUE,sep = ",")

##Link it to a Reporting Period##
line_adverse_data <- merge(PID_clean, linedata, by.x = "episode_id", by.y = "episode_id")
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

drugs_adverse_events <- merge(PID_clean, drugs_clean, by.x = "episode_id", by.y = "episode_id")

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

iv_drugs <- drugs_clean
iv_drugs <- subset(drugs,route!="Oral")
iv_drugs <- subset(drugs,route!="PO")

##Now we summate all the IV drugs for anyone person - this is the amount of OPAT per person
iv_drugs_summary <- summaryBy(duration  ~ episode_id, FUN=c(sum), data=iv_drugs)

###We need both OPAT days by PID and by Referring Team##

###Link the summary of overall OPAT Days per person to a PID Value###
iv_drugs_summary_PID <- merge (PID_clean, iv_drugs_summary, by.x = "episode_id", by.y = "episode_id")

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
  j <- paste("opat_days_by_PID_",period[i],".csv")
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

####7.1 We generate a spreadsheet which tells us how reasonable the assumptions underlying this spreadsheet####

##WE WANT DRUGS MISSING A START DATE, END DATE, FREE-TEXT ADVERSE EVENTS##
missingdrugdata <- drugs

missingdrugdata$missingstart <- ifelse (missingdrugdata$start_date=="None",1,0)
missingdrugdata$missingend <- ifelse (missingdrugdata$end_date=="None",1,0)
missingdrugdata$freetextadverse <- ifelse (missingdrugdata$adverse_event_ft!="",1,0)

###Pull this in to a smaller data-frame, identify people with any mistakes and drop those with no mistakes##
drugqcdata <- missingdrugdata[c("episode_id","drug","missingstart","missingend","freetextadverse")]
drugqcdata$qc <- drugqcdata$missingstart+drugqcdata$missingend+drugqcdata$freetextadverse

drugqcdata <- subset(drugqcdata, qc!=0)

###We should Reshape this dataset to Wide to make it 1 line per person
drugqcdata$n <- with(drugqcdata, ave(episode_id,episode_id,FUN = seq_along))
drugqcdata <- reshape(drugqcdata,idvar="episode_id", timevar = "n" ,direction = "wide")

##WE WANT PATIENTS WITH A FREE-TEXT PID OR MISSING OPAT/PATIENT OUTCOMES DATA## 

missingoutcomedata <- PID

missingoutcomedata$missing_opat_outcome <- ifelse (missingoutcomedata$opat_outcome=="None" & missingoutcomedata$outcome_stage!="None",1,0)

missingoutcomedata$missing_patient_outcome <- ifelse (missingoutcomedata$patient_outcome=="None"& missingoutcomedata$outcome_stage!="None",1,0)

missingoutcomedata$PIDmissing_free_text <- ifelse (missingoutcomedata$infective_diagnosis =="None"& missingoutcomedata$outcome_stage!="None",1,0)

###Pull this in to a smaller data-frame, identify people with any mistakes and drop those with no mistakes##

outcomeqcdata <- missingoutcomedata[c("episode_id","outcome_stage","missing_opat_outcome","missing_patient_outcome","PIDmissing_free_text")]

outcomeqcdata$qc <- outcomeqcdata$missing_opat_outcome+outcomeqcdata$missing_patient_outcome+outcomeqcdata$PIDmissing_free_text

outcomeqcdata <- subset(outcomeqcdata, qc!=0)

###We should Reshape this dataset to Wide to make it 1 line per person
outcomeqcdata$n <- with(outcomeqcdata, ave(episode_id,episode_id,FUN = seq_along))
outcomeqcdata <- reshape(outcomeqcdata,idvar="episode_id", timevar = "n" ,direction = "wide")

##We also want people who are missing an end of IV Review Completely
##Merge Data on Outcomes, Referrals and Rejections to get a definitive list of OPAT people##

missingendoftherapy <- merge(PID, opat_acceptance,  by.x = "episode_id", by.y = "episode_id", all=TRUE)

##Give a score of 1 if you have a Completed Therapy Review##
missingendoftherapy$stage <- ifelse (missingendoftherapy$outcome_stage =="Completed Therapy",1,0)
##Sum these scores by episode ID##
missingendoftherapy$reviews <- ave (missingendoftherapy$stage,missingendoftherapy$episode_id,FUN = sum)
##Keep only those episode_id where there is no End of Therapy Record##
missingendoftherapy <- subset(missingendoftherapy,reviews==0)
missingendoftherapy$n <- with(missingendoftherapy, ave(episode_id,episode_id,FUN = seq_along))
missingendoftherapy <- subset(missingendoftherapy, missingendoftherapy$n==1)  

completedtherapyqcdata <- missingendoftherapy[c("episode_id")]
completedtherapyqcdata$missing_end_therapy <- 1

##We would like to know if the reason someone is missing an end of IV Review is because they are really recent and might still be on OPAT##

qcperiod <- opat_acceptance
qcperiod$year <- substr(qcperiod$opat_referral,1,4)
qcperiod$month <- substr(qcperiod$opat_referral,start = 6,7)
qcperiod$quarter <- as.numeric(qcperiod$month)
qcperiod$opat_referral <- qcperiod$opat_acceptance  <- NULL
qcperiod$quarter <- recode(qcperiod$quarter,"1:3=1;4:6=2;7:9=3;10:12=4")
qcperiod$reportingperiod <- paste(qcperiod$year, qcperiod$quarter, sep="_")
qcperiod$recent_quarter <- ifelse(qcperiod$reportingperiod==tail(qcperiod$reportingperiod,1),1,0)

###Bring these QC Checks together in to a single spreadsheet##

qcchecks <- merge(outcomeqcdata,completedtherapyqcdata, by.x = "episode_id", by.y = "episode_id", all=TRUE)

qcchecks <- merge(qcchecks,drugqcdata, by.x = "episode_id", by.y = "episode_id", all=TRUE)

qcchecks$missing_end_therapy <- na.zero (qcchecks$missing_end_therapy)
qcchecks$qc.1.x <- na.zero (qcchecks$qc.1.x)
qcchecks$qc.1.y <- na.zero (qcchecks$qc.1.y)
qcchecks$any_missing <- qcchecks$missing_end_therapy + qcchecks$qc.1.x + qcchecks$qc.1.y
qcchecks$any_missing <- recode(qcchecks$any_missing,"1:hi=1")

qcchecks <- merge (qcchecks,qcperiod,by = "episode_id", all = TRUE)

##We want to report these errors by quarter##
qcquarter <- as.factor(qcchecks$reportingperiod)
qcquarter <- levels(qcquarter)
numqcquarters <- length(qcquarter)

#One output CSV per quarter##

for (i in 1:numqcquarters){
  
  u <- data.frame(subset(qcchecks,reportingperiod==qcquarter[i]))
  j <- paste("QC_Checks_",qcquarter[i],".csv")
  write.table (u,j, sep=",")
}


####8 We want a Wide Summary Spreadsheet by Patient####

##Make Drugs Wide and keep Drug, Delivered by, Start and End, Adjusted Duration, Adverse Events##
drug_summary <- drugs_clean
drug_summary <- arrange(drug_summary,episode_id, start_date)
drug_summary$n <- with(drug_summary, ave(episode_id,episode_id,FUN = seq_along))

drug_summary$created <- drug_summary$updated <- drug_summary$created_by_id <- drug_summary$updated_by_id <- drug_summary$comments <- drug_summary$no_antimicrobials <- drug_summary$route_fk_id <- drug_summary$route_ft <- drug_summary$drug_ft <- drug_summary$drug_fk_id <- drug_summary$frequency_fk_id <- drug_summary$frequency_fk_id <-drug_summary$delivered_by_fk_id <- drug_summary$delivered_by_ft <- drug_summary$adverse_event_fk_id <- drug_summary$adverse_event_ft <- drug_summary$reason_for_stopping_fk_id <- drug_summary$reason_for_stopping_ft <- drug_summary$start <- drug_summary$end <- drug_summary$opat_acceptance <- drug_summary$opat_referral <- drug_summary$previousepisodedrug <- drug_summary$beforeOPAT <- NULL

drug_summary <- reshape(drug_summary,idvar="episode_id", timevar = "n" ,direction = "wide")



outcome_summary <- PID
outcome_summary <- arrange(outcome_summary,episode_id)
outcome_summary$stage <- ifelse(outcome_summary$outcome_stage=="Completed Therapy",1,ifelse(outcome_summary$outcome_stage=="Completed Therapy Post Follow Up",2,ifelse(outcome_summary$outcome_stage=="OPAT Review",3,0)))

outcome_summary <- reshape(outcome_summary,idvar="episode_id", timevar = "stage" ,direction = "wide")

line_summary <- linedata
line_summary <- arrange(line_summary,episode_id, insertion_datetime)
line_summary$n <- with(line_summary, ave(episode_id,episode_id,FUN = seq_along))
line_summary <- reshape(line_summary,idvar="episode_id", timevar = "n" ,direction = "wide")

referred_summary <- qcperiod
rejected_summary <- rejected

pt_summary <- merge(drug_summary,outcome_summary, by.x = "episode_id", by.y = "episode_id", all=TRUE)
pt_summary <- merge(pt_summary, line_summary, by.x = "episode_id", by.y = "episode_id", all=TRUE)
pt_summary <- merge(pt_summary, referred_summary, by.x = "episode_id", by.y = "episode_id", all=TRUE)
pt_summary <- merge(pt_summary, rejected_summary, by.x = "episode_id", by.y = "episode_id", all=TRUE)
