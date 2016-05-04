##Lets try and do some stuff##
setwd(dir = "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016/" )

drugs <- read.table(file = "antimicrobial.csv" , header = TRUE,sep = ",")
drugs <- subset(drugs, delivered_by!="Inpatient Team")
drugs <- subset(drugs1, delivered_by!="")

####Generate days per prescription###
drugs$start <- as.Date(drugs$start_date)
drugs$end <- as.Date(drugs$end_date)
drugs$duration <- drugs$end - drugss$start

###Lets Make some cleaned up PID Data###
PID <- read.table(file = "opat_outcome.csv" , header = TRUE,sep = ",")
PID$time1 <- substring(PID$created,1,10)
PID$time2 <- substring(PID$updated,1,10)
PID$time1 <- ifelse (PID$time1=="None" & PID$time2!="None", PID$time2,PID$time1)
PID$year <- substr(PID$time1,1,4)
PID$month <- substr(PID$time1,6,2)
PID$time1 <- PID$time2 <- NULL
