*The purpose of this DO file is to make data for NORS Reporting*
**It should generate  consistent results to the R commands **

cd "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016"
**Meta Code**
**Identify the reporting Period**
clear 
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"

*The purpose of this DO file is to make data for NORS Reporting*

cd "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016"




**DONE*****Manipulation 1: Antibiotic Days for each Drug by Primary Diagnosis**

** Import Data and drop non-OPAT drugs **
cd "/Users/Michael/Dropbox/Work/HTD Database/elCID Data Analysis/OPAT May 2016"
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""

** Generate days per prescription **
gen start = date(start_date,"YMD")
gen end = date(end_date,"YMD")
gen duration = end-start

**Save this to a temporary file**
tempfile tempfile1
save `tempfile1'

**Merge this against PID**
clear 
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"

*this section is needed in case someone double-clicked save"
sort episode_id created
by episode_id: gen n=_n
drop if n!=1 

*Merge the PID Data to the Drugs*
merge 1:m episode_id using `tempfile1'

*drop data without an outcome or any IV therapy*
drop if _merge!=3

** Summate all the durations by drug **


bysort drug infective_diagnosis reportingperiod: egen totaldays = sum(duration)
bysort drug infective_diagnosis reportingperiod: egen no_episodes=count(episode_id)

levelsof reportingperiod, local(levels)
foreach l of local levels {
preserve
collapse (max)totaldays (max)no_episodes  if reportingperiod=="`l'", by(drug infective_diagnosis reportingperiod)
export delimited antibiotic_days_per_drug_`l'.csv,replace
restore
}
** The data table now lists each drug and the total number of days it was prescribed across the whole OPAT dataset **
** Uncollapse the dataset **

**DONE***Manipulation 2: Outcomes by Specialty and Infective Diagnosis**
clear
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"

*this section is needed in case someone double-clicked save"
sort episode_id created
by episode_id: gen n=_n
drop if n!=1 
tempfile tempfile_outcome
save `tempfile_outcome'

**Find out who referred people and what the Infective Diagnosis was**
clear
import delimited "location.csv",varname(1) bindquotes(strict)
merge 1:1 episode_id using `tempfile_outcome'
drop if _merge!=3

**Sum the number of each outcome type by Referring Team**
bysort opat_referral_team patient_outcome reportingperiod: gen rtpo=_N
label variable rtpo "Patient Outcome by Referring Team"
bysort opat_referral_team opat_outcome reportingperiod: gen rtoo=_N
label variable rtoo "OPAT Outcome by Referring Team"
bysort infective_diagnosis patient_outcome reportingperiod: gen pidpo=_N
label variable pidpo "Patient Outcome by Infective Diagnosis"
bysort infective_diagnosis opat_outcome reportingperiod: gen pidoo=_N
label variable pidoo "OPAT Outcome by Infective Diagnosis"

levelsof reportingperiod, local(levels)
foreach l of local levels {
preserve
collapse (max)rtpo if reportingperiod=="`l'", by(opat_referral_team patient_outcome reportingperiod)
export delimited patient_outcomes_by_referral`l'.csv,replace
restore
preserve
collapse (max)rtoo if reportingperiod=="`l'", by(opat_referral_team opat_outcome reportingperiod)
export delimited opat_outcomes_by_referral`l'.csv,replace
restore
preserve
collapse (max)pidpo if reportingperiod=="`l'", by(infective_diagnosis patient_outcome reportingperiod)
export delimited patient_outcomes_by_diagnosis`l'.csv,replace
restore
preserve
collapse (max)pidoo if reportingperiod=="`l'", by(infective_diagnosis opat_outcome reportingperiod)
export delimited opat_outcomes_by_diagnosis`l'.csv,replace
restore
}


**DONE****Manipulation 3: Work out what different adverse events we had for lines**

**Get some data about the quarter**
clear
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"
**Need this line in case of double-clicking**
sort episode_id created
by episode_id: gen n=_n
drop if n!=1 
tempfile tempfile1
save `tempfile1'

** Import Data **
clear
import delimited "line.csv",varname(1) bindquotes(strict)

merge m:1 episode_id using `tempfile1'

drop if _merge!=3
bysort complications reportingperiod: gen N=_n

levelsof reportingperiod, local(levels)
foreach l of local levels {
preserve
collapse (max)N if reportingperiod=="`l'", by(complications reportingperiod)
export delimited line_complications`l'.csv,replace
restore
}
**DONE****Manipulation 4: Work out what different adverse events we had for IV Drugs**

**Get Reporting Period**
clear
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"
**Need this line in case of double-clicking**
sort episode_id created
by episode_id: gen n=_n
drop if n!=1 
tempfile tempfile1
save `tempfile1'


** Import Data **
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""
drop if delivered_by=="in patient"
replace delivered_by="Self" if delivered_by=="self"
replace delivered_by ="Carer" if delivered_by=="Carer / DN"
drop if route=="Oral"
drop if route=="PO"

**Merge Data**
merge m:1 episode_id using `tempfile1'


bysort adverse_event reportingperiod: gen N=_n
levelsof reportingperiod, local(levels)
foreach l of local levels {
preserve
collapse (max)N if reportingperiod=="`l'", by(adverse_event reportingperiod)
export delimited drug_adverse_event_`l'.csv,replace
restore
}

**Manipulation 5: Work out the total number of treatment days and episodes by PID**
** Import Data and drop non-OPAT drugs **

**Merge this against PID**
clear 
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"

*this section is needed in case someone double-clicked save"
sort episode_id created
by episode_id: gen n=_n
drop if n!=1 

**Save this to a temporary file**
tempfile tempfile1
save `tempfile1'

**Get the data we need**
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""
drop if route=="Oral"
drop if route=="PO"

** Generate days per prescription **
gen start = date(start_date,"YMD")
gen end = date(end_date,"YMD")
gen duration = end-start

**Generate the total number of days per episode**
bysort episode_id: egen totalduration=sum(duration)
**Make it one entry per person**
collapse (max)totalduration, by(episode_id)

*Merge the PID Data to the Drugs*
merge 1:1 episode_id using `tempfile1'

*drop data without an outcome or any IV therapy*
drop if _merge!=3

*Count the number of episodes per Diagnosis*
bysort infective_diagnosis reportingperiod: gen N=_N
*Count the overall number of days of OPAT per diagnosis**
bysort infective_diagnosis reportingperiod: egen overall_duration=sum(totalduration)

**Generate the summary data for NORS**
levelsof reportingperiod, local(levels)
foreach l of local levels {
preserve
collapse (max)overall_duration (max)N if reportingperiod=="`l'", by(infective_diagnosis reportingperiod)
export delimited overall_opat_duration_by_diagnosis`l'.csv,replace
restore
}

**DONE*****Manipulation 7: Antibiotic Days by REFERRING TEAM**

** Import Data and drop non-OPAT drugs **
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""

** Generate days per prescription **
gen start = date(start_date,"YMD")
gen end = date(end_date,"YMD")
gen duration = end-start

**Save this to a temporary file**
tempfile tempfile1
save `tempfile1'

**Merge this to find people who finished that quarter**
clear 
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"

*this section is needed in case someone double-clicked save"
sort episode_id created
by episode_id: gen n=_n
drop if n!=1 

*Merge the to outcome data to the Drugs*
merge 1:m episode_id using `tempfile1'

*drop data without an outcome or any IV therapy*
drop if _merge!=3
drop _merge
tempfile tempfile2
save `tempfile2'

clear
import delimited "location.csv",varname(1) bindquotes(strict)
merge 1:m episode_id using `tempfile2'
drop if _merge!=3


** Summate all the durations by Referring Team **
bysort  opat_referral_team reportingperiod: egen totaldays = sum(duration)
bysort  opat_referral_team reportingperiod: gen no_episodes=_N
levelsof reportingperiod, local(levels)
foreach l of local levels {
preserve
collapse (max)totaldays (max)no_episodes  if reportingperiod=="`l'", by(opat_referral_team reportingperiod)
export delimited totalduration_by_referringteam_`l'.csv,replace
restore
}

****Manipulation 8****
** Import Data and drop non-OPAT drugs **
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""

** Generate days per prescription **
gen start = date(start_date,"YMD")
gen end = date(end_date,"YMD")
gen duration = end-start

**Save this to a temporary file**
tempfile tempfile1
save `tempfile1'

**Merge this against PID**
clear 
import delimited "opat_outcome.csv" ,varname(1) bindquotes(strict)
gen time1 = substr(created,1,10)
gen time2 = substr(updated,1,10)
replace time1 = time2 if time1=="None" & time2!="None"
gen year = substr(time1,1,4)
gen month = substr(time1,6,2)
drop time2 time1
gen quarter = month
destring quarter,replace
recode quarter (1/3=1)(4/6=2)(7/9=3)(10/12=4)
drop month
egen reportingperiod = concat(year quarter),punct("-")
drop if outcome_stage !="Completed Therapy"

*this section is needed in case someone double-clicked save"
sort episode_id created
by episode_id: gen n=_n
drop if n!=1 

*Merge the PID Data to the Drugs*
merge 1:m episode_id using `tempfile1'

*drop data without an outcome or any IV therapy*
drop if _merge!=3
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""
drop if delivered_by=="in patient"
replace delivered_by="Self" if delivered_by=="self"
replace delivered_by ="Carer" if delivered_by=="Carer / DN"
drop if route=="Oral"
drop if route=="PO"

** Summate all the durations by route of administration **


bysort delivered_by reportingperiod: egen totaldays = sum(duration)
bysort delivered_by reportingperiod: gen no_episodes=_N
levelsof reportingperiod, local(levels)
foreach l of local levels {
preserve
collapse (max)totaldays (max)no_episodes  if reportingperiod=="`l'", by(delivered_by  reportingperiod)
export delimited antibiotic_days_by_route_`l'.csv,replace
restore
}
*******

