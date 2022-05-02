*************
** RESHAPE **
*************
// reshape data from wide to long,
// such that it is stored in a person-month format
// each row will represent a month of each woman's life and indicate
// her marital/cohabitation status, pregnancy status, sexual activity
// as well as time invariate variables like race, mother's education.

local wave `1'

use "$NSFGKeep/`wave'cohabfertwide.dta", clear

// keep relevant data
keep CASEID CMBIRTH CMINTVW HIEDUC HISPRACE AGE_A	   /// demographic and adninistrative data - time invariant
CMMONSX WGT* SECU SEST 							   	   ///
mc180-mc528 										   /// marriage/cohabitation status in each person month between 15-44
p180-p528 											   /// person-month 9 months prior to birth; same as "rrpm"
censorpreg180-censorpreg528							   /// person-months which will be censored due to an ongoing preg (no risk of pregnancy)
conceptionmo_ongoing180-conceptionmo_ongoing528 	   /// person-months of conception for pregnancies that haven't resulted in births by interview
sexmonth180-sexmonth528 							   /// person-month sex
cpmethodmonth180-cpmethodmonth528					   /// person-month cpmethod
nmethodsmonth180-nmethodsmonth528					   /// person-month number of methods used in a month
curage FMARIT EDUCMOM CMFSTSEX RHADSEX EVERUSED BRNOUT NONMARR cohpend4 cohp5 ongoingpregcount wave exclude weight?? ///
curmethod reasonfornocp intendparity CONSTAT1 infecundnosterdate CMJAN3YR misschk

drop cohdur*

** Reshape
// reshape long p censorpreg mc sexmonth cpmethodmonth nmethodsmonth, i(CASEID)j(agemonth)
// Using an alternative reshape command, tolong, which needs to be installed (ssc install tolong).
// The setup file for this analysis should require this package before running.
tolong p censorpreg conceptionmo_ongoing mc sexmonth cpmethodmonth nmethodsmonth, i(CASEID)j(agemonth)

// reapply labels which are lost in tolong
label values mc mcstatus
label values cpmethodmonth cpmethod
label values sexmonth sexmonth

// drop observations that are later than age of R at interview (they contain no data)
drop if curage < agemonth

// gen cm date of each observation
gen cmyear = agemonth + CMBIRTH
gen id = CASEID
gen age = agemonth

********************
** Save long data ** 
********************

save "$NSFGKeep/`wave'cohabfertlong.dta", replace
