// 4.sample_counts.do
// This script reads in the final data and generates some counts for sample size
// that are reported in the text of the manuscript.

use "$NSFGKeep/cohabfertlong_appended.dta", clear

// svyset data
svyset SECU [pweight = xrndweight], strata(SEST)

// Recode race/ethnicity so that black and hispanic are in one group, 
// white women are in another group
local racethn "Black/Hispanic White Total"
recode HISPRACE (1=1)(3=1)(2=2)(4=3), gen(racethn)
label define racethn 1 "Black or Hispanic" 2 "NH White" 3 "NH other"
label values racethn racethn

// indicator of twenties
gen twenties = 0
replace twenties = 1 if age1 == 2 | age1 == 3

************************************
** Sample size - reported in text **
************************************
preserve 

// Remove if doesn't meet previously set inclusion criteria
keep if exclude == 0
// Remove people who are not white, black , or hispanic
drop if racethn == 3
// Remove person-months that aren't in 20s.
drop if age1 == 1 | age1 == .
// Remove person-months that aren't in the periods of analysis
drop if period == .

** Sample counts for women in 20s 
// Person-months in analysis, which includes white, black, and hispanic women
count if racethn != 3 & nmarsta != .

// Count of women, in which total number of person-months are nested
egen Nwomen = tag(CASEID)
count if Nwomen == 1 

// Distribution of observations by survey
tab wave 

// Number of observations per person
// count number of person-months by CASEID - var has same value within CASEID
bysort CASEID: generate obsperperson = _N
// summarize one observation per per (tagged with Nwomen == 1) if in analytic sample
sum obsperperson if Nwomen == 1
// hist obsperperson if Nwomen == 1 & racethn != 3, discrete percent

// Number of pregnancies total
count if p == 1

// Number of pregnancies that haven't resulted in births by time of interview
// but are at least 3mo along
tab mobeforeinterview conceptionmo_ongoing 
// list CASEID wave if mobeforeinterview == 28 & conceptionmo_ongoing == 1
count if conceptionmo_ongoing == 1 
count if mobeforeinterview >= 3 & mobeforeinterview <= 9 

// Number of pregnancies per woman
bysort CASEID: egen pregsperperson = total(p)
tab pregsperperson if Nwomen == 1 


** Sexually active Black women in their 20s
// Number of person-months among sexually active Black
count if HISPRACE == 3 & ((nmarsta == 1 | nmarsta == 2) & sexmonth == 1)

// Number of women in which these person-months are nested
// marst can change, so we need to count number of women who are *ever* sexually active or cohabiting
drop Nwomen obsperperson pregsperperson


// only include black women
drop if HISPRACE != 3
// determine thier lowest value of nmarsta
// Gen a var that is the minimum value of marst w/in one person-- 
// cohab == 1, sexually active == 2, single not sexually active == 3
bysort CASEID: egen minmarst = min(nmarsta)
drop if minmarst == 3

// gen indicator of whether that person-month is included in analysis
// drop those excluded
gen countobs = 1 if (nmarsta == 1 | nmarsta == 2) & sexmonth == 1
drop if countobs != 1
count // should match line 52; it does!

// gen a count of the women in which the person-months are nested
egen Nwomen = tag(CASEID)
cou if Nwomen == 1 & racethn == 1

bysort CASEID: generate obsperperson = _N
// summarize one observation per per (tagged with Nwomen == 1) if in analytic sample
sum obsperperson if Nwomen == 1 & racethn == 1

// Number of pregnancies among B&H sexually active person-months
tab p
bysort CASEID: egen pregsperperson = total(p)
tab pregsperperson if Nwomen == 1

restore

********************************************
** How many are excluded for each reason? **
********************************************

// We'll run through the exclusion criteria, which are generally applied on a person-month
// basis, and see how many *whole* people are removed from the analysis
// e.g. if someone is always married for all of the months we observe, or if they have missing
// data for all sexual activity.

// Remove people who are not white, black , or hispanic
drop if racethn == 3
// Remove person-months that aren't in 20s.
drop if age1 == 1 | age1 == . 
// Remove person-months that aren't in the periods of analysis
drop if period == .

egen Nwomen = tag(CASEID)
count if Nwomen == 1
global totaln `r(N)'

gen excludecount = 0

** misschk
// missing important wide data on marital / cohab start/stop dates
replace excludecount = 1 if misschk == 1
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1

** Marital status
// exclude all married person-months
replace excludecount = 1 if mc == 1
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafteralwaysmarried `r(N)'

** Sexual Activity
// Sexual activity data are only collected for up to 48 months
// prior to interview; drop all person-months in which there
// are no data on sexual activity
replace excludecount = 1 if sexmonth == .
drop if excludecount == 1 
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global naftersexmonth `r(N)'

// Drop months in which there is no sexual activity among cohabiting couples, and
// drop months in which sexual activity is unknown among single people
replace excludecount = 1 if sexmonth == .
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafternosexcohabmar `r(N)'

display ($naftersexmonth - $nafternosexcohabmar)

** Contraceptive use
// If there is sexual activity, but unknown/refused contraceptive use, remove from analysis
replace excludecount = 1 if cpmethodmonth == 99 & sexmonth == 1
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafternocp `r(N)'

** Contraceptive calendar
// Exclude person-months that are further back in time than cp calendar 
// these are likely already excluded based on sexmonth data, but double checking here
replace excludecount = 1 if cmyear < CMJAN3YR
// also make sure we're only including observations in the period of interest
replace excludecount = 1 if year < 2003 
replace excludecount = 1 if year > 2015 & year < .
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafteroutsidecalendar `r(N)'

** close to interview
replace excludecount = 1 if mobeforeinterview <= 3
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1

** currently pregnant
replace excludecount = 1 if censorpreg == 1
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1

** Age
// Keep data for person months bewteen ages 15-29
replace excludecount = 1 if (agemonth <= 180 | agemonth >= 360)
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafteroutsideage `r(N)'

** Nonmarital cohabitation
// If there are greater than 4 non-marital cohabs,
// exclude person months after cohab 4 ends if there are more than 5 nonmarital cohabs (NONMARR >= 5) 
replace excludecount = 1 if (cohpend4 < agemonth) & (NONMARR > 5 & NONMARR < .)
// If there are exactly 5 non-marital cohabs, exclude person-months after the 4th if the 5th 
// cohab is not ongoing (cohp5 == .)
replace excludecount = 1 if (cohpend4 < agemonth) & (NONMARR == 5 & cohp5 == .)
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global naftercohabdatalimits `r(N)'


** Infecundity
// There are some people who are infecund but do not report a date for sterilization
// We'll exclude them from the analysis because we don't know when they stopped
// being able to concieve and therefore likely have their contraceptive history miscoded.
replace excludecount = 1 if infecundnosterdate == 1
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafterinfecundity `r(N)'
