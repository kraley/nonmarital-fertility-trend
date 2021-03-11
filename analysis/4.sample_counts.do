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

************************************
** Sample size - reported in text **
************************************
preserve 

// Remove if doesn't meet previously set inclusion criteria
keep if exclude == 0

// Person-months in analysis, which includes white, black, and hispanic women
count if racethn != 3

// Count of women, in which total number of person-months are nested
egen Nwomen = tag(CASEID)
count if Nwomen == 1 & racethn != 3

// Distribution of observations by survey
tab wave if racethn != 3

// Number of observations per person
// count number of person-months by CASEID - var has same value within CASEID
bysort CASEID: generate obsperperson = _N
// summarize one observation per per (tagged with Nwomen == 1) if in analytic sample
sum obsperperson if Nwomen == 1 & racethn != 3
// hist obsperperson if Nwomen == 1 & racethn != 3, discrete percent

// Number of pregnancies total
count if p == 1 & racethn != 3

// Number of pregnancies per woman
bysort CASEID: egen pregsperperson = total(p)
tab pregsperperson if Nwomen == 1 & racethn != 3


** Sexually active Black and Hispanic women
// Number of person-months among sexually active Black and Hispanic women
count if racethn == 1 & (nmarsta == 1 | nmarsta == 2)

// Number of women in which these person-months are nested
// marst can change, so we need to count number of women who are *ever* sexually active or cohabiting
drop Nwomen obsperperson pregsperperson


// only include black and hisp women
drop if racethn != 1
// determine thier lowest value of nmarsta
// Gen a var that is the minimum value of marst w/in one person-- 
// cohab == 1, sexually active == 2, single not sexually active == 3
bysort CASEID: egen minmarst = min(nmarsta)
drop if minmarst == 3

// gen indicator of whether that person-month is included in analysis
// drop those excluded
gen countobs = 1 if nmarsta == 1 | nmarsta == 2
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

keep if racethn == 1 | racethn == 2
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global totaln `r(N)'

gen excludecount = 0

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

// Drop months in which there is no sexual activity among married and cohabiting couples, and
// drop months in which sexual activity is unknown among single people
replace excludecount = 1 if marst == .
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafternosexcohabmar `r(N)'

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

** Marital status
// exclude all married person-months
replace excludecount = 1 if mc == 1
drop if excludecount == 1
drop Nwomen
egen Nwomen = tag(CASEID)
count if Nwomen == 1
global nafteralwaysmarried `r(N)'

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

// how many lost to infecundity w/o dates
display $nafteralwaysmarried - $nafterinfecundity
