**********************
** Append wide data **
**********************

use "$NSFGKeep/0610cohabfertwide.dta", clear
append using "$NSFGKeep/1113cohabfertwide.dta"
append using "$NSFGKeep/1315cohabfertwide.dta"


// Merge weights into one variable from across rounds

// xrndweight is a cross-round rescaled weight so that each round's average is 1
gen xrndweight = .
replace xrndweight = weight06 if wave == 610
replace xrndweight = weight11 if wave == 1113
replace xrndweight = weight13 if wave == 1315
cou if xrndweight == .

// unscaled weight is each round's weight merged into one variable, not rescaled
gen unscaledweight = .
replace unscaledweight = WGTQ1Q16     if wave == 610
replace unscaledweight = WGT2011_2013 if wave == 1113
replace unscaledweight = WGT2013_2015 if wave == 1315
cou if unscaledweight == .

/* If we're to not rescale weights, uncomment:
drop xrndweight
rename unscaledweight xrndweight
*/

save "$NSFGKeep/cohabfertwide_appended.dta", replace

**********************
** Append long data **
**********************

// load and append all 3 rounds of the NSFG data
use "$NSFGKeep/0610cohabfertlongrecodes.dta", clear
append using "$NSFGKeep/1113cohabfertlongrecodes.dta"
append using "$NSFGKeep/1315cohabfertlongrecodes.dta"

tab year
// Generate a period variable, which groups observations by the person-month of observation.
gen period = .
replace period = 1 if year >= 2004 & year < 2007
replace period = 2 if year >= 2008 & year < 2011
replace period = 3 if year >= 2012 & year < 2014
label define period 1 "04-06" 2 "08-10" 3 "12-14"
label values period period
// Exclude observations outside of the study periods
// replace exclude = 1 if period == .


// Exclude observations from case 31005
// this is someone who reports a pregnancy while she is a teenager
// while using an IUD. Because we have a small n of LARC person-months, this
// pregnancy results in a high pregnancy rate for teen LARC users during the period
// 2003-2006. We've decided to remove this case, which does not substantively influence
// interpretation but does clean up our results.
replace exclude = 1 if CASEID == 31005

** Create cross-round weights

// xrndweight is a cross-round rescaled weight so that each round's average is 1
gen xrndweight = .
replace xrndweight = weight06 if wave == 610
replace xrndweight = weight11 if wave == 1113
replace xrndweight = weight13 if wave == 1315
cou if xrndweight == .

// unscaled weight is each round's weight merged into one variable, not rescaled
gen unscaledweight = .
replace unscaledweight = WGTQ1Q16     if wave == 610
replace unscaledweight = WGT2011_2013 if wave == 1113
replace unscaledweight = WGT2013_2015 if wave == 1315
cou if unscaledweight == .

/* If we're to not rescale weights, uncomment:
drop xrndweight
rename unscaledweight xrndweight
*/

save "$NSFGKeep/cohabfertlong_appended.dta", replace

