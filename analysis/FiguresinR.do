// FiguresinR
// We wish to create two figures
// Figure 1 - plot of nonmarital fertility rates using NCHS data on fertility rates
// 			  this data is downloaded from the NCHS
// Figure 2 - plot of fertility rates among sexually active black and hispanic women by
//			  contraceptive method used. This comes from our analysis file.

// This script will do the following:
// 1) Generate data for Figure 2 from recodes that will get exported to an excel spreadsheet
// 2) Run a set-up file in R that will then allow you to 
// 3) Read in NCHS data for Figure 1, generate Figure 1
// 4) Read in exported data from step 1 to generate Figure 2

// Generate a graph of pregnancy rates by contraceptive method across 3 periods
// of analysis. 

*******************************************************************
** Prepare and export data for Figure 2 - by race only among 20s **
*******************************************************************

use "$NSFGKeep/cohabfertlong_appended.dta", clear

// svyset
svyset SECU [pweight = xrndweight], strata(SEST)

** exclusion criteria
// keep only twenties
gen agegroup = 1 if age1 == 1
replace agegroup = 2 if inlist(age1, 2, 3)
label define agegroup 1 "Teens" 2 "Twenties"
label values agegroup agegroup
replace exclude = 1 if agegroup == . | agegroup == 1

// keep only cohabiting or sexualy active person-months
replace exclude = 1 if nmarsta != 1 & nmarsta != 2
replace exclude = 1 if sexmonth != 1

// only black and hispanic women
replace exclude = 1 if HISPRACE != 1 & HISPRACE != 3
gen raceethn = .
replace raceethn = 1 if HISPRACE == 1 
replace raceethn = 2 if HISPRACE == 3
label define raceethn 1 "Hispanic" 2 "Black"
label values raceethn raceethn

// Collapse into 4 categories
// Permanet, LARC + hormonal, LEM, none
recode cpmethodcat5 (3 = 2) (4 = 3) (5 = 4), gen(cpmethodcat4)
label define cpmethodcat4 1 "Permanent" 2 "Hormonal and IUD" 3 "Condoms, withdrawal, other" 4 "None"
label values cpmethodcat4 cpmethodcat4

********************************************
** Pregnancy rates by method, race/ethn, period ** 
********************************************

// Loop over raceethn groups, years, and methods and store results in a 1 row x 2 column matrix that contains
// monthly preg rate and variance
forvalues re = 1/2 {
	forvalues period = 1/3{
		forvalues method = 2/4 { // not including ster
			count if cpmethodcat4 == `method' & raceethn == `re' & period == `period' & exclude == 0
			// if there are no observations recorded for that method, age, period
			// record a 0 in the matrix for pregnancy rates
			if r(N) == 0 {
				matrix ppregs`re'`period'`method' = 0
			}
			else {
				// count if there are any pregnancies in this group
				count if p == 1 & cpmethodcat4 == `method' & raceethn == `re' & period == `period' & exclude == 0
				// if there are no pregnancies, set to 0
				if r(N) == 0 {
					matrix ppregs`re'`period'`method' = 0
				}
				else {
					// If there are pregnancies for this group, 
					// estimate the mean of p, indicator of a pregnancy
					svy, subpop(if exclude == 0 & cpmethodcat4 == `method' & raceethn == `re' & period == `period'): mean p 
					// Store preg rate and variange in a matrix
					matrix pregs`re'`period'`method' = e(b), e(V)
				}
			}
		}
		// Calculate overall pregnancy rate for each age group and period
		svy, subpop(if exclude == 0 & raceethn == `re' & period == `period'): mean p 
		matrix pregs`re'`period' = e(b), e(V)
	}
}

// get overall rates

// Export rates and variance by age, period, method
putexcel set "$NSFGKeep/pregratesbymethodbyraceethn20s", replace
putexcel A1 = ("raceethn") B1 = ("period") C1 = ("method") D1 = ("pregrate") E1 = ("var_pregrate")

// append all matricies with data on which group, period, method
local row = 2 
forvalues re = 1/2 {
	forvalues period = 1/3 {
		forvalues method = 2/4 {
		matrix pregs = `re', `period', `method' , pregs`re'`period'`method'
		putexcel A`row' = matrix(pregs)
		local row = `row' + 1
		}
	// exporting the overall values as well, with 0 in the matrix as a placeholder for the
	// "method type" so that this actually fills in the spreadsheet we set up.
	// It'll serve as category label in R for "overall" trends, too.
	matrix pregs = `re', `period', 0, pregs`re'`period'
	putexcel A`row' = matrix(pregs)
	local row = `row' + 1
	}
}

****************************
** Run R scripts in Stata **
****************************
// Used this tutorial to learn rsource 
// https://fsolt.org/blog/2018/08/15/switch-to-r/


// The line below opens up an R session; syntax is different
// The code sources the general setup file for this project, which
// includes your personal setup file, and then sources the Figure-generating script
// Oddly, it seems like rsource reall doesn't like you indenting-- throws error at end saying that 
// matching close brace is missing.
if "`c(os)'"=="MacOSX" | "`c(os)'"=="UNIX" {
rsource, terminator(END_OF_R) rpath("/usr/local/bin/R") roptions(`"--vanilla"')

source("setup_cohab_fertility.R")

# Check to make sure directory location is correct
print(scripts)
print(personal_setup)
print(original)
print(NSFGKeep)
print(results)

source("analysis/Figures.R")

END_OF_R
}   // We're out of R now!

// Note: KLB on a mac, so not sure if this actually works as intended.
if ("`c(os)'" == "Windows") {
rsource, terminator(END_OF_R) rpath("/usr/local/bin/R") roptions(`"--vanilla"')

source("setup_cohab_fertility.R")

source("analysis/Figures.R")

END_OF_R 

}
