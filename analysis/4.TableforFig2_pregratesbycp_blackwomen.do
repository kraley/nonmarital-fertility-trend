// Generate a table of pregnancy rates by contraceptive method across 3 periods
// of analysis. Test for significant differences in rates.
// This table complements figure 2 -- it reports preg rates aggregated into periods
// and runs statistical tests

use "$NSFGKeep/cohabfertlong_appended.dta", clear

// svyset
svyset SECU [pweight = xrndweight], strata(SEST)

** exclusion criteria
// keep only teens and twenties
gen agegroup = 1 if age1 == 1
replace agegroup = 2 if inlist(age1, 2, 3)
label define agegroup 1 "Teens" 2 "Twenties"
label values agegroup agegroup
// keep only 20s
replace exclude = 1 if agegroup == . | agegroup == 1

// keep only cohabiting or sexualy active person-months
replace exclude = 1 if nmarsta != 1 & nmarsta != 2
replace exclude = 1 if sexmonth != 1

// only black and hispanic women
gen raceethn = .
replace raceethn = 1 if HISPRACE == 1 
replace raceethn = 2 if HISPRACE == 3
replace raceethn = 3 if HISPRACE == 2
label define raceethn 1 "Hispanic" 2 "Black" 3 "White"
label values raceethn raceethn

// Collapse into 4 (?) categories
// Permanet, LARC + hormonal, LEM, none
recode cpmethodcat5 (3 = 2) (4 = 3) (5 = 4), gen(cpmethodcat4)
label define cpmethodcat4 1 "Permanent" 2 "Hormonal and IUD" 3 "Condoms, withdrawal, other" 4 "None"
label values cpmethodcat4 cpmethodcat4

********************************************
** Pregnancy rates by method, age, period ** 
********************************************
preserve

// Can drop excluded cases in the preg rate tabulations because we're not relying on std errrors
drop if exclude == 1

// Loop over age groups, years, and methods
forvalues re = 1/3 {
	forvalues period = 1/3{
		forvalues method = 1/4 {
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
					svy: mean p if cpmethodcat4 == `method' & raceethn == `re' & period == `period'
					// Multiply the person-month mean by 12*1000 to get an annual rate, and store in 
					// a (one cell) matrix.
					matrix ppregs`re'`period'`method' = e(b)*(12*1000)
				}
			}
			// Calculate preg rate for that age group & year
			svy: mean p if raceethn == `re' & period == `period' 
			matrix ppregstotal`re'`period' = e(b)*(12*1000)
		}
	}
}

// vertically append matricies
// first, within age group, append pregnancies across methods within a given period
forvalues re = 1/3 {
	forvalues period = 1/3 {
		matrix ppregs`re'`period' = ppregs`re'`period'1\ppregs`re'`period'2\ppregs`re'`period'3\ppregs`re'`period'4\ppregstotal`re'`period'
	}
}

// horizontally append across all period
forvalues re = 1/3 {
	matrix ppregs`re' = ppregs`re'1
	forvalues period = 2/3 {
		matrix ppregs`re' = ppregs`re', ppregs`re'`period'
	}
}

// restore data with cases that have exclude == 1 , because we will need to use
// subpop command to test for significant differences
restore

** Significance tests for pregnancy rates by method across periods

gen period1v2 = .
replace period1v2 = 0 if period == 1
replace period1v2 = 1 if period == 2

gen period2v3 = .
replace period2v3 = 0 if period == 2 
replace period2v3 = 1 if period == 3

gen period1v3 = .
replace period1v3 = 0 if period == 1
replace period1v3 = 1 if period == 3

// reg by age group, period, method
local period1 1 2 1
local period2 2 3 3
// reg by age group, period, method
forvalues re = 1/3 {
	forvalues method = 1/4 {
		local i = 1
		foreach comp in 1v2 2v3 1v3 {
		local per1: word `i' of `period1'
		local per2: word `i' of `period2'
		count if p == 1 & exclude == 0 & raceethn == `re' & cpmethodcat4 == `method' & period == `per1'
		local per1count = `r(N)'

		count if p == 1 & exclude == 0 & raceethn == `re' & cpmethodcat4 == `method' & period == `per1'
		local per2count = `r(N)'

		// if there are no pregnancies, set to 0
		if (`per1count' == 0 | `per2count' == 0) {
			di "No pregs in group r/e `re' period (`per1' or `per2') method `method'"
			gen pval`re'`method'`comp' = .
			}
		else {
			svy, subpop(if exclude == 0 & raceethn == `re' & cpmethodcat4 == `method'):logit p period`comp'
		
			gen pval`re'`method'`comp' = 0
			replace pval`re'`method'`comp' = e(p) if e(p) != .
			}
		local i = `i' + 1
		}
	}
}

// reg by age group, period
forvalues re = 1/3 {
	foreach comp in 1v2 2v3 1v3 {
		svy, subpop(if exclude == 0 & raceethn == `re'):logit p period`comp'
		gen pval`re'`comp' = 0
		replace pval`re'`comp' = e(p) if e(p) != .
		}
}


*********************
** Export to excel **
*********************

putexcel set "$results/pregrates_twenties_bymethod_byrace.xlsx", replace

putexcel A1  = "Table. Trends in pregnancy rates among sexually active Black and Hispanic women in their 20s period, 2003-2015"
putexcel C2 = "2004-06"
putexcel D2 = "2008-10"
putexcel E2 = "2012-14"
putexcel F2 = "04-06 vs 08-10 p-value"
putexcel G2 = "08-10 2 vs 12-14 p-value"
putexcel H2 = "04-06 1 vs 12-14 p-value"

putexcel A3  = "Hisp" B3 = "Permanent"
putexcel B4  = "Hormonal and IUDs"
putexcel B5  = "Condoms, withdrawal, other"
putexcel B6  = "No method"
putexcel B7  = "Total"
putexcel A8  = "Black" B8 = "Permanent"
putexcel B9  = "Hormonal and IUDs"
putexcel B10 = "Condoms, withdrawal, other"
putexcel B11 = "No method"
putexcel B12 = "Total"
putexcel A13  = "White" B13 = "Permanent"
putexcel B14  = "Hormonal and IUDs"
putexcel B15 = "Condoms, withdrawal, other"
putexcel B16 = "No method"
putexcel B17 = "Total"


// add in matricies
putexcel C3   = matrix(ppregs1), nformat(###.#)
putexcel C8   = matrix(ppregs2), nformat(###.#)
putexcel C13  = matrix(ppregs3), nformat(###.#)

// Hisp by method
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 3
	forvalues method = 1/4 {
			local pval : di %5.4f = pval1`method'`comp'
			putexcel `col'`row' = `pval', nformat(#.###)
			local row = `row' + 1
		}
	local i = `i' + 1
}

// total hisp
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col: word `i' of `column'
	local pval : di %5.4f = pval1`comp'
	putexcel `col'7 = `pval', nformat(#.###)
	local i = `i' + 1
}

// blakc by method
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 8
	forvalues method = 1/4 {
		local pval : di %5.4f = pval2`method'`comp'
		putexcel `col'`row' = `pval', nformat(#.###)
		local row = `row' + 1
	}
	local i = `i' + 1
}

// total black
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col: word `i' of `column'
	local pval : di %5.4f = pval2`comp'
	putexcel `col'12 = `pval', nformat(#.###)
	local i = `i' + 1
}

// white by method
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 13
	forvalues method = 1/4 {
		local pval : di %5.4f = pval3`method'`comp'
		putexcel `col'`row' = `pval', nformat(#.###)
		local row = `row' + 1
	}
	local i = `i' + 1
}

// total white
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col: word `i' of `column'
	local pval : di %5.4f = pval3`comp'
	putexcel `col'17 = `pval', nformat(#.###)
	local i = `i' + 1
}
