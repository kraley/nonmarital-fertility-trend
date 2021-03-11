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
replace exclude = 1 if agegroup == .

// keep only cohabiting or sexualy active person-months
replace exclude = 1 if nmarsta != 1 & nmarsta != 2

// only black and hispanic women
replace exclude = 1 if HISPRACE != 1 & HISPRACE != 3

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
forvalues agegroup = 1/2 {
	forvalues period = 1/3{
		forvalues method = 1/4 {
			count if cpmethodcat4 == `method' & agegroup == `agegroup' & period == `period' & exclude == 0
			// if there are no observations recorded for that method, age, period
			// record a 0 in the matrix for pregnancy rates
			if r(N) == 0 {
				matrix ppregs`agegroup'`period'`method' = 0
			}
			else {
				// count if there are any pregnancies in this group
				count if p == 1 & cpmethodcat4 == `method' & agegroup == `agegroup' & period == `period' & exclude == 0
				// if there are no pregnancies, set to 0
				if r(N) == 0 {
					matrix ppregs`agegroup'`period'`method' = 0
				}
				else {
					// If there are pregnancies for this group, 
					// estimate the mean of p, indicator of a pregnancy
					svy: mean p if cpmethodcat4 == `method' & agegroup == `agegroup' & period == `period'
					// Multiply the person-month mean by 12*1000 to get an annual rate, and store in 
					// a (one cell) matrix.
					matrix ppregs`agegroup'`period'`method' = e(b)*(12*1000)
				}
			}
			// Calculate preg rate for that age group & year
			svy: mean p if agegroup == `agegroup' & period == `period' 
			matrix ppregstotal`agegroup'`period' = e(b)*(12*1000)
		}
	}
}

// vertically append matricies
// first, within age group, append pregnancies across methods within a given period
forvalues agegroup = 1/2 {
	forvalues period = 1/3 {
		matrix ppregs`agegroup'`period' = ppregs`agegroup'`period'1\ppregs`agegroup'`period'2\ppregs`agegroup'`period'3\ppregs`agegroup'`period'4\ppregstotal`agegroup'`period'
	}
}

// horizontally append across all period
forvalues agegroup = 1/2 {
	matrix ppregs`agegroup' = ppregs`agegroup'1
	forvalues period = 2/3 {
		matrix ppregs`agegroup' = ppregs`agegroup', ppregs`agegroup'`period'
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
forvalues agegroup = 1/2 {
	forvalues method = 1/4 {
		foreach comp in 1v2 2v3 1v3 {
		svy, subpop(if exclude == 0 & agegroup == `agegroup' & cpmethodcat4 == `method'):reg p period`comp'
		
		gen pval`agegroup'`method'`comp' = 0
		replace pval`agegroup'`method'`comp' = e(p) if e(p) != .
		}
	}
}

// reg by age group, period
forvalues agegroup = 1/2 {
	foreach comp in 1v2 2v3 1v3 {
		svy, subpop(if exclude == 0 & agegroup == `agegroup'):reg p period`comp'
		gen pval`agegroup'`comp' = 0
		replace pval`agegroup'`comp' = e(p) if e(p) != .
		}
}

*********************
** Export to excel **
*********************

putexcel set "$results/Table4.xlsx", replace

putexcel A1  = "Table 4. Trends in pregnancy rates among sexually active Black and Hispanic women by age and period, 2003-2015"
putexcel C2 = "2003-06"
putexcel D2 = "2007-10"
putexcel E2 = "2011-15"
putexcel F2 = "03-06 vs 07-10 p-value"
putexcel G2 = "07-10 2 vs 11-15 p-value"
putexcel H2 = "03-06 1 vs 11-15 p-value"

putexcel A3  = "Teens" B3 = "Permanent"
putexcel B4  = "Hormonal and IUDs"
putexcel B5  = "Condoms, withdrawal, other"
putexcel B6  = "No method"
putexcel B7  = "Total"
putexcel A8  = "Twenties" B8 = "Permanent"
putexcel B9  = "Hormonal and IUDs"
putexcel B10 = "Condoms, withdrawal, other"
putexcel B11 = "No method"
putexcel B12 = "Total"


// add in matricies
putexcel C3  = matrix(ppregs1), nformat(###.#)
putexcel C8  = matrix(ppregs2), nformat(###.#)

// teens
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

// total teens
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col: word `i' of `column'
	local pval : di %5.4f = pval1`comp'
	putexcel `col'7 = `pval', nformat(#.###)
	local i = `i' + 1
}

// twenties
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

// total twens
local column F G H
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col: word `i' of `column'
	local pval : di %5.4f = pval2`comp'
	putexcel `col'12 = `pval', nformat(#.###)
	local i = `i' + 1
}
