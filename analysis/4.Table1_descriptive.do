** Table 1
// Describe fertile pregnancy rate by age, stratified by race/ethnicity
// to be compared with NCHS data on preg rates.

use "$NSFGKeep/cohabfertlong_appended.dta", clear

// svyset data
svyset SECU [pweight = xrndweight], strata(SEST)

*********************************************************
** Table 1: Preg rates by r/e (Black, Hispanic, White) **
*********************************************************


** set put excel file
putexcel set "$results/Table1_BandHsep.xlsx", replace

recode HISPRACE (3=1) (1=2) (2=3), gen(re)
label define re 1 "Non-Hispanic Black" 2 "Hispanic" 3 "Non-Hispanic White"
label values re re

preserve 

keep if exclude == 0

** Set labels for putexcel file, effectively creating a table shell
putexcel A1 = "Table 1. Non-marital fertile pregnancy rates by age, period, and race/ethnicity, 2004-2014"
putexcel C2 = ("2004-06") D2 = ("2008-10") E2 = ("2011-14") F2 = ("Change 04-06 to 08-10") H2 = ("Change 08-10 to 12-14") J2 = ("Change 04-06 to 12-14")
putexcel A3 = "Non-Hispanic Black" B3 = "20-24"
putexcel B4 = "25-29"
putexcel B5 = "20-29"
putexcel B6 = "Person-months"
putexcel B7 = "Women"


putexcel C8  = ("2004-06") D8 = ("2008-10") E8 = ("2011-14") F8 = ("Change 04-06 to 08-10") H8 = ("Change 08-10 to 12-14") J8 = ("Change 04-06 to 12-14")
putexcel A9  = "Hispanic" B9 = "20-24"
putexcel B10 = "25-29"
putexcel B11 = "20-29"
putexcel B12 = "Person-months"
putexcel B13 = "Women"


putexcel C14 = ("2004-06") D14 = ("2008-10") E14 = ("2011-14") F14 = ("Change 04-06 to 08-10") H14 = ("Change 08-10 to 12-14") J14 = ("Change 04-06 to 12-14")
putexcel A15 = "Non-Hispanic White" B15 = "20-24"
putexcel B16 = "25-29"
putexcel B17 = "20-29"
putexcel B18 = "Person-months"
putexcel B19 = "Women"

// Populate the table shell with estimated pregnancy rates for target groups

// Age sub-categories
local column C D E									// periods across columns
local row = 3										// ages across rows
forvalues r = 1/3 { 								// race/ethnicity in 3 categories (b, h, w)
	forvalues a = 2/3{								// age in 3=2 categories (20-24, 25-29)
		forvalues p = 1/3 { 						// 3 periods of analysis
			local col : word `p' of `column'
			quietly svy, subpop(if exclude == 0): mean p if period == `p'  & age1 == `a' & re == `r'
			matrix pregs = e(b)*(12*1000)
			putexcel `col'`row' = matrix(pregs), nformat("0.0")
		}
		local row=`row' + 1
	}
	local row = `row' + 4
}

// Overall estimates for women ages 20-29
local column C D E
local row = 5
forvalues r = 1/3 {
		forvalues p=1/3{
			local col : word `p' of `column'
			svy, subpop(if exclude == 0): mean p if period == `p'  & re == `r' & inrange(age1,2,3)
			matrix pregs`r'`p' = e(b)*(12*1000)
			putexcel `col'`row' = matrix(pregs`r'`p'), nformat("0.0")
	}
	local row = `row' + 6
}

// Count person-months and individuals
// drop teens
keep if age1 == 2 | age1 == 3

local column C D E
local row = 6
local rowb = 7
forvalues r = 1/3 {
		forvalues p = 1/3{
			local col : word `p' of `column'

			// person months
			count if period == `p' & re == `r'
			matrix ccount`r'`p' = r(N)
			putexcel `col'`row' = matrix(ccount`r'`p'), nformat(number_sep)

			// individuals
			unique CASEID if period == `p' & re == `r'
			matrix pcount`r'`p' = r(unique)
			putexcel `col'`rowb' = matrix(pcount`r'`p'), nformat(number_sep)
	}
	local row = `row' + 6
	local rowb = `rowb' + 6
}


restore

***********************************************************
** Significance tests for pregnancy rates across periods **
***********************************************************

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
forvalues agegroup = 2/3 {
	forvalues re = 1/3 {
		foreach comp in 1v2 2v3 1v3 {
		display as error "Evaluating r/e `re' and agegroup `agegroup'"
		svy, subpop(if exclude == 0 & age1 == `agegroup' & re == `re' ):logit p period`comp'
		matrix results = r(table)
		scalar pval`agegroup'`re'`comp' = results[4, 1]
		}
	}
}


// reg for all women in 20s by racethn, period
preserve

replace exclude = 1 if age1 == 1
forvalues re = 1/3 {
	foreach comp in 1v2 2v3 1v3 {
		display as error "Evaluating r/e `re' ages 20-29'"
		svy, subpop(if exclude == 0 & re == `re'):logit p period`comp'
		matrix results = r(table)
		scalar pval`re'`comp' = results[4, 1]
		}
}

restore

// Calculates the change in rates between adjacent periods. Outer loop is by race/ethnic group
// Next is about comparison (early versus later).
// Innermost loop is by age.

local column C D E
local pointA = 0

foreach re in 3 9 15 {
	local rl = `re' + 2
	foreach c in F H {
		local pointA=`pointA'+1
		local pointB=`pointA'+1
		local pointcolA : word `pointA' of `column'
		local pointcolB : word `pointB' of `column'
		forvalues r = `re' / `rl' {
			putexcel `c'`r'=formula((+`pointcolB'`r')-(`pointcolA'`r')), nformat("0.0")
		}
	}
	local pointA=0
} 

// Calculates change in rates from earliest to latest period
local column C D E
local pointA = 0

foreach re in 3 9 15 {
	local rl = `re' + 2

	local pointA=`pointA'+1
	local pointB=`pointA'+2
	
	local pointcolA : word `pointA' of `column'
	local pointcolB : word `pointB' of `column'
	
	forvalues r = `re' / `rl' {
			putexcel J`r'=formula((+`pointcolB'`r')-(`pointcolA'`r')), nformat("0.0")
		}	
	local pointA=0
}


// Populates p-values with sig stars

// Black by age group
local column G I K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 3
	forvalues agegroup = 2/3 {

		if pval`agegroup'1`comp' < 0.1 {
			putexcel `col'`row' = "*"
		}
		if pval`agegroup'1`comp' < 0.05 {
			putexcel `col'`row' = "**"
		}
		if pval`agegroup'1`comp' < 0.01 {
			putexcel `col'`row' = "***"
		}

		local row = `row' + 1
	}
	local i = `i' + 1
}

// Black ages 20-29
local column G I K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 5

 	if pval1`comp' < 0.1 {
		putexcel `col'`row' = "*"
	}
	if pval1`comp' < 0.05 {
		putexcel `col'`row' = "**"
	}
	if pval1`comp' < 0.01 {
		putexcel `col'`row' = "***"
	}

	local i = `i' + 1
}


// Hisp by age group
local column G I K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 9
	forvalues agegroup = 2/3 {

		if pval`agegroup'2`comp' < 0.1 {
			putexcel `col'`row' = "*"
		}
		if pval`agegroup'2`comp' < 0.05 {
			putexcel `col'`row' = "**"
		}
		if pval`agegroup'2`comp' < 0.01 {
			putexcel `col'`row' = "***"
		}

		local row = `row' + 1
	}
	local i = `i' + 1
}

// Hisp ages 20-29
local column G I K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 11

 	if pval2`comp' < 0.1 {
		putexcel `col'`row' = "*"
	}
	if pval2`comp' < 0.05 {
		putexcel `col'`row' = "**"
	}
	if pval2`comp' < 0.01 {
		putexcel `col'`row' = "***"
	}

	local i = `i' + 1
}

// White by age group
local column G I K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 15
	forvalues agegroup = 2/3 {

		if pval`agegroup'3`comp' < 0.1 {
			putexcel `col'`row' = "*"
		}
		if pval`agegroup'3`comp' < 0.05 {
			putexcel `col'`row' = "**"
		}
		if pval`agegroup'3`comp' < 0.01 {
			putexcel `col'`row' = "***"
		}

		local row = `row' + 1
	}
	local i = `i' + 1
}

// White ages 20-29
local column G I K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 17

 	if pval3`comp' < 0.1 {
		putexcel `col'`row' = "*"
	}
	if pval3`comp' < 0.05 {
		putexcel `col'`row' = "**"
	}
	if pval3`comp' < 0.01 {
		putexcel `col'`row' = "***"
	}

	local i = `i' + 1
}


putexcel A20 = "*p<0.1, **p<0.05, ***p<0.01"