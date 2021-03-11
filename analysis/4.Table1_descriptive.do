** Table 1: Descriptive table
// Describe pregnancy rate by age, stratified by race/ethnicity
// to be compared with NCHS data on preg rates.

use "$NSFGKeep/cohabfertlong_appended.dta", clear

// svyset data
svyset SECU [pweight = xrndweight], strata(SEST)

// Recode race/ethnicity so that black and hispanic are in one group, 
// white women are in another group
local racethn "Black/Hispanic White Total"
recode HISPRACE (1=1)(3=1)(2=2)(4=3), gen(racethn)
label define racethn 1 "Black or Hispanic" 2 "NH White" 3 "NH other"
label values racethn racethn

*************************
** Descriptive tables  **
*************************

// Table 1: 
// pregnancy rates by age and period by race/ethnicity

// We aren't estimating SEs in this table, so we can drop everyone with exclude == 0
preserve
keep if exclude == 0

** set put excel file
putexcel set "$results/Table1.xlsx", replace

** Set labels for putexcel file, effectively creating a table shell
putexcel A1 = "Table 1. Non-marital fertile pregnancy rates by age, period, and race/ethnicity, 2003-2015"
putexcel C2 = ("2003-06") D2 = ("2007-10") E2 = ("2011-15") F2 = ("Change 03-06 to 07-10") G2 = ("Change 07-10 to 11-15") H2 = ("Change 03-06 to 11-15")
putexcel A3 = "Black and Hispanic" B3 = "15-19"
putexcel B4 = "20-24"
putexcel B5 = "25-29"
putexcel B6 = "15-29"
putexcel B7 = "N"


putexcel C9 = ("2003-06") D9 = ("2007-10") E9 = ("2011-15") F9 = ("Change 03-06 to 07-10") G9 = ("Change 07-10 to 11-15") H9 = ("Change 03-06 to 11-15")
putexcel A10 = "Non-Hispanic White" B10 = "15-19"
putexcel B11 = "20-24"
putexcel B12 = "25-29"
putexcel B13 = "15-29"
putexcel B14 = "N"

// Populate the table shell with estimated pregnancy rates for target groups

// Age sub-categories
local column C D E									// periods across columns
local row = 3										// ages across rows
forvalues r = 1/2 { 								// race/ethnicity in 2 categories (b/h, w)
	forvalues a = 1/3{								// age in 3 categories
		forvalues p = 1/3{ 							// 3 periods of analysis
			local col : word `p' of `column'
			quietly svy, subpop(if exclude == 0): mean p if period == `p'  & age1 == `a' & racethn == `r'
			matrix pregs = e(b)*(12*1000)
			putexcel `col'`row' = matrix(pregs), nformat(####.#)
		}
		local row=`row' + 1
	}
	local row = 10
}

// Overall estimates for women ages 15-19 and count of Ns
local column C D E
local row = 6
local rowb = 7
forvalues r = 1/2 {
		forvalues p=1/3{
			local col : word `p' of `column'
			svy, subpop(if exclude == 0): mean p if period == `p'  & racethn == `r' & inrange(age1,1,3)
			matrix pregs`r'`p' = e(b)*(12*1000)
			matrix ccount`r'`p' = e(N)
			putexcel `col'`row' = matrix(pregs`r'`p'), nformat(####.#)
			putexcel `col'`rowb' = matrix(ccount`r'`p'), nformat(number_sep)
	}
	local row = 13
	local rowb = 14
}

// Calculates the change in rates between adjacent periods. Outer loop is by race/ethnic group
// Next is about comparison (early versus later).
// Innermost loop is by age.

local column C D E
local pointA = 0

foreach re in 3 10 {
	local rl = `re' + 3
	foreach c in F G {
		local pointA=`pointA'+1
		local pointB=`pointA'+1
		local pointcolA : word `pointA' of `column'
		local pointcolB : word `pointB' of `column'
		forvalues r = `re' / `rl' {
			putexcel `c'`r'=formula((+`pointcolB'`r')-(`pointcolA'`r')), nformat(###.#)
		}
	}
	local pointA=0
} 

// Calculates change in rates from earliest to latest period
local column C D E
local pointA = 0

foreach re in 3 10 {
	local rl = `re' + 3

	local pointA=`pointA'+1
	local pointB=`pointA'+2
	
	local pointcolA : word `pointA' of `column'
	local pointcolB : word `pointB' of `column'
	
	forvalues r = `re' / `rl' {
			putexcel H`r'=formula((+`pointcolB'`r')-(`pointcolA'`r')), nformat(###.#)
		}	
	local pointA=0
}

restore

exit

// If we want sig levels for changes, can export them below. 
// exiting program to circumvent them bc they take a long time and i think we
// might not want them-- per Kelly's comment (which I agree with) the point of T1 is
// to show that the NSFG parallels the NCHS numbers.

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
forvalues agegroup = 1/3 {
	forvalues re = 1/2 {
		foreach comp in 1v2 2v3 1v3 {
		svy, subpop(if exclude == 0 & age1 == `agegroup' & racethn == `re' ):reg p period`comp'
		
		gen pval`agegroup'`re'`comp' = 0
		replace pval`agegroup'`re'`comp' = e(p) if e(p) != .
		}
	}
}


// reg by racethn, period
forvalues re = 1/2 {
	foreach comp in 1v2 2v3 1v3 {
		svy, subpop(if exclude == 0 & racethn == `re'):reg p period`comp'
		gen pval`re'`comp' = 0
		replace pval`re'`comp' = e(p) if e(p) != .
		}
}

// export to Excel
putexcel I2 = ("03-06 vs 07-10 p-value") J2 = ("07-10 2 vs 11-15 p-value") K2 = ("03-06 1 vs 11-15 p-value")

// Black and Hispanic by age group
local column I J K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 3
	forvalues agegroup = 1/3 {
		local pval : di %5.4f = pval`agegroup'1`comp'
		putexcel `col'`row' = `pval', nformat(#.###)
		local row = `row' + 1
	}
	local i = `i' + 1
}
// overall b/h preg rates ages 15-29
local column I J K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 6
	local pval : di %5.4f = pval1`comp'
	putexcel `col'`row' = `pval', nformat(#.###)
	
	local i = `i' + 1
}



// White by age group
local column I J K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 10
	forvalues agegroup = 1/3 {
		local pval : di %5.4f = pval`agegroup'2`comp'
		putexcel `col'`row' = `pval', nformat(#.###)
		local row = `row' + 1
	}
	local i = `i' + 1
}

// overall w preg rates ages 15-29
local column I J K
local i = 1
foreach comp in 1v2 2v3 1v3 {
	local col : word `i' of `column'
	local row = 13
	local pval : di %5.4f = pval2`comp'
	putexcel `col'`row' = `pval', nformat(#.###)
	
	local i = `i' + 1
}
