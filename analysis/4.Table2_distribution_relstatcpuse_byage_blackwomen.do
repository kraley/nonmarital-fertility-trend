** Table 2 
// Distribution of relationship status and contraceptive use among Black women in their 20s.

use "$NSFGKeep/cohabfertlong_appended.dta", clear
// Svyset data
svyset SECU [pweight = xrndweight], strata(SEST)
		
// Can drop all exclude obs because we're not calculating SEs
keep if exclude == 0

// Only include Black
keep if HISPRACE == 3

gen agegroup=1 if age1==1
replace agegroup=2 if inlist(age1, 2, 3)	
	
		  
* Distribution of person-months by relationship status
forvalues age = 1/2 {

	// Relationship status
	forvalues period = 1/3 {
		svy, subpop(if exclude == 0 & period == `period' & agegroup == `age'): tab nmarsta
		// Store distribution in a matrix called distrel`agegroup'`period',
		matrix distrel`age'`period' = e(Prop)
	}


	matrix distrel`age' = distrel`age'1, distrel`age'2, distrel`age'3
	
	// Label the row/column names in the matrix, which at this point is
	// the distribution of relationship statuses in two periods which we are
	// comparing among women of raceeth `re'
	matrix rownames distrel`age' = Cohabiting Single,_SA Single,_NA
	matrix colnames distrel`age' = 04-06 08-10 12-14
	
}


* Distribution of person-months by contraceptive use among sexually-active person-months
preserve 

keep if (nmarsta == 1 | nmarsta == 2) & sexmonth == 1

forvalues age = 1/2 {

	// Contraceptive use status
	forvalues period = 1/3 {
		svy, subpop(if exclude == 0 & period == `period' & agegroup == `age'): tab cpmethodcat5
		// Store distribution in a matrix called distrel`agegroup'`period',
		matrix distcp`age'`period' = e(Prop)
	}


	matrix distcp`age' = distcp`age'1, distcp`age'2, distcp`age'3
	
	// Label the row/column names in the matrix, which at this point is
	// the distribution of relationship statuses in two periods which we are
	// comparing among women of raceeth `re'
	matrix rownames distcp`age' = Permanent LARC Hormonal LEM None
	matrix colnames distcp`age' = 04-06 08-10 12-14
	
}


restore

mat list distcp1

*********************
** Export to excel **
*********************

** Twenties **
putexcel set "$results/Table2_twentiesdist_relandcp_black.xlsx", replace


// Create Shell
putexcel A1 = "Table 2. Distribution of person-months by relationship status and contraceptive use among unmarried Black women ages 20-29, 2004-2014"

putexcel B2=("2004-06") C2=("2008-10") D2=("2012-14") E2=("Change 04-06 to 08-10") G2=("Change 08-10 to 12-14") I2=("Change 04-06 to 12-14")

putexcel A3 = "Relationship status"
putexcel A4 = "Cohabiting"
putexcel A5 = "Single, Sexually Active"
putexcel A6 = "Single, Sexually Inactive"
putexcel A7 = "Total"

putexcel A8  = "Contraceptive use among sexually active"
putexcel A9  = "Permanent"
putexcel A10 = "Long-acting reversible"
putexcel A11 = "Hormonal"
putexcel A12 = "Less-effective methods"
putexcel A13 = "None"
putexcel A14 = "Total"



// Fill Shell with estimates
// relationship status
putexcel B4 = matrix(distrel2),  nformat(percent)
// sum columns
foreach col in B C D {
	putexcel `col'7 = formula(+`col'4+`col'5+`col'6), nformat(percent)
}

// contraceptive use
putexcel B9 = matrix(distcp2), nformat(percent)
// sum columns 
foreach col in B C D {
	putexcel `col'14 = formula(+`col'9+`col'10+`col'11+`col'12+`col'13), 	nformat(percent)
}

// Calculates the change in distribution between periods. 
foreach row in  4 5 6 9 10 11 12 13 {
	putexcel E`row' = formula((+C`row')-(B`row')), nformat(percent)
	putexcel G`row' = formula((+D`row')-(C`row')), nformat(percent)
	putexcel I`row' = formula((+D`row')-(B`row')), nformat(percent)
}

************************
** Significance tests **
************************
// Calculate whether percent changes were statistically significant and export.
// We want to export p-values (or rather, stars) for significant changes in dist 
// of relationship status or contraceptive use between periods.

** Twenties 
putexcel set "$results/Table2_twentiesdist_relandcp_black.xlsx", modify

use "$NSFGKeep/cohabfertlong_appended.dta", clear
// Svyset data
svyset SECU [pweight = xrndweight], strata(SEST)
		
// Set further exclusion criteria for analyses- do this through "exclude" var rather
// than dropping because for running regressions we'll want to retain the whole sample
// for SEs
// Keep only Black
replace exclude = 1 if HISPRACE == 1 | HISPRACE == 2 | HISPRACE == 4

// Keep only women in their 20s 
gen agegroup=1 if age1==1
replace agegroup=2 if inlist(age1, 2, 3)	
replace exclude = 1 if agegroup != 2


* Relationship status
// Gen dummy variables for relationship status
tabulate nmarsta, generate(nmarsta_dummy)

// Run regression models
// Comparing periods 2 and 3 with period 1
forvalues r = 1/3 {
	// run logit model predicting relstat by period, with period 2 as reference.
	svy, subpop(if exclude == 0):logit nmarsta_dummy`r' i.b2.period
	// store the results
	foreach p in 1 3 {
		matrix results = r(table)
		scalar period`p'_nmarsta`r'p = results[4, `p']
	}
}
// comparing periods 1 w/ 3 (possible overkill)
forvalues r = 1/3 {
	svy, subpop(if exclude == 0): logit nmarsta_dummy`r' i.b1.period
	matrix results = r(table)
	scalar period1v3_nmarsta`r'p = results[4, 3]
}

// Export results
local cols F H J
local periods 1 3 1v3
local rows 4 5 6 
forvalues p = 1/3 {						// p indexes period
	local period: word `p' of `periods'
	local col: word `p' of `cols'


	forvalues r = 1/3 {						// r indexes relstat
		local row: word `r' of `rows'
		if period`period'_nmarsta`r'p < 0.1 {
			putexcel `col'`row' = "*"
		}
		if period`period'_nmarsta`r'p < 0.05 {
			putexcel `col'`row' = "**"
		}
		if period`period'_nmarsta`r'p < 0.01 {
			putexcel `col'`row' = "***"
		}
	}
}
// note: with 20s, this doesn't export anything because nothing is sig at 0.05


// Do the same for contraceptive use
tabulate cpmethodcat5, generate(cp_dummy)

// For calculations of cp use, also need to exclude those who aren't sexually active
replace exclude = 1 if nmarsta == 3
replace exclude = 1 if sexmonth != 1

forvalues c = 1/5 {
	di "cpmethod `c'"
	// run logit model predicting relstat by period, with period 2 as reference.
	svy, subpop(if exclude == 0):logit cp_dummy`c' i.b2.period
	// store the results
	foreach p in 1 3 {
		matrix results = r(table)
		scalar period`p'_cp`c'p = results[4, `p']
	}
}
// comparing periods 1 w/ 3 (possible overkill)
forvalues c = 1/5 {
	svy, subpop(if exclude == 0): logit cp_dummy`c' i.b1.period
	matrix results = r(table)
	scalar period1v3_cp`c'p = results[4, 3]
}

// Export results
local cols F H J
local periods 1 3 1v3
local rows 9 10 11 12 13 
forvalues p = 1/3 {						// p indexes period
	local period: word `p' of `periods'
	local col: word `p' of `cols'


	forvalues c = 1/5 {						// c indexes contraceptive method
		local row: word `c' of `rows'
		if period`period'_cp`c'p < 0.1 {
			putexcel `col'`row' = "*"
		}
		if period`period'_cp`c'p < 0.05 {
			putexcel `col'`row' = "**"
		}
		if period`period'_cp`c'p < 0.01 {
			putexcel `col'`row' = "***"
		}
	}
}

putexcel A15 = "*p<0.1, **p<0.05, ***p<0.01"
