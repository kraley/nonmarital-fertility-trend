** Tables 2 and 3
// Parallel tables which include the distribution  of person-months by relationship status
// and contraceptive use. Table 2 for teens, table 3 for women in their twenties. Limited to
// Black and Hispanic women.

use "$NSFGKeep/cohabfertlong_appended.dta", clear
// Svyset data
svyset SECU [pweight = xrndweight], strata(SEST)
		
// Can drop all exclude obs because we're not calculating SEs
keep if exclude == 0

// Only include Black and Hispanic women
keep if HISPRACE==1 | HISPRACE==3

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
	matrix colnames distrel`age' = 03-06 07-10 11-15
	
}


* Distribution of person-months by contraceptive use among sexually-active person-months
preserve 

keep if nmarsta == 1 | nmarsta == 2

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
	matrix colnames distcp`age' = 03-06 07-10 11-15
	
}


restore

mat list distcp1

** Export to excel

** Teens **
putexcel set "$results/Table2_teendist_relandcp_blackandhispanic.xlsx", replace


// Create Shell
putexcel A1 = "Table 2. Distribution of person-months by relationship status and contraceptive use among unmarried Black and Hispanic women ages 15-19, 2003-2015"

putexcel B2=("2003-06") C2=("2007-10") D2=("2011-15") E2=("Change 03-06 to 07-10") F2=("Change 07-10 to 11-15") G2=("Change 03-06 to 11-15")

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
putexcel B4 = matrix(distrel1),  nformat(percent)
// sum columns
foreach col in B C D {
	putexcel `col'7 = formula(+`col'4+`col'5+`col'6), nformat(percent)
}

// contraceptive use
putexcel B9 = matrix(distcp1), nformat(percent)
// sum columns 
foreach col in B C D {
	putexcel `col'14 = formula(+`col'9+`col'10+`col'11+`col'12+`col'13), 	nformat(percent)
}

// Calculates the change in distribution between periods. 
foreach row in  4 5 6 9 10 11 12 13 {
	putexcel E`row' = formula((+C`row')-(B`row')), nformat(percent)
	putexcel F`row' = formula((+D`row')-(C`row')), nformat(percent)
	putexcel G`row' = formula((+D`row')-(B`row')), nformat(percent)
}


** Twenties **
putexcel set "$results/Table3_twentiesdist_relandcp_blackandhispanic.xlsx", replace


// Create Shell
putexcel A1 = "Table 3. Distribution of person-months by relationship status and contraceptive use among unmarried Black and Hispanic women ages 20-29, 2003-2015"

putexcel B2=("2003-06") C2=("2007-10") D2=("2011-15") E2=("Change 03-06 to 07-10") F2=("Change 07-10 to 11-15") G2=("Change 03-06 to 11-15")

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
	putexcel F`row' = formula((+D`row')-(C`row')), nformat(percent)
	putexcel G`row' = formula((+D`row')-(B`row')), nformat(percent)
}
