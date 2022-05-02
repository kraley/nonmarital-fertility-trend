** Table 3 
// Decomposition; contraceptive use among sexually active Black women in their 20s
 
// This is a decomposition analysis identifying how much of the trend in non-marital 
// fertile pregnancy is due to changes in rates w/in method use categories and how much 
// is due to changes in distribution of method use.

***************************
** Generate table shells **
***************************

** Twenties 
putexcel set "$results/Table3_twenties_decomposition_cpuse_black.xlsx", replace
// Create Shell
putexcel A1 = "Table 3. Decomposition of trends in non-marital fertile pregnancy rate by contraceptive method use among unmarried, sexually active Black women ages 20-29, 2004-2014"
putexcel C2 = ("Distribution") G2=("Pregnancy Rates")
putexcel C3 = ("2004-06") D3=("2008-10") E3=("Change") G3=("2004-06") H3=("2008-10") I3=("# change") J3=("% change")

putexcel A4= ("Comparing 2004-06 and 2008-10") B4 = ("Permanent")
putexcel B5= "Long-acting reversible method"
putexcel B6 = "Hormonal"
putexcel B7 = "Less-effective methods"
putexcel B8 = "No method"
putexcel B9 = "Total"
putexcel C10 = ("Distribution") E10 = ("Rates") G10 = ("Interaction")
putexcel B11 = "% change due to"

putexcel C12=("2008-10") D12=("2012-14") E12=("Change") G12=("2008-10") H12=("2012-14") I12=("# change") J12=("% change")
putexcel A13= ("Comparing 2008-10 and 2012-14") B13 = ("Permanent")
putexcel B14= "Long-acting reversible method"
putexcel B15 = "Hormonal"
putexcel B16 = "Less-effective methods"
putexcel B17 = "No method"
putexcel B18 = "Total"
putexcel C19 = ("Distribution") E19 = ("Rates") G19 = ("Interaction")
putexcel B20 = "% change due to"


// Export sample sizes
// Create Shell
putexcel A23 = "Sample sizes for decomposition"
putexcel B24 = "Early Period"
putexcel B25=("2004-06") C25=("2008-10") 

putexcel A26 = ("Permanent")
putexcel A27= "LARC"
putexcel A28 = "Hormonal"
putexcel A29 = "LEM"
putexcel A30 = "None"
putexcel A31 = "Total"

putexcel B33 = "Later Period"
putexcel B34=("2008-10") C34=("2012-14") 

putexcel A35 = ("Permanent")
putexcel A36= "LARC"
putexcel A37 = "Hormonal"
putexcel A38 = "LEM"
putexcel A39 = "None"
putexcel A40 = "Total"

 
******************************** 
** Decomposition Calculations **
******************************** 

** Periods 1 v 2 **

local agegroup teens twens

// Loops from wide to narrow: age group (teens, twens);  period (first, second); 
// First estimate the distribution by relationship status, next pregnancy rates by relationship status
// Calculate pregnancy rates under different scenarios (e.g. first period distribution, second period rates). 
// Actual pregnancy rates should be on the diagonals 


// Loop over age groups: 1) teens or 2) twenties
forvalues a = 2/2 {

	// Store name of agegroup in `ageg' (teens or twens)
	local ageg : word `a' of `agegroup'
	
	use "$NSFGKeep/cohabfertlong_appended.dta", clear
		
	gen agegroup = 1 if age1==1
	replace agegroup = 2 if inlist(age1, 2, 3)

	keep if agegroup==`a'
		
	* Identify sample for this loop
	// Exclude those issing nmarsta (not non-marital person-months)
	replace exclude =  1 if missing(nmarsta)
	// Drop all excluded person-months
	
	keep if exclude == 0
	
	// Only analyze black women
	keep if HISPRACE == 3	
	// Only analyze sexually active and cohabiting person-months
	keep if (nmarsta == 1 | nmarsta == 2) & sexmonth == 1
		
	keep if period == 1 | period == 2
		
	// Svyset data
	svyset SECU [pweight = xrndweight], strata(SEST)
		  
	* Distribution of person-months by contraceptive method use
	forvalues period=1/2 {
		// Tab cpmethod cat 5 in order to test the form of the output
		quietly svy: tab cpmethodcat5 if period == `period'
			// If the length of the vector that results is 4 (instead of 5, for 5 categories of contraception)....
			if `e(r)' == 4 {
				// ...check to make sure that the category that's missing observations is permanent contraception (likely few teens using permanent cp)
				count if cpmethodcat5 == 1 & period == `period'
				// If that's what's missing, make row 1 of the vector = 0, and append proportions in each
				// other contraception category below
				if `r(N)' == 0 {
					matrix distP`period' = [0\e(Prop)]
				}
			}
			// If the vector is length 5 when tabbing cpmethodcat5, just store that vector of proportions as matrix.
			else {
				svy, subpop(if exclude == 0): tab cpmethodcat5 if period == `period'
				// Store distribution in a matrix called distP`re'`period',
				// matrix would be called distP62
				matrix distP`period' = e(Prop)
				}
	}

	display "horitontally append matrices distP1, distP2"
	matrix dist`ageg' = distP1, distP2
		
	// drop unappended matricies after we've appended them
	matrix drop distP1 distP2
	
	// Label the row/column names in the matrix, which at this point is
	// the distribution of contraceptive method use in each period
	matrix rownames dist`ageg' = Permanent LARC Hormonal LEM None
	matrix colnames dist`ageg' = 04-06 08-10

	display "Distribution of Black person-months by contraceptive use (`ageg' agegroup)"
	matrix list dist`ageg'
	
	* Pregnancy rates bycontraceptive method use
	// Loop over periods
	forvalues period= 1/2 {
		// Loop over each contraceptive method 
		forvalues method = 1/5 {
			quietly count if cpmethodcat5 == `method' & period == `period'
			if r(N) == 0 {
					matrix pregs`period'`method' = 0
					matrix ccount`a'`period'`method' = 0
				}
			else {
				// Estimate the mean person-months pregnancies began if using a particular contraceptive method
				// in a given period period
				quietly svy, subpop(if exclude == 0): mean p if cpmethodcat5 == `method' & period == `period'
				// Multiply the person-month mean by 12*1000 to get an annual rate, and store in 
				// a (one cell) matrix.
				// For example, matrix would be pregs612 for black women in 0306 the "early"
				// comparison who are sexually active single
				matrix pregs`period'`method' = e(b)*(12*1000)
				matrix ccount`a'`period'`method' = e(N)
			}
		}
	}
		
	// Horzontally append matricies within relationship statuses across the two periods
	// of comparison-- i.e. append black women using LARC's pregnancy rates in
	// periods 0306 and 0710
	forvalues method = 1/5 {
		matrix pregs`method' = pregs1`method', pregs2`method'
		// drop single cell matricies
		matrix drop pregs1`method' pregs2`method'
	}

	// Vertically appending rows of pregnancy rate matricies for each status
	// The number at the end of each matrix name that is not a local indicates
	// which relationship status.
	matrix pregs`ageg' = pregs1
	matrix pregs`ageg' = (pregs`ageg'\pregs2)
	matrix pregs`ageg' = (pregs`ageg'\pregs3)
	matrix pregs`ageg' = (pregs`ageg'\pregs4)
	matrix pregs`ageg' = (pregs`ageg'\pregs5)

	// Drop matricies for individual rows
	matrix drop pregs1 pregs2 pregs3 pregs4 pregs5

	// Label row/column names in the matrix, which at this point is the pregnancy
	// rate by relationship status among women in the two periods which we are 
	// comparing among black and Hispanic women
	matrix rownames pregs`ageg' = Permanent LARC Hormonal LEM None
	matrix colnames pregs`ageg' = 04-06 08-10

	display "Pregnancy rates of Black women by contraceptive use (`ageg' agegroup)"
	matrix list pregs`ageg'

*********************************************************************
* multiply each rate by each population composition for each period *
*********************************************************************

	* d indexes distribution (i.e. distribution for period p)
	* r indexes rates
	* p indexes period 
	* s indexes cp method use status	
	forvalues ds=1/5{
		forvalues dp=1/2{
			forvalues rs=1/5{
				forvalues rp=1/2{
					gen product`ageg'`ds'`dp'`rs'`rp'=dist`ageg'[`ds',`dp'] * pregs`ageg'[`rs',`rp']
				}
			}
		}	
	}

**********************************************************************
* Sum across producs to get non-marital fertility rate under each
* population composition/within-composition rate scenario 
********************************************************************** 
	* dist = distribution in periods 1 and 2
	* r = rates in periods 1 and 2
	* s = nonmarital relationship statuses
	// nmfratecDistR is the total nonmarital fertility rate estimated using distribution in period d and rates in period r
	// summed across all statuses
	// In this loop, we're taking the products generated above and summing them within a given period/rate combination
	// to generate different scenarios of total fertility rates, which will be used in the decomposition, as the decomposition
	// requires you to consider what would be the fertility rate if the distribution between period was unchanged? And if the
	// pregnancy rates by relationsihp status were unchanged?
	// Distribution in periods 1/2 
	forvalues dist = 1/2 {
		// Rates in period 1/2
		forvalues r = 1/2 {
			// Initiate nmfrate to 0, so that we can add products to it.
			gen nmfrate`ageg'`dist'`r' = 0 
			// Non-marital relationship statues -- sum across all statuses to get total fertility under this
			// combination of dist * rate
			forvalues s = 1/5 {
				replace nmfrate`ageg'`dist'`r' = nmfrate`ageg'`dist'`r' + product`ageg'`s'`dist'`s'`r'
			}
			// Save the sum in a local that indicates age group, race, which distribution, and which rate
			// E.e. nmfrate1612 = nmfert rate among black women using 1st period distribution and 2nd period rates in the early comparison period
			local nmfrate`ageg'`dist'`r': di %5.2f = nmfrate`ageg'`dist'`r'
		}
	}
	
	// Generate a matrix that has the nonmarital fertility rates that we observe on the diag [1,1] and [2,2]
	// and counterfacutals if rates or distribution had been unchangnged on the off diag
	matrix scenarios1 = (`nmfrate`ageg'11', `nmfrate`ageg'12'\ `nmfrate`ageg'21', `nmfrate`ageg'22')
		
	// Label row/column names and save matrix
	// Columns are differences in rates, rows are differenecs in distribution
	matrix rownames scenarios1 = dist_04-06 dist_08-10
	matrix colnames scenarios1 = rates_04-06 rates_08-10

	display "Non-Marital Pregnancy rates for Black women under different scenarios"
	matrix list scenarios1`re'

*********************************************************************
* Decomposition
*********************************************************************

	// Calculate difference in observed nonmarital fertility between periods; 
	// the difference on the diag.
	local adiff`ageg'2: di %5.2f = scenarios1[2,2] - scenarios1[1,1] 
	display "Between period 04-06 and 08-10 the non-marital fertility rates increased by `adiff`ageg'2' births per 1,000 women Black women (a negative value indicates a decline in the non-marital fertility rate)."
   
	// Calculate difference if distribution was unchanged -- call it rdiff  for rates differing
	// Rate differences are across columns, so compare cells [1,1 ] and [1,2]
	local rdiff`ageg'2: di %5.2f = scenarios1[1,2] - scenarios1[1,1]
	display "The change in non-marital fertility rates between period 04-06 and 08-10  if distributions had remained unchanged would be `rdiff`ageg'2'."

	// Calculate difference if rates were unchanged unchanged -- call it ddiff, for distributions differing
	// Distribution differences are across rows, so compare fertility rates in cells [2,1] and [1,1]
	local ddiff`ageg'2: di %5.2f = scenarios1[2,1] - scenarios1`re'[1,1]
	display "The change in non-marital fertility rates between period 04-06 and 08-10 if rates had remained unchanged would be `ddiff`ageg'`re'2'"

* Proportion of difference due to rates and to distribution   

	// Proportion change in pregnancy rates due to changing rates within groups is the difference in pregnancy rates when rates vary but distribution
	// is held constant over the total change in pregnancy rates -- rdiff over adiff
	local pdiff_rates`ageg'2: di %4.1f = 100*`rdiff`ageg'2'/`adiff`ageg'2'
	// Prop change in pregnacny rates due to changing distributions is difference in pregnancy rates when rates are held constant but distribution
	// varies over total change in pregnancy rates -- ddiff over adiff
	local pdiff_dist`ageg'2 : di %4.1f = 100*`ddiff`ageg'`re'2'/`adiff`ageg'2'
	// Interaction is x when prop rates + prop dist + x = 100 %
	local pdiff_int`ageg'2: di %4.1f = 100-`pdiff_rates`ageg'2' - `pdiff_dist`ageg'2'
  
	display "Rates account for `pdiff_rates`ageg'2' percent of the difference in non-marital fertility rates between period 04-06 and 08-10. Distribution accounts for `pdiff_dist`ageg'2' percent and the interaction accounts for `pdiff_int`ageg'2' percent."
 
	di " "
	di "************************************************************************************************************ "
	di " "
}

*********************************
** Populate top half of tables **
*********************************

* Twens
putexcel set "$results/Table3_twenties_decomposition_cpuse_black.xlsx", modify 
putexcel C4=matrix(disttwens), nformat(percent)
putexcel G4=matrix(pregstwens), nformat("0.0")
putexcel C9=formula(+C4+C5+C6+C7+C8), nformat(percent)
putexcel D9=formula(+D4+D5+D6+D7+D8), nformat(percent)
putexcel G9=`nmfratetwens11', nformat("0.0")
putexcel H9=`nmfratetwens22', nformat("0.0") 

putexcel C11=`pdiff_disttwens2', nformat("0.0")
putexcel E11=`pdiff_ratestwens2', nformat("0.0")
putexcel G11=`pdiff_inttwens2', nformat("0.0")

// export counts
local columns B C
local startrow = 26

forvalues p=1/2 {
	local col : word `p' of `columns'
	local row = `startrow'
	forvalues s=1/5 {
		putexcel `col'`row'=matrix(ccount2`p'`s'), nformat(number_sep)
		display "startrow = `startrow' row = `row'"
		matrix list ccount2`p'`s'
		local row=`row'+1
	}
	local row = `startrow'
}

// fill in totals
foreach col in B C {
	putexcel `col'31=formula(+`col'26 + `col'27 + `col'28 + `col'29 + `col'30), nformat(number_sep)
}


**************************
** DECOMP: Period 2 v 3 **
**************************

local agegroup teens twens

// Loops from wide to narrow: age group (teens, twens);  period (second, third); 
// First estimate the distribution by relationship status, next pregnancy rates by relationship status
// Calculate pregnancy rates under different scenarios (e.g. first period distribution, second period rates). 
// Actual pregnancy rates should be on the diagonals 


// Loop over age groups: 1) teens or 2) twenties
forvalues a = 2/2 {

	// Store name of agegroup in `ageg' (teens or twens)
	local ageg : word `a' of `agegroup'
	
	use "$NSFGKeep/cohabfertlong_appended.dta", clear
		
	gen agegroup = 1 if age1==1
	replace agegroup = 2 if inlist(age1, 2, 3)

	keep if agegroup==`a'
		
	* Identify sample for this loop
	// Exclude those issing nmarsta (not non-marital person-months)
	replace exclude =  1 if missing(nmarsta)
	// Drop all excluded person-months
	
	keep if exclude == 0
	
	// Only analyze Black women
	keep if HISPRACE ==3	
	// Only analyze sexually active and cohabiting person-months
	keep if (nmarsta == 1 | nmarsta == 2) & sexmonth == 1
	
	// Keep later periods, 2 and 3	
	keep if period == 2 | period == 3
	// adjust so that they're now periods 1 and 2
	// so that the script will compare them properly
	replace period = 1 if period == 2
	replace period = 2 if period == 3
		
	// Svyset data
	svyset SECU [pweight = xrndweight], strata(SEST)
		  
	* Distribution of person-months by contraceptive method use
	forvalues period=1/2 {
		svy, subpop(if exclude == 0): tab cpmethodcat5 if period == `period'
		// Store distribution in a matrix called distP`re'`period',
		// matrix would be called distP62
		matrix distP`period' = e(Prop)
	}

	display "horitontally append matrices distP1, distP2"
	matrix dist`ageg' = distP1, distP2
		
	// drop unappended matricies after we've appended them
	matrix drop distP1 distP2
	
	// Label the row/column names in the matrix, which at this point is
	// the distribution of contraceptive method use in each period
	matrix rownames dist`ageg' = Permanent LARC Hormonal LEM None
	matrix colnames dist`ageg' = 08-10 12-14

	display "Distribution of Black person-months by contraceptive use (`ageg' agegroup)"
	matrix list dist`ageg'
	
	* Pregnancy rates bycontraceptive method use
	// Loop over periods
	forvalues period= 1/2 {
		// Loop over each contraceptive method 
		forvalues status = 1/5 {
			// Estimate the mean person-months pregnancies began if using a particular contraceptive method
			// in a given period period
			quietly svy, subpop(if exclude == 0): mean p if cpmethodcat5 == `status' & period == `period'
			// Multiply the person-month mean by 12*1000 to get an annual rate, and store in 
			// a (one cell) matrix.
			// For example, matrix would be pregs612 for black women in 0306 the "early"
			// comparison who are sexually active single
			matrix pregs`period'`status' = e(b)*(12*1000)
			matrix ccount`a'`period'`status' = e(N)
		}
	}
		
	// Horzontally append matricies within relationship statuses across the two periods
	// of comparison-- i.e. append black women using LARC's pregnancy rates in
	// periods 0306 and 0710
	forvalues status = 1/5 {
		matrix pregs`status' = pregs1`status', pregs2`status'
		// drop single cell matricies
		matrix drop pregs1`status' pregs2`status'
	}

	// Vertically appending rows of pregnancy rate matricies for each status
	// The number at the end of each matrix name that is not a local indicates
	// which relationship status.
	matrix pregs`ageg' = pregs1
	matrix pregs`ageg' = (pregs`ageg'\pregs2)
	matrix pregs`ageg' = (pregs`ageg'\pregs3)
	matrix pregs`ageg' = (pregs`ageg'\pregs4)
	matrix pregs`ageg' = (pregs`ageg'\pregs5)

	// Drop matricies for individual rows
	matrix drop pregs1 pregs2 pregs3 pregs4 pregs5

	// Label row/column names in the matrix, which at this point is the pregnancy
	// rate by relationship status among women in the two periods which we are 
	// comparing among black and Hispanic women
	matrix rownames pregs`ageg' = Permanent LARC Hormonal LEM None
	matrix colnames pregs`ageg' = 08-10 12-14

	display "Pregnancy rates of Black women by contraceptive use (`ageg' agegroup)"
	matrix list pregs`ageg'

*********************************************************************
* multiply each rate by each population composition for each period *
*********************************************************************

	* d indexes distribution (i.e. distribution for period p)
	* r indexes rates
	* p indexes period 
	* s indexes status. 		
	forvalues ds=1/5{
		forvalues dp=1/2{
			forvalues rs=1/5{
				forvalues rp=1/2{
					gen product`ageg'`ds'`dp'`rs'`rp'=dist`ageg'[`ds',`dp'] * pregs`ageg'[`rs',`rp']
				}
			}
		}	
	}

**********************************************************************
* Sum across producs to get non-marital fertility rate under each
* population composition/within-composition rate scenario 
********************************************************************** 
	* dist = distribution in periods 1 and 2
	* r = rates in periods 1 and 2
	* s = nonmarital relationship statuses
	// nmfratecDistR is the total nonmarital fertility rate estimated using distribution in period d and rates in period r
	// summed across all statuses
	// In this loop, we're taking the products generated above and summing them within a given period/rate combination
	// to generate different scenarios of total fertility rates, which will be used in the decomposition, as the decomposition
	// requires you to consider what would be the fertility rate if the distribution between period was unchanged? And if the
	// pregnancy rates by relationsihp status were unchanged?
	// Distribution in periods 1/2 
	forvalues dist = 1/2 {
		// Rates in period 1/2
		forvalues r = 1/2 {
			// Initiate nmfrate to 0, so that we can add products to it.
			gen nmfrate`ageg'`dist'`r' = 0 
			// Non-marital relationship statues -- sum across all statuses to get total fertility under this
			// combination of dist * rate
			forvalues s = 1/5 {
				replace nmfrate`ageg'`dist'`r' = nmfrate`ageg'`dist'`r' + product`ageg'`s'`dist'`s'`r'
			}
			// Save the sum in a local that indicates age group, race, which distribution, and which rate
			// E.e. nmfrate1612 = nmfert rate among black women using 1st period distribution and 2nd period rates in the early comparison period
			local nmfrate`ageg'`dist'`r': di %5.2f = nmfrate`ageg'`dist'`r'
		}
	}
	
	// Generate a matrix that has the nonmarital fertility rates that we observe on the diag [1,1] and [2,2]
	// and counterfacutals if rates or distribution had been unchangnged on the off diag
	matrix scenarios1 = (`nmfrate`ageg'11', `nmfrate`ageg'12'\ `nmfrate`ageg'21', `nmfrate`ageg'22')
		
	// Label row/column names and save matrix
	// Columns are differences in rates, rows are differenecs in distribution
	matrix rownames scenarios1 = dist_08-10 dist_12-14
	matrix colnames scenarios1 = rates_08-10 rates_12-14

	display "Non-Marital Pregnancy rates for Black women under different scenarios"
	matrix list scenarios1`re'

*********************************************************************
* Decomposition
*********************************************************************

	// Calculate difference in observed nonmarital fertility between periods; 
	// the difference on the diag.
	local adiff`ageg'2: di %5.2f = scenarios1[2,2] - scenarios1[1,1] 
	display "Between period 08-10 and 12-14 the non-marital fertility rates increased by `adiff`ageg'2' births per 1,000 women Black women (a negative value indicates a decline in the non-marital fertility rate)."
   
	// Calculate difference if distribution was unchanged -- call it rdiff  for rates differing
	// Rate differences are across columns, so compare cells [1,1 ] and [1,2]
	local rdiff`ageg'2: di %5.2f = scenarios1[1,2] - scenarios1[1,1]
	display "The change in non-marital fertility rates between period 08-10 and 12-14 if distributions had remained unchanged would be `rdiff`ageg'2'."

	// Calculate difference if rates were unchanged unchanged -- call it ddiff, for distributions differing
	// Distribution differences are across rows, so compare fertility rates in cells [2,1] and [1,1]
	local ddiff`ageg'2: di %5.2f = scenarios1[2,1] - scenarios1`re'[1,1]
	display "The change in non-marital fertility rates between period 08-10 and 12-14 if rates had remained unchanged would be `ddiff`ageg'`re'2'"

* Proportion of difference due to rates and to distribution   

	// Proportion change in pregnancy rates due to changing rates within groups is the difference in pregnancy rates when rates vary but distribution
	// is held constant over the total change in pregnancy rates -- rdiff over adiff
	local pdiff_rates`ageg'2: di %4.1f = 100*`rdiff`ageg'2'/`adiff`ageg'2'
	// Prop change in pregnacny rates due to changing distributions is difference in pregnancy rates when rates are held constant but distribution
	// varies over total change in pregnancy rates -- ddiff over adiff
	local pdiff_dist`ageg'2 : di %4.1f = 100*`ddiff`ageg'`re'2'/`adiff`ageg'2'
	// Interaction is x when prop rates + prop dist + x = 100 %
	local pdiff_int`ageg'2: di %4.1f = 100-`pdiff_rates`ageg'2' - `pdiff_dist`ageg'2'
  
	display "Rates account for `pdiff_rates`ageg'2' percent of the difference in non-marital fertility rates between period 08-10 and 12-14. Distribution accounts for `pdiff_dist`ageg'2' percent and the interaction accounts for `pdiff_int`ageg'2' percent."
 
	di " "
	di "************************************************************************************************************ "
	di " "
}



** Populate bottom half of table **

* Twens
putexcel set "$results/Table3_twenties_decomposition_cpuse_black.xlsx", modify 

putexcel C13=matrix(disttwens), nformat(percent)
putexcel G13=matrix(pregstwens), nformat("0.0")
putexcel C18=formula(+C13+C14+C15+C16+C17), nformat(percent)
putexcel D18=formula(+D13+D14+D15+D16+D17), nformat(percent)
putexcel G18=`nmfratetwens11', nformat("0.0")
putexcel H18=`nmfratetwens22', nformat("0.0")

putexcel C20=`pdiff_disttwens2', nformat("0.0")
putexcel E20=`pdiff_ratestwens2', nformat("0.0")
putexcel G20=`pdiff_inttwens2', nformat("0.0")


// export counts
local columns B C
local startrow = 35

forvalues p=1/2 {
	local col : word `p' of `columns'
	local row = `startrow'
	forvalues s=1/5 {
		putexcel `col'`row'=matrix(ccount2`p'`s'), nformat(number_sep)
		display "startrow = `startrow' row = `row'"
		matrix list ccount2`p'`s'
		local row=`row'+1
	}
	local row = `startrow'
}

// fill in totals
foreach col in B C {
	putexcel `col'40=formula(+`col'35 + `col'36 + `col'37 + `col'38 + `col'39), nformat(number_sep)
}


// calculate change in # and % across periods
// distribution
foreach re in 4 13 {
	local rl = `re' + 5
		forvalues r = `re' / `rl' {
			putexcel E`r'=formula((+D`r')-(C`r')), nformat(percent)
		}
}


// rates
foreach re in 4 13 {
	local rl = `re' + 5
		forvalues r = `re' / `rl' {
			putexcel I`r'=formula((+H`r')-(G`r')), nformat(###.#)
			putexcel J`r'=formula((+I`r')/(G`r')), nformat(percent)
		}
}
