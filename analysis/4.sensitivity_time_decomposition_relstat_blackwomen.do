// This is a decomposition analysis identifying how much of the trend in non-marital 
// pregnancy is due to changes in rates and how much to changes in composition

// Decomposition of pregnancy rates by relationship status
// Limited to one year reporting window.


***************************
** Generate table shells **
***************************

** Twenties 
putexcel set "$results/sensitivity_time_twenties_decomposition_relstat_black.xlsx", replace
// Create Shell
putexcel A1 = "Appendix Table 1. Decomposition of trends in non-marital fertile pregnancy rate by relationship status among unmarried Black women ages 20-29, 2004-2014"
putexcel C2= ("Distribution") G2=("Pregnancy Rates")
putexcel C3=("2004-06") D3=("2008-10") E3=("Change") G3=("2004-06") H3=("2008-10") I3=("# change") J3=("% change")

putexcel A4= ("Comparing 2004-06 and 2008-10") B4 = ("Cohabiting")
putexcel B5= "Single, Sexually Active"
putexcel B6 = "Single, Sexually Inactive"
putexcel B7 = "Total"
putexcel C8 = ("Distribution") E8 = ("Rates") G8 = ("Interaction")
putexcel B9 = "% change due to"

putexcel C10=("2008-10") D10=("2012-14") E10=("Change") G10=("2008-10") H10=("2012-14") I10=("# change") J10=("% change")
putexcel A11= ("Comparing 2007-10 and 2011-15") B11 = ("Cohabiting")
putexcel B12= "Single, Sexually Active"
putexcel B13 = "Single, Sexually Inactive"
putexcel B14 = "Total"
putexcel C15 = ("Distribution") E15 = ("Rates") G15 = ("Interaction")
putexcel B16 = "% change due to"


**************************************************************************
** Decomposition of pregnancy rates by relationship status, Black women **
**************************************************************************
** Periods 1 v 2

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
		
	gen agegroup=1 if age1==1
	replace agegroup=2 if inlist(age1, 2, 3)

	keep if agegroup==`a'
		
	* Identify sample for this loop
	// Exclude those issing nmarsta (not non-marital person-months)
	replace exclude =  1 if missing(nmarsta)
	// Drop all excluded person-months
	
	keep if exclude == 0
	
	// keep only Black women
	keep if HISPRACE==3	

	// keep only one year period prior to interview
	keep if mobeforeinterview >= 4 & mobeforeinterview <= 15
		
	keep if period==1 | period==2
		
	// Svyset data
	svyset SECU [pweight = xrndweight], strata(SEST)
		  
	* Distribution of person-months by relationship status
	forvalues period=1/2 {
		svy, subpop(if exclude == 0): tab nmarsta if period == `period'
		// Store distribution in a matrix called distP`re'`period',
		// matrix would be called distP62
		matrix distP`period' = e(Prop)
	}

	display "horitontally append matrices distP1, distP2"
	matrix dist`ageg' = distP1, distP2
		
	// drop unappended matricies after we've appended them
	matrix drop distP1 distP2
	
	// Label the row/column names in the matrix, which at this point is
	// the distribution of relationship statuses in two periods which we are
	// comparing among women of raceeth `re'
	matrix rownames dist`ageg' = Cohabiting Single,_SA Single,_NA
	matrix colnames dist`ageg' = 04-06 08-10

	display "Distribution of Black women by relationship status (`ageg' agegroup)"
	matrix list dist`ageg'
	
	* Pregnancy rates by relationship status
	// Loop over periods
	forvalues period= 1/2 {
		// Loop over each relationship status we're considering - cohab, singleSA, singleNA
		forvalues status = 1/3 {
			// Estimate the mean person-months pregnancies began if a particular relationship status
			// and period
			quietly svy, subpop(if exclude == 0): mean p if nmarsta == `status' & period == `period'
			// Multiply the person-month mean by 12*1000 to get an annual rate, and store in 
			// a (one cell) matrix.
			// For example, matrix would be pregs612 for black women in 0306 the "early"
			// comparison who are sexually active single
			matrix pregs`period'`status' = e(b)*(12*1000)
			matrix ccount`a'`period'`status' = e(N)
		}
	}
		
	// Horzontally append matricies within relationship statuses across the two periods
	// of comparison-- i.e. append sexually active black women's pregnancy rates in
	// periods 0306 and 0710 if we're in the early comparison period to create a 
	// two-cell matrix
	forvalues status = 1/3 {
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

	// Drop matricies for individual rows
	matrix drop pregs1 pregs2 pregs3

	// Label row/column names in the matrix, which at this point is the pregnancy
	// rate by relationship status among women in the two periods which we are 
	// comparing among black and Hispanic women
	matrix rownames pregs`ageg' = Cohabiting Single,_SA Single,_NA
	matrix colnames pregs`ageg' = 04-06 08-10

	display "Pregnancy rates of Black women by relationship status (`ageg' agegroup)"
	matrix list pregs`ageg'

*********************************************************************
* multiply each rate by each population composition for each period *
*********************************************************************

	* d indexes distribution (i.e. distribution for period p)
	* r indexes rates
	* p indexes period 
	* s indexes status. 
	// So, for example dp 1,1 is proportion in status 1 in period 1, and
	// rp 1,1 if pregnancy rate in status 1 in period 1
	// We are looping over 2 periods and 3 statuses -- ds ps are looping 1/3, and
	// dp rp are looping over 1/2
	// We are generating product of rate * distribution, which we can sum to get
	// overall pregnancy rates
	// E.g product141212 = product of distribution of relationship
	// status in the early comparison period (1) of black women (4) who are cohabiting (1)
	// in 0710 (2) * pregnancy rates of black women who were cohabiting (1) in 0710 (2)		
	forvalues ds=1/3{
		forvalues dp=1/2{
			forvalues rs=1/3{
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
			forvalues s = 1/3 {
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

	display "Non-Marital Pregnancy rates for Black and Hispanic women under different scenarios"
	matrix list scenarios1`re'

*********************************************************************
* Decomposition
*********************************************************************

	// Calculate difference in observed nonmarital fertility between periods; 
	// the difference on the diag.
	local adiff`ageg'2: di %5.2f = scenarios1[2,2] - scenarios1[1,1] 
	display "Between period 04-06 and 08-10 the non-marital fertility rates increased by `adiff`ageg'2' births per 1,000 women Black and Hispanic women (a negative value indicates a decline in the non-marital fertility rate)."
   
	// Calculate difference if distribution was unchanged -- call it rdiff  for rates differing
	// Rate differences are across columns, so compare cells [1,1 ] and [1,2]
	local rdiff`ageg'2: di %5.2f = scenarios1[1,2] - scenarios1[1,1]
	display "The change in non-marital fertility rates between period 04-06 and 08-10 if distributions had remained unchanged would be `rdiff`ageg'2'."

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


macro list


// Fill Shell with estimates for period 1v2


* Twens
putexcel set "$results/sensitivity_time_twenties_decomposition_relstat_black.xlsx", modify
putexcel C4=matrix(disttwens), nformat(percent)
putexcel G4=matrix(pregstwens), nformat("0.0")
putexcel C7=formula(+C4+C5+C6), nformat(percent)
putexcel D7=formula(+D4+D5+D6), nformat(percent)
putexcel G7=`nmfratetwens11', nformat("0.0")
putexcel H7=`nmfratetwens22', nformat("0.0")

putexcel C9 =`pdiff_disttwens2', nformat("0.0")
putexcel E9 =`pdiff_ratestwens2', nformat("0.0")
putexcel G9 =`pdiff_inttwens2', nformat("0.0")


// Calculates the change in rates between periods. 

// distribution
foreach re in 4 11 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel E`r'=formula((+D`r')-(C`r')), nformat(percent)
		}
}


// rates
foreach re in 4 11 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel I`r'=formula((+H`r')-(G`r')), nformat("0.0")
			putexcel J`r'=formula((+I`r')/(G`r')), nformat(percent)
			
		}
}


*******************
** Periods 2 v 3 **
*******************

// Loop over age groups: 1) teens or 2) twenties
forvalues a = 2/2 {

	// Store name of agegroup in `ageg' (teens or twens)
	local ageg : word `a' of `agegroup'
	
	use "$NSFGKeep/cohabfertlong_appended.dta", clear
		
	gen agegroup=1 if age1==1
	replace agegroup=2 if inlist(age1, 2, 3)

	keep if agegroup==`a'
		
	* Identify sample for this loop
	// Exclude those issing nmarsta (not non-marital person-months)
	replace exclude =  1 if missing(nmarsta)
	// Drop all excluded person-months
	
	keep if exclude == 0
	
	// keep only Black women
	keep if HISPRACE==3	

	// keep only one year period prior to interview
	keep if mobeforeinterview >= 4 & mobeforeinterview <= 15
		
	// Keep later periods, 2 and 3	
	keep if period == 2 | period == 3
	// adjust so that they're now periods 1 and 2
	// so that the script will compare them properly
	replace period = 1 if period == 2
	replace period = 2 if period == 3
		
	// Svyset data
	svyset SECU [pweight = xrndweight], strata(SEST)
		  
	* Distribution of person-months by relationship status
	forvalues period=1/2 {
		svy, subpop(if exclude == 0): tab nmarsta if period == `period'
		// Store distribution in a matrix called distP`re'`period',
		// matrix would be called distP62
		matrix distP`period' = e(Prop)
	}

	display "horitontally append matrices distP1, distP2"
	matrix dist`ageg' = distP1, distP2
		
	// drop unappended matricies after we've appended them
	matrix drop distP1 distP2
	
	// Label the row/column names in the matrix, which at this point is
	// the distribution of relationship statuses in two periods which we are
	// comparing among women of raceeth `re'
	matrix rownames dist`ageg' = Cohabiting Single,_SA Single,_NA
	matrix colnames dist`ageg' = 08-10 12-14

	display "Distribution of Black women by relationship status (`ageg' agegroup)"
	matrix list dist`ageg'
	
	* Pregnancy rates by relationship status
	// Loop over periods
	forvalues period= 1/2 {
		// Loop over each relationship status we're considering - cohab, singleSA, singleNA
		forvalues status = 1/3 {
			// Estimate the mean person-months pregnancies began if a particular relationship status
			// and period
			quietly svy, subpop(if exclude == 0): mean p if nmarsta == `status' & period == `period'
			// Multiply the person-month mean by 12*1000 to get an annual rate, and store in 
			// a (one cell) matrix.
			// For example, matrix would be pregs612 for black women in 0306 the "early"
			// comparison who are sexually active single
			matrix pregs`period'`status' = e(b)*(12*1000)
			matrix ccount`a'`period'`status' = e(N)
		}
	}
		
	// Horzontally append matricies within relationship statuses across the two periods
	// of comparison-- i.e. append sexually active black women's pregnancy rates in
	// periods 0306 and 0710 if we're in the early comparison period to create a 
	// two-cell matrix
	forvalues status = 1/3 {
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

	// Drop matricies for individual rows
	matrix drop pregs1 pregs2 pregs3

	// Label row/column names in the matrix, which at this point is the pregnancy
	// rate by relationship status among women in the two periods which we are 
	// comparing among black and Hispanic women
	matrix rownames pregs`ageg' = Cohabiting Single,_SA Single,_NA
	matrix colnames pregs`ageg' = 08-10 12-14

	display "Pregnancy rates of Black women by relationship status (`ageg' agegroup)"
	matrix list pregs`ageg'

*********************************************************************
* multiply each rate by each population composition for each period *
*********************************************************************

	* d indexes distribution (i.e. distribution for period p)
	* r indexes rates
	* p indexes period 
	* s indexes status. 
	// So, for example dp 1,1 is proportion in status 1 in period 1, and
	// rp 1,1 if pregnancy rate in status 1 in period 1
	// We are looping over 2 periods and 3 statuses -- ds ps are looping 1/3, and
	// dp rp are looping over 1/2
	// We are generating product of rate * distribution, which we can sum to get
	// overall pregnancy rates
	// E.g product141212 = product of distribution of relationship
	// status in the early comparison period (1) of black women (4) who are cohabiting (1)
	// in 0710 (2) * pregnancy rates of black women who were cohabiting (1) in 0710 (2)		
	forvalues ds=1/3{
		forvalues dp=1/2{
			forvalues rs=1/3{
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
			forvalues s = 1/3 {
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


macro list


// Fill Shell with estimates

* Twens
putexcel set "$results/sensitivity_time_twenties_decomposition_relstat_black.xlsx", modify

putexcel C11=matrix(disttwens), nformat(percent)
putexcel G11=matrix(pregstwens), nformat("0.0")
putexcel C14=formula(+C11+C12+C13), nformat(percent)
putexcel D14=formula(+D11+D12+D13), nformat(percent)
putexcel G14=`nmfratetwens11', nformat("0.0")
putexcel H14=`nmfratetwens22', nformat("0.0")

putexcel C16=`pdiff_disttwens2', nformat("0.0")
putexcel E16=`pdiff_ratestwens2', nformat("0.0")
putexcel G16=`pdiff_inttwens2', nformat("0.0")


// Calculates the change in rates between periods. 

// distribution
foreach re in 4 11 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel E`r'=formula((+D`r')-(C`r')), nformat(percent)
		}
}


// rates
foreach re in 4 11 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel I`r'=formula((+H`r')-(G`r')), nformat("0.0")
			putexcel J`r'=formula((+I`r')/(G`r')), nformat(percent)
			
		}
}
