// This is a decomposition analysis identifying how much of the trend in non-marital 
// pregnancy is due to changes in rates and how much to changes in composition

// Decomposition of pregnancy rates by relationship status, stratified by race/ethnicity

* By Kelly Raley and Kristen Burke



***********************************************************************************************
** Decomposition of pregnancy rates by relationship status, stratified by race/ethnicity **
***********************************************************************************************

******************
** Period 1 v 2 **
******************

putexcel set "$results/decomposition_relstat_byage_byrace_03060710.xlsx", replace

local periods 03-06 07-10 11-15
local racethn blank blank blank Hispanic NHwhite NHblack
local agegroup teens twens


// Loops from wide to narrow: age group (teens, twens); racethnicity (Hispanic, Black); period (first, second); 
// First estimate the distribution by relationship status, next pregnancy rates by relationship status
// Calculate pregnancy rates under different scenarios (e.g. first period distribution, second period rates). 
// Actual pregnancy rates should be on the diagonals 


// Loop over age groups: 1) teens or 2) twenties
forvalues a = 1/2 {

	// Store name of agegroup in `ageg' (teens or twens)
	local ageg : word `a' of `agegroup'
	
	// Loop over race/ethnicity-- focusing on Hispanic and NHBlack
	foreach re in 4 6 {
		use "$NSFGKeep/cohabfertlong_appended.dta", clear
		
		// I'm bumping valid values of HISPRACE higher so that the matrices have unique ids
		// If I don't then I can have pregs13 mean pregsearlycomparisonblack or pregshispanic11-15
		replace HISPRACE=HISPRACE+3
		
		gen agegroup=1 if age1==1
		replace agegroup=2 if inlist(age1, 2, 3)

		keep if agegroup==`a'
		
		* Identify sample for this loop
		// Exclude those issing nmarsta (not non-marital person-months)
		replace exclude =  1 if missing(nmarsta)
		// Drop all excluded person-months
		keep if exclude == 0
		// Drop all person-months that are not for people of race/ethnicity in this loop
		// 4 and 6 (the values of this loop) correspond to Hispanic and NHBlack in `racethn', set outside of the loop
		// as well as the new values of HISPRACE recoded above.
		keep if HISPRACE==`re'	
		
		keep if period==1 | period==2
		
		// Svyset data
		svyset SECU [pweight = xrndweight], strata(SEST)
		
		// Store race that we're focusing on in this loop in `reth'
		local reth:     word `re' of `racethn'
	   
		* Distribution of person-months by relationship status
		forvalues period=1/2 {
			svy, subpop(if exclude == 0): tab nmarsta if period == `period'
			// Store distribution in a matrix called distP`re'`period',
			// matrix would be called distP62
			matrix distP`re'`period' = e(Prop)
		}

		display "horitontally append matrices distP`re'1, distP`re'2"
		matrix dist`ageg'`re' = distP`re'1, distP`re'2
		
		// drop unappended matricies after we've appended them
		matrix drop distP`re'1 distP`re'2
		
		// Label the row/column names in the matrix, which at this point is
		// the distribution of relationship statuses in two periods which we are
		// comparing among women of raceeth `re'
		matrix rownames dist`ageg'`re' = Cohabiting Single,_SA Single,_NA
		matrix colnames dist`ageg'`re' = 03-06 07-10

		display "Distribution of `reth' women by relationship status (`ageg' agegroup)"
		matrix list dist`ageg'`re'
	
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
				matrix pregs`re'`period'`status' = e(b)*(12*1000)
			}
		}
		
		// Horzontally append matricies within relationship statuses across the two periods
		// of comparison-- i.e. append sexually active black women's pregnancy rates in
		// periods 0306 and 0710 if we're in the early comparison period to create a 
		// two-cell matrix
		forvalues status = 1/3 {
			matrix pregs`re'`status' = pregs`re'1`status', pregs`re'2`status'
			// drop single cell matricies
			matrix drop pregs`re'1`status' pregs`re'2`status'
		}

		// Vertically appending rows of pregnancy rate matricies for each status
		// The number at the end of each matrix name that is not a local indicates
		// which relationship status.
		matrix pregs`ageg'`re' = pregs`re'1
		matrix pregs`ageg'`re' = (pregs`ageg'`re'\pregs`re'2)
		matrix pregs`ageg'`re' = (pregs`ageg'`re'\pregs`re'3)

		// Drop matricies for individual rows
		matrix drop pregs`re'1 pregs`re'2 pregs`re'3

		// Label row/column names in the matrix, which at this point is the pregnancy
		// rate by relationship status among women in the two periods which we are 
		// comparing among women of raceeth `re'
		matrix rownames pregs`ageg'`re' = Cohabiting Single,_SA Single,_NA
		matrix colnames pregs`ageg'`re' = 03-06 07-10

		display "Pregnancy rates of `reth' women by relationship status (`ageg' agegroup)"
		matrix list pregs`ageg'`re'

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
						gen product`ageg'`re'`ds'`dp'`rs'`rp'=dist`ageg'`re'[`ds',`dp'] * pregs`ageg'`re'[`rs',`rp']
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
				gen nmfrate`ageg'`re'`dist'`r' = 0 
				// Non-marital relationship statues -- sum across all statuses to get total fertility under this
				// combination of dist * rate
				forvalues s = 1/3 {
					replace nmfrate`ageg'`re'`dist'`r' = nmfrate`ageg'`re'`dist'`r' + product`ageg'`re'`s'`dist'`s'`r'
				}
				// Save the sum in a local that indicates age group, race, which distribution, and which rate
				// E.e. nmfrate1612 = nmfert rate among black women using 1st period distribution and 2nd period rates in the early comparison period
				local nmfrate`ageg'`re'`dist'`r': di %5.2f = nmfrate`ageg'`re'`dist'`r'
			}
		}
	
		// Generate a matrix that has the nonmarital fertility rates that we observe on the diag [1,1] and [2,2]
		// and counterfacutals if rates or distribution had been unchangnged on the off diag
		matrix scenarios1`re' = (`nmfrate`ageg'`re'11', `nmfrate`ageg'`re'12'\ `nmfrate`ageg'`re'21', `nmfrate`ageg'`re'22')
		
		// Label row/column names and save matrix
		// Columns are differences in rates, rows are differenecs in distribution
		matrix rownames scenarios1`re' = dist_03-06 dist_07-10
		matrix colnames scenarios1`re' = rates_03-06 rates_07-10

		display "Non-Marital Pregnancy rates for `reth' women under different scenarios"
		matrix list scenarios1`re'

*********************************************************************
* Decomposition
*********************************************************************

		// Calculate difference in observed nonmarital fertility between periods; 
		// the difference on the diag.
		local adiff`ageg'`re'2: di %5.2f = scenarios1`re'[2,2] - scenarios1`re'[1,1] 
		display "Between period 03-06 and 07-10 the non-marital fertility rates increased by `adiff`ageg'`re'2' births per 1,000 women `reth' women (a negative value indicates a decline in the non-marital fertility rate)."
   
		// Calculate difference if distribution was unchanged -- call it rdiff  for rates differing
		// Rate differences are across columns, so compare cells [1,1 ] and [1,2]
		local rdiff`ageg'`re'2: di %5.2f = scenarios1`re'[1,2] - scenarios1`re'[1,1]
		display "The change in non-marital fertility rates between period 03-06 and 07-10 if distributions had remained unchanged would be `rdiff`ageg'`re'2'."

		// Calculate difference if rates were unchanged unchanged -- call it ddiff, for distributions differing
		// Distribution differences are across rows, so compare fertility rates in cells [2,1] and [1,1]
		local ddiff`ageg'`re'2: di %5.2f = scenarios1`re'[2,1] - scenarios1`re'[1,1]
		display "The change in non-marital fertility rates between period 03-06 and 07-10 if rates had remained unchanged would be `ddiff`ageg'`re'2'"

* Proportion of difference due to rates and to distribution   

		// Proportion change in pregnancy rates due to changing rates within groups is the difference in pregnancy rates when rates vary but distribution
		// is held constant over the total change in pregnancy rates -- rdiff over adiff
		local pdiff_rates`ageg'`re'2: di %4.1f = 100*`rdiff`ageg'`re'2'/`adiff`ageg'`re'2'
		// Prop change in pregnacny rates due to changing distributions is difference in pregnancy rates when rates are held constant but distribution
		// varies over total change in pregnancy rates -- ddiff over adiff
		local pdiff_dist`ageg'`re'2 : di %4.1f = 100*`ddiff`ageg'`re'2'/`adiff`ageg'`re'2'
		// Interaction is x when prop rates + prop dist + x = 100 %
		local pdiff_int`ageg'`re'2: di %4.1f = 100-`pdiff_rates`ageg'`re'2' - `pdiff_dist`ageg'`re'2'
  
		display "Rates account for `pdiff_rates`ageg'`re'2' percent of the difference in non-marital fertility rates between period 03-06 and 07-10. Distribution accounts for `pdiff_dist`ageg'`re'2' percent and the interaction accounts for `pdiff_int`ageg'`re'2' percent."
 
		di " "
		di "************************************************************************************************************ "
		di " "
	}
}

macro list
// Create Shell
putexcel A1 = "Table 2. Decomposition of Trends in Non-Marital Fertility Rate between 2003-06 and 2007-10"
putexcel C2 = "Teens"
putexcel C3= ("Distribution") G3=("Pregnancy Rates")
putexcel C4=("2003-06") D4=("2007-10") E4=("Change") G4=("2003-06") H4=("2007-10") I4=("#Change") J4=("%Change")

putexcel A5= ("Hispanic") B5 = ("Cohabiting")
putexcel B6= "Single, Sexually Active"
putexcel B7 = "Single, Sexually Inactive"
putexcel B8 = "Total"
putexcel C9 = ("Distribution") E9 = ("Rates") G9 = ("Interaction")
putexcel B10 = "% Change due to"
putexcel A12= ("Black") B12 = ("Cohabiting")
putexcel B13= "Single, Sexually Active"
putexcel B14 = "Single, Sexually Inactive"
putexcel B15 = "Total"
putexcel C16 = ("Distribution") E16 = ("Rates") G16 = ("Interaction")
putexcel B17 = "% Change due to"
putexcel C19 = "Twenties"
putexcel C20= ("Distribution") G20=("Pregnancy Rates")
putexcel C21=("2003-06") D21=("2007-10") E21=("Change") G21=("2003-06") H21=("2007-10") I21=("#Change") J21=("%Change")

putexcel A22= ("Hispanic") B22 = ("Cohabiting")
putexcel B23= "Single, Sexually Active"
putexcel B24 = "Single, Sexually Inactive"
putexcel B25 = "Total"
putexcel C26 = ("Distribution") E26 = ("Rates") G26 = ("Interaction")
putexcel B27 = "% Change due to"
putexcel A29= ("Black") B29 = ("Cohabiting")
putexcel B30= "Single, Sexually Active"
putexcel B31 = "Single, Sexually Inactive"
putexcel B32 = "Total"

putexcel C33 = ("Distribution") E33 = ("Rates") G33 = ("Interaction")
putexcel B34 = "% Change due to"

// Fill Shell with estimates

* Hispanic teens
putexcel C5=matrix(distteens4)
putexcel G5=matrix(pregsteens4)
putexcel C8=formula(+C5+C6+C7)
putexcel D8=formula(+D5+D6+D7)
putexcel G8=`nmfrateteens411'
putexcel H8=`nmfrateteens422' 

putexcel C10=`pdiff_distteens42'
putexcel E10=`pdiff_ratesteens42'
putexcel G10=`pdiff_intteens42'

* Black women teens
putexcel C12=matrix(distteens6)
putexcel G12=matrix(pregsteens6)
putexcel C15=formula(+C12+C13+C14)
putexcel D15=formula(+D12+D13+D14)
putexcel G15=`nmfrateteens611'
putexcel H15=`nmfrateteens622'

putexcel C17=`pdiff_distteens62'
putexcel E17=`pdiff_ratesteens62'
putexcel G17=`pdiff_intteens62'

* Hispanic women in twens
putexcel C22=matrix(disttwens4)
putexcel G22=matrix(pregstwens4)
putexcel C25=formula(+C22+C23+C24)
putexcel D25=formula(+D22+D23+D24)
putexcel G25=`nmfratetwens411'
putexcel H25=`nmfratetwens422'

putexcel C27=`pdiff_disttwens42'
putexcel E27=`pdiff_ratestwens42'
putexcel G27=`pdiff_inttwens42'

* Black women in twens
putexcel C29=matrix(disttwens6)
putexcel G29=matrix(pregstwens6)
putexcel C32=formula(+C29+C30+C31)
putexcel D32=formula(+D29+D30+D31)
putexcel G32=`nmfratetwens611'
putexcel H32=`nmfratetwens622'

putexcel C34=`pdiff_disttwens62'
putexcel E34=`pdiff_ratestwens62'
putexcel G34=`pdiff_inttwens62'

// Calculates the change in rates between periods. Outer loop is by race/ethnic groupd
// Next is about comparison (early versus later).
// Innermost loop is by age.

//Teens

// distribution
foreach re in 5 12 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel E`r'=formula((+D`r')-(C`r'))
		}
}


// rates
foreach re in 5 12 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel I`r'=formula((+H`r')-(G`r'))
		}
}

// Twens

// distribution
foreach re in 22 29 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel E`r'=formula((+D`r')-(C`r'))
		}
}


// rates
foreach re in 22 29 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel I`r'=formula((+H`r')-(G`r'))
		}
}


******************
** Period 2 v 3 **
******************

putexcel set "$results/decomposition_relstat_byage_byrace_07101113.xlsx", replace

local periods 03-06 07-10 11-15
local racethn blank blank blank Hispanic NHwhite NHblack
local agegroup teens twens


// Loops from wide to narrow: age group (teens, twens); racethnicity (Hispanic, Black); period (first, second); 
// First estimate the distribution by relationship status, next pregnancy rates by relationship status
// Calculate pregnancy rates under different scenarios (e.g. first period distribution, second period rates). 
// Actual pregnancy rates should be on the diagonals 


// Loop over age groups: 1) teens or 2) twenties
forvalues a = 1/2 {

	// Store name of agegroup in `ageg' (teens or twens)
	local ageg : word `a' of `agegroup'
	
	// Loop over race/ethnicity-- focusing on Hispanic and NHBlack
	foreach re in 4 6 {
		use "$NSFGKeep/cohabfertlong_appended.dta", clear
		
		// I'm bumping valid values of HISPRACE higher so that the matrices have unique ids
		// If I don't then I can have pregs13 mean pregsearlycomparisonblack or pregshispanic11-15
		replace HISPRACE=HISPRACE+3
		
		gen agegroup=1 if age1==1
		replace agegroup=2 if inlist(age1, 2, 3)

		keep if agegroup==`a'
		
		* Identify sample for this loop
		// Exclude those issing nmarsta (not non-marital person-months)
		replace exclude =  1 if missing(nmarsta)
		// Drop all excluded person-months
		keep if exclude == 0
		// Drop all person-months that are not for people of race/ethnicity in this loop
		// 4 and 6 (the values of this loop) correspond to Hispanic and NHBlack in `racethn', set outside of the loop
		// as well as the new values of HISPRACE recoded above.
		keep if HISPRACE==`re'	
		
		keep if period== 2 | period== 3
		replace period = 1 if period == 2
		replace period = 2 if period == 3
		
		// Svyset data
		svyset SECU [pweight = xrndweight], strata(SEST)
		
		// Store race that we're focusing on in this loop in `reth'
		local reth:     word `re' of `racethn'
	   
		* Distribution of person-months by relationship status
		forvalues period=1/2 {
			svy, subpop(if exclude == 0): tab nmarsta if period == `period'
			// Store distribution in a matrix called distP`re'`period',
			// matrix would be called distP62
			matrix distP`re'`period' = e(Prop)
		}

		display "horitontally append matrices distP`re'1, distP`re'2"
		matrix dist`ageg'`re' = distP`re'1, distP`re'2
		
		// drop unappended matricies after we've appended them
		matrix drop distP`re'1 distP`re'2
		
		// Label the row/column names in the matrix, which at this point is
		// the distribution of relationship statuses in two periods which we are
		// comparing among women of raceeth `re'
		matrix rownames dist`ageg'`re' = Cohabiting Single,_SA Single,_NA
		matrix colnames dist`ageg'`re' = 03-06 07-10

		display "Distribution of `reth' women by relationship status (`ageg' agegroup)"
		matrix list dist`ageg'`re'
	
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
				matrix pregs`re'`period'`status' = e(b)*(12*1000)
			}
		}
		
		// Horzontally append matricies within relationship statuses across the two periods
		// of comparison-- i.e. append sexually active black women's pregnancy rates in
		// periods 0306 and 0710 if we're in the early comparison period to create a 
		// two-cell matrix
		forvalues status = 1/3 {
			matrix pregs`re'`status' = pregs`re'1`status', pregs`re'2`status'
			// drop single cell matricies
			matrix drop pregs`re'1`status' pregs`re'2`status'
		}

		// Vertically appending rows of pregnancy rate matricies for each status
		// The number at the end of each matrix name that is not a local indicates
		// which relationship status.
		matrix pregs`ageg'`re' = pregs`re'1
		matrix pregs`ageg'`re' = (pregs`ageg'`re'\pregs`re'2)
		matrix pregs`ageg'`re' = (pregs`ageg'`re'\pregs`re'3)

		// Drop matricies for individual rows
		matrix drop pregs`re'1 pregs`re'2 pregs`re'3

		// Label row/column names in the matrix, which at this point is the pregnancy
		// rate by relationship status among women in the two periods which we are 
		// comparing among women of raceeth `re'
		matrix rownames pregs`ageg'`re' = Cohabiting Single,_SA Single,_NA
		matrix colnames pregs`ageg'`re' = 03-06 07-10

		display "Pregnancy rates of `reth' women by relationship status (`ageg' agegroup)"
		matrix list pregs`ageg'`re'

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
						gen product`ageg'`re'`ds'`dp'`rs'`rp'=dist`ageg'`re'[`ds',`dp'] * pregs`ageg'`re'[`rs',`rp']
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
				gen nmfrate`ageg'`re'`dist'`r' = 0 
				// Non-marital relationship statues -- sum across all statuses to get total fertility under this
				// combination of dist * rate
				forvalues s = 1/3 {
					replace nmfrate`ageg'`re'`dist'`r' = nmfrate`ageg'`re'`dist'`r' + product`ageg'`re'`s'`dist'`s'`r'
				}
				// Save the sum in a local that indicates age group, race, which distribution, and which rate
				// E.e. nmfrate1612 = nmfert rate among black women using 1st period distribution and 2nd period rates in the early comparison period
				local nmfrate`ageg'`re'`dist'`r': di %5.2f = nmfrate`ageg'`re'`dist'`r'
			}
		}
	
		// Generate a matrix that has the nonmarital fertility rates that we observe on the diag [1,1] and [2,2]
		// and counterfacutals if rates or distribution had been unchangnged on the off diag
		matrix scenarios1`re' = (`nmfrate`ageg'`re'11', `nmfrate`ageg'`re'12'\ `nmfrate`ageg'`re'21', `nmfrate`ageg'`re'22')
		
		// Label row/column names and save matrix
		// Columns are differences in rates, rows are differenecs in distribution
		matrix rownames scenarios1`re' = dist_03-06 dist_07-10
		matrix colnames scenarios1`re' = rates_03-06 rates_07-10

		display "Non-Marital Pregnancy rates for `reth' women under different scenarios"
		matrix list scenarios1`re'

*********************************************************************
* Decomposition
*********************************************************************

		// Calculate difference in observed nonmarital fertility between periods; 
		// the difference on the diag.
		local adiff`ageg'`re'2: di %5.2f = scenarios1`re'[2,2] - scenarios1`re'[1,1] 
		display "Between period 07-10 and 11-13 the non-marital fertility rates increased by `adiff`ageg'`re'2' births per 1,000 women `reth' women (a negative value indicates a decline in the non-marital fertility rate)."
   
		// Calculate difference if distribution was unchanged -- call it rdiff  for rates differing
		// Rate differences are across columns, so compare cells [1,1 ] and [1,2]
		local rdiff`ageg'`re'2: di %5.2f = scenarios1`re'[1,2] - scenarios1`re'[1,1]
		display "The change in non-marital fertility rates between period 03-06 and 07-10 if distributions had remained unchanged would be `rdiff`ageg'`re'2'."

		// Calculate difference if rates were unchanged unchanged -- call it ddiff, for distributions differing
		// Distribution differences are across rows, so compare fertility rates in cells [2,1] and [1,1]
		local ddiff`ageg'`re'2: di %5.2f = scenarios1`re'[2,1] - scenarios1`re'[1,1]
		display "The change in non-marital fertility rates between period 03-06 and 07-10 if rates had remained unchanged would be `ddiff`ageg'`re'2'"

* Proportion of difference due to rates and to distribution   

		// Proportion change in pregnancy rates due to changing rates within groups is the difference in pregnancy rates when rates vary but distribution
		// is held constant over the total change in pregnancy rates -- rdiff over adiff
		local pdiff_rates`ageg'`re'2: di %4.1f = 100*`rdiff`ageg'`re'2'/`adiff`ageg'`re'2'
		// Prop change in pregnacny rates due to changing distributions is difference in pregnancy rates when rates are held constant but distribution
		// varies over total change in pregnancy rates -- ddiff over adiff
		local pdiff_dist`ageg'`re'2 : di %4.1f = 100*`ddiff`ageg'`re'2'/`adiff`ageg'`re'2'
		// Interaction is x when prop rates + prop dist + x = 100 %
		local pdiff_int`ageg'`re'2: di %4.1f = 100-`pdiff_rates`ageg'`re'2' - `pdiff_dist`ageg'`re'2'
  
		display "Rates account for `pdiff_rates`ageg'`re'2' percent of the difference in non-marital fertility rates between period 07-10 and 11-13. Distribution accounts for `pdiff_dist`ageg'`re'2' percent and the interaction accounts for `pdiff_int`ageg'`re'2' percent."
 
		di " "
		di "************************************************************************************************************ "
		di " "
	}
}

macro list
// Create Shell
putexcel A1 = "Table 2. Decomposition of Trends in Non-Marital Fertility Rate between 07-10 and 11-13"
putexcel C2 = "Teens"
putexcel C3= ("Distribution") G3=("Pregnancy Rates")
putexcel C4=("2003-06") D4=("2007-10") E4=("Change") G4=("2003-06") H4=("2007-10") I4=("#Change") J4=("%Change")

putexcel A5= ("Hispanic") B5 = ("Cohabiting")
putexcel B6= "Single, Sexually Active"
putexcel B7 = "Single, Sexually Inactive"
putexcel B8 = "Total"
putexcel C9 = ("Distribution") E9 = ("Rates") G9 = ("Interaction")
putexcel B10 = "% Change due to"
putexcel A12= ("Black") B12 = ("Cohabiting")
putexcel B13= "Single, Sexually Active"
putexcel B14 = "Single, Sexually Inactive"
putexcel B15 = "Total"
putexcel C16 = ("Distribution") E16 = ("Rates") G16 = ("Interaction")
putexcel B17 = "% Change due to"
putexcel C19 = "Twenties"
putexcel C20= ("Distribution") G20=("Pregnancy Rates")
putexcel C21=("2003-06") D21=("2007-10") E21=("Change") G21=("2003-06") H21=("2007-10") I21=("#Change") J21=("%Change")

putexcel A22= ("Hispanic") B22 = ("Cohabiting")
putexcel B23= "Single, Sexually Active"
putexcel B24 = "Single, Sexually Inactive"
putexcel B25 = "Total"
putexcel C26 = ("Distribution") E26 = ("Rates") G26 = ("Interaction")
putexcel B27 = "% Change due to"
putexcel A29= ("Black") B29 = ("Cohabiting")
putexcel B30= "Single, Sexually Active"
putexcel B31 = "Single, Sexually Inactive"
putexcel B32 = "Total"

putexcel C33 = ("Distribution") E33 = ("Rates") G33 = ("Interaction")
putexcel B34 = "% Change due to"

// Fill Shell with estimates

* Hispanic teens
putexcel C5=matrix(distteens4)
putexcel G5=matrix(pregsteens4)
putexcel C8=formula(+C5+C6+C7)
putexcel D8=formula(+D5+D6+D7)
putexcel G8=`nmfrateteens411'
putexcel H8=`nmfrateteens422' 

putexcel C10=`pdiff_distteens42'
putexcel E10=`pdiff_ratesteens42'
putexcel G10=`pdiff_intteens42'

* Black women teens
putexcel C12=matrix(distteens6)
putexcel G12=matrix(pregsteens6)
putexcel C15=formula(+C12+C13+C14)
putexcel D15=formula(+D12+D13+D14)
putexcel G15=`nmfrateteens611'
putexcel H15=`nmfrateteens622'

putexcel C17=`pdiff_distteens62'
putexcel E17=`pdiff_ratesteens62'
putexcel G17=`pdiff_intteens62'

* Hispanic women in twens
putexcel C22=matrix(disttwens4)
putexcel G22=matrix(pregstwens4)
putexcel C25=formula(+C22+C23+C24)
putexcel D25=formula(+D22+D23+D24)
putexcel G25=`nmfratetwens411'
putexcel H25=`nmfratetwens422'

putexcel C27=`pdiff_disttwens42'
putexcel E27=`pdiff_ratestwens42'
putexcel G27=`pdiff_inttwens42'

* Black women in twens
putexcel C29=matrix(disttwens6)
putexcel G29=matrix(pregstwens6)
putexcel C32=formula(+C29+C30+C31)
putexcel D32=formula(+D29+D30+D31)
putexcel G32=`nmfratetwens611'
putexcel H32=`nmfratetwens622'

putexcel C34=`pdiff_disttwens62'
putexcel E34=`pdiff_ratestwens62'
putexcel G34=`pdiff_inttwens62'

// Calculates the change in rates between periods. Outer loop is by race/ethnic groupd
// Next is about comparison (early versus later).
// Innermost loop is by age.

//Teens

// distribution
foreach re in 5 12 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel E`r'=formula((+D`r')-(C`r'))
		}
}


// rates
foreach re in 5 12 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel I`r'=formula((+H`r')-(G`r'))
		}
}

// Twens

// distribution
foreach re in 22 29 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel E`r'=formula((+D`r')-(C`r'))
		}
}


// rates
foreach re in 22 29 {
	local rl = `re' + 3
		forvalues r = `re' / `rl' {
			putexcel I`r'=formula((+H`r')-(G`r'))
		}
}

