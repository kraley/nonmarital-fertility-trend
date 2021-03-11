// This is a decomposition analysis identifying how much of the trend in non-marital 
// pregnancy is due to changes in rates and how much to changes in composition

// Decomposition of pregnancy rates by contraceptive use, stratified by race/ethnicity and age

* By Kelly Raley and Kristen Burke

use "$NSFGKeep/cohabfertlong_appended.dta", clear

// Exclusion criteria -- race, marsta
keep if exclude   == 0
keep if HISPRACE == 1 | HISPRACE == 3
keep if nmarsta  == 1 | nmarsta == 2

svyset SECU [pweight = xrndweight], strata(SEST)

// tabs - 
// Hispanic teens
tab cpmethodcat5 period if HISPRACE == 1 & agecat2 == 1
tab cpmethodcat5 period if HISPRACE == 1 & agecat2 == 1 & p == 1 // pregnancies
// Hispanic 20s
tab cpmethodcat5 period if HISPRACE == 1 & agecat2 == 2
tab cpmethodcat5 period if HISPRACE == 1 & agecat2 == 2 & p == 1 // pregnancies
// Black teens
tab cpmethodcat5 period if HISPRACE == 3 & agecat2 == 1
tab cpmethodcat5 period if HISPRACE == 3 & agecat2 == 1 & p == 1 // pregnancies
// black 20s
tab cpmethodcat5 period if HISPRACE == 3 & agecat2 == 2
tab cpmethodcat5 period if HISPRACE == 3 & agecat2 == 2 & p == 1 // pregnancies


// Run sensitivity analysis
putexcel set "$results/decomposition_cpuse_byage_byrace.xlsx", replace

local periods 03-06 07-10 11-15
local racethn blank blank blank Hispanic NHwhite NHblack
local agec   blank blank blank blank blank blank 1519 2029
local comparison early late


// Loops from wide to narrow: comparison (early, late); racethnicity (Hispanic, Black); period (first, second); 
// First estimate the distribution by relationship status, next pregnancy rates by relationship status
// Calculate pregnancy rates under different scenarios (e.g. first period distribution, second period rates). 
// Actual pregnancy rates should be on the diagonals 


// Loop over comparisons: 1) early (0306 vs 0710) or 2) late (0710 vs 1115)
forvalues c = 1/2 {

	// Store name of comparison period in `comp' (early or late)
	local comp : word `c' of `comparison'
	
	local d = `c'+1
	// Store years of the two periods that we're comparing in `first' and `second'
	local first : word `c' of `periods'
	local second : word `d' of `periods'
	
	// Loop over race/ethnicity-- focusing on Hispanic and NHBlack
	foreach re in 4 6 {
		foreach age in 7 8 {
			use "$NSFGKeep/cohabfertlong_appended.dta", clear
		
			// I'm bumping valid values of HISPRACE higher so that the matrices have unique ids
			// If I don't then I can have pregs13 mean pregsearlycomparisonblack or pregshispanic11-15
			replace HISPRACE = HISPRACE+3
			// Also bumping the values of agecat2 so that they uniquely identify age groups
			replace agecat2 = agecat2 + 6
		
			* Identify sample for this loop
			// Exclude those issing nmarsta (not non-marital person-months)
			replace exclude =  1 if missing(nmarsta)
			// Exclude not sexually active person-months
			replace exclude = 1 if nmarsta == 3
			// Drop all excluded person-months
			keep if exclude == 0
			// Drop all person-months that are not for people of race/ethnicity in this loop
			// 4 and 6 (the values of this loop) correspond to Hispanic and NHBlack in `racethn', set outside of the loop
			// as well as the new values of HISPRACE recoded above.
			keep if HISPRACE == `re'
			// Drop all person-months that are not for people of age group in this loop
			keep if agecat2  == `age'
		
			// Svyset data
			svyset SECU [pweight = xrndweight], strata(SEST)
		
			// Store race that we're focusing on in this loop in `reth'
			local reth:     word `re' of `racethn'
			local agecat: 	word `age' of `agec'
	   
		* Distribution of person-months by contraceptive use
		// Loop over periods 1-2 or 2-3, depending on the value of c in the outermost loop
		forvalues period=`c'/`d' {
			// There are some period/race/age combinations which have 0 observations in the
			// permanent contraception category, so it throws an error when you try to append matricies.
			// Tab cpmethodcat5
			quietly svy: tab cpmethodcat5 if period == `period'
			// If the length of the vector that results is 4 (instead of 5, for 5 categories of contraception)....
			if `e(r)' == 4 {
				// check to make sure that the category that's missing observations is permanent contraception
				count if cpmethodcat5 == 1 & period == `period'
				// If that's what's missing, make row 1 of the vector = 0, and append proportions in each
				// other contraception category below
				if `r(N)' == 0 {
					matrix distP`re'`age'`period' = [0\e(Prop)]
				}
			}
			else if `e(r)' == 3 {
				count if cpmethodcat5 == 1 & period == `period' // count permanent
				local permanent `r(N)'
				count if cpmethodcat5 == 2 & period == `period' // count larc
				local larc `r(N)'
				if `permanent' == 0 & `permanent' == 0 {
					matrix distP`re'`age'`period' = [0\0\e(Prop)]
				}
			}
			// If the vector is length 5 when tabbing cpmethodcat5, just store that vector of proportions as matrix.
			else {
				svy, subpop(if exclude == 0): tab cpmethodcat5 if period == `period'
				// Store distribution in a matrix called distP`re'`period',
				// So for black teens in period 2 in the early comparison,
				// matrix would be called distP672
				matrix distP`re'`age'`period' = e(Prop)
				}
		}

		display "horitontally append matrices distP`re'`age'`c', distP`re'`age'`d'"
		matrix dist`comp'`re'`age' = distP`re'`age'`c', distP`re'`age'`d'
		
		// drop unappended matricies after we've appended them
		matrix drop distP`re'`age'`c' distP`re'`age'`d'
		
		// Label the row/column names in the matrix, which at this point is
		// the distribution of contraceptive method use in two periods which we are
		// comparing among women of raceeth `re'
		matrix rownames dist`comp'`re'`age' = Permanent LARC Hormonal LEM None
		matrix colnames dist`comp'`re'`age' = `first' `second'

		display "Distribution of `agecat' year old `reth' women by contraceptive use (`comp' comparison)"
		matrix list dist`comp'`re'`age'
	
		* Pregnancy rates by contraceptive use
		// Loop over periods 1-2 or 2-3, depending on the value of c in outermost loop
		forvalues period = `c'/`d' {
			// Loop over each contraceptive method category we're considering
			forvalues method = 1/5 {
				quietly count if cpmethodcat5 == `method' & period == `period'
				// if there are no observations recorded for that method, age, race, period
				// record a 0 in the matrix for pregnancy rates
				if r(N) == 0 {
					matrix pregs`re'`age'`period'`method' = 0
				}
				else {
					// Estimate the mean person-months pregnancies began if a particular relationship status
					// and period
					quietly svy, subpop(if exclude == 0): mean p if cpmethodcat5 == `method' & period == `period'
					// Multiply the person-month mean by 12*1000 to get an annual rate, and store in 
					// a (one cell) matrix.
					// For example, matrix would be pregs6712 for black teens in 0306 the "early"
					// comparison who are sexually active single
					matrix pregs`re'`age'`period'`method' = e(b)*(12*1000)
				}
			}
		}
		
		// Horzontally append matricies within relationship statuses across the two periods
		// of comparison-- i.e. append sexually active black women's pregnancy rates in
		// periods 0306 and 0710 if we're in the early comparison period to create a 
		// two-cell matrix
		forvalues method = 1/5 {
			matrix pregs`re'`age'`method' = pregs`re'`age'`c'`method', pregs`re'`age'`d'`method'
			// drop single cell matricies
			matrix drop pregs`re'`age'`c'`method' pregs`re'`age'`d'`method'
		}

		// Vertically appending rows of pregnancy rate matricies for each cp use status
		// The number at the end of each matrix name that is not a local indicates
		// which contraceptive method is used.
		matrix pregs`comp'`re'`age' = pregs`re'`age'1
		matrix pregs`comp'`re'`age' = (pregs`comp'`re'`age'\pregs`re'`age'2)
		matrix pregs`comp'`re'`age' = (pregs`comp'`re'`age'\pregs`re'`age'3)
		matrix pregs`comp'`re'`age' = (pregs`comp'`re'`age'\pregs`re'`age'4)
		matrix pregs`comp'`re'`age' = (pregs`comp'`re'`age'\pregs`re'`age'5)

		// Drop matricies for individual rows
		matrix drop pregs`re'`age'1 pregs`re'`age'2 pregs`re'`age'3 pregs`re'`age'4 pregs`re'`age'5

		// Label row/column names in the matrix, which at this point is the pregnancy
		// rate by relationship status among women in the two periods which we are 
		// comparing among women of raceeth `re'
		matrix rownames pregs`comp'`re'`age' = Permanent LARC Hormonal LEM None
		matrix colnames pregs`comp'`re'`age' = `first' `second'

		display "Pregnancy rates of `agecat' year old `reth' women by contraceptive use (`comp' comparison)"
		matrix list pregs`comp'`re'`age'

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
		// E.g product1671212 = product of distribution of relationship
		// status in the early comparison period (1) of black (6) teens (7) who are sterilized (1)
		// in 0710 (2) * pregnancy rates of black teens who were sterilized (1) in 0710 (2)		
		forvalues ds = 1/5{
			forvalues dp = 1/2{
				forvalues rs = 1/5{
					forvalues rp = 1/2{
						gen product`comp'`re'`age'`ds'`dp'`rs'`rp'=dist`comp'`re'`age'[`ds',`dp'] * pregs`comp'`re'`age'[`rs',`rp']
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
	* s = contraceptive use status
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
				gen nmfrate`comp'`re'`age'`dist'`r' = 0 
				// Contraceptive use statuses -- sum across all statuses to get total fertility under this
				// combination of dist * rate
				forvalues s = 1/5 {
					replace nmfrate`comp'`re'`age'`dist'`r' = nmfrate`comp'`re'`age'`dist'`r' + product`comp'`re'`age'`s'`dist'`s'`r'
				}
				// Save the sum in a local that indicates comparison period, race, which distribution, and which rate
				// E.e. nmfrate1612 = nmfert rate among black women using 1st period distribution and 2nd period rates in the early comparison period
				local nmfrate`comp'`re'`age'`dist'`r': di %5.2f = nmfrate`comp'`re'`age'`dist'`r'
			}
		}
	
		// Generate a matrix that has the nonmarital fertility rates that we observe on the diag [1,1] and [2,2]
		// and counterfacutals if rates or distribution had been unchangnged on the off diag
		matrix scenarios`c'`re'`age' = (`nmfrate`comp'`re'`age'11', `nmfrate`comp'`re'`age'12'\ `nmfrate`comp'`re'`age'21', `nmfrate`comp'`re'`age'22')
		
		// Label row/column names and save matrix
		// Columns are differences in rates, rows are differenecs in distribution
		matrix rownames scenarios`c'`re'`age' = dist_`first' dist_`second'
		matrix colnames scenarios`c'`re'`age' = rates_`first' rates_`second'

		display "Non-Marital Pregnancy rates for `agecat' year old `reth' women under different scenarios"
		matrix list scenarios`c'`re'`age'

*********************************************************************
* Decomposition
*********************************************************************

		// Calculate difference in observed nonmarital fertility between periods; 
		// the difference on the diag.
		local adiff`comp'`re'`age'`d': di %5.2f = scenarios`c'`re'`age'[2,2] - scenarios`c'`re'`age'[1,1] 
		display "Between period `first' and `second' the non-marital fertility rates increased by `adiff`comp'`re'`age'`d'' births per 1,000 `agecat' year old `reth' women (a negative value indicates a decline in the non-marital fertility rate)."
   
		// Calculate difference if distribution was unchanged -- call it rdiff  for rates differing
		// Rate differences are across columns, so compare cells [1,1 ] and [1,2]
		local rdiff`comp'`re'`age'`d': di %5.2f = scenarios`c'`re'`age'[1,2] - scenarios`c'`re'`age'[1,1]
		display "The change in non-marital fertility rates between period `first' and `second' if distributions had remained unchanged would be `rdiff`comp'`re'`age'`d''."

		// Calculate difference if rates were unchanged unchanged -- call it ddiff, for distributions differing
		// Distribution differences are across rows, so compare fertility rates in cells [2,1] and [1,1]
		local ddiff`comp'`re'`age'`d': di %5.2f = scenarios`c'`re'`age'[2,1] - scenarios`c'`re'`age'[1,1]
		display "The change in non-marital fertility rates between period `first' and `second' if rates had remained unchanged would be `ddiff`comp'`re'`age'`d''"

* Proportion of difference due to rates and to distribution   

		// Proportion change in pregnancy rates due to changing rates within groups is the difference in pregnancy rates when rates vary but distribution
		// is held constant over the total change in pregnancy rates -- rdiff over adiff
		local pdiff_rates`comp'`re'`age'`d': di %4.1f = 100*`rdiff`comp'`re'`age'`d''/`adiff`comp'`re'`age'`d''
		// Prop change in pregnacny rates due to changing distributions is difference in pregnancy rates when rates are held constant but distribution
		// varies over total change in pregnancy rates -- ddiff over adiff
		local pdiff_dist`comp'`re'`age'`d' : di %4.1f = 100*`ddiff`comp'`re'`age'`d''/`adiff`comp'`re'`age'`d''
		// Interaction is x when prop rates + prop dist + x = 100 %
		local pdiff_int`comp'`re'`age'`d': di %4.1f = 100-`pdiff_rates`comp'`re'`age'`d'' - `pdiff_dist`comp'`re'`age'`d''
  
		display "Rates account for `pdiff_rates`comp'`re'`age'`d'' percent of the difference in non-marital fertility rates between period `first' and `second'. Distribution accounts for `pdiff_dist`comp'`re'`age'`d'' percent and the interaction accounts for `pdiff_int`comp'`re'`age'`d'' percent."
 
		di " "
		di "************************************************************************************************************ "
		di " "
		}
	}
}

macro list

// This table will contain estimates of changes in fertiililty rate by contracepvie use, 
// Create Shell
putexcel A1 = "Table 3. Decomposition of Trends in Non-Marital Fertility Rate by Contraceptive Use Among Cohabiting and Sexually Active, Single Person-Months"
putexcel C2 = "Change from 2003-06 to 2007-10"
putexcel C3= ("Distribution") G3=("Pregnancy Rates")
putexcel C4=("2003-06") D4=("2007-10") E4=("Change") G4=("2003-06") H4=("2007-10") I4=("#Change") J4=("%Change")

putexcel A5= ("Hispanic, age 15-19") B5 = ("Permanent")
putexcel B6= "LARC"
putexcel B7 = "Hormonal"
putexcel B8 = "LEM"
putexcel B9 = "None"
putexcel B10 = "Total"
putexcel C11 = ("Distribution") E11 = ("Rates") G11 = ("Interaction")
putexcel B12 = "% Change due to"
putexcel A14= ("Hispanic, age 20-29") B14 = ("Permanent")
putexcel B15= "LARC"
putexcel B16 = "Hormonal"
putexcel B17 = "LEM"
putexcel B18 = "None"
putexcel B19 = "Total"
putexcel C20 = ("Distribution") E20 = ("Rates") G20 = ("Interaction")
putexcel B21 = "% Change due to"

putexcel A23= ("Black, age 15-19") B23 = ("Permanent")
putexcel B24= "LARC"
putexcel B25 = "Hormonal"
putexcel B26 = "LEM"
putexcel B27 = "None"
putexcel B28 = "Total"
putexcel C29 = ("Distribution") E29 = ("Rates") G29 = ("Interaction")
putexcel B30 = "% Change due to"
putexcel A32= ("Black, age 20-29") B32 = ("Permanent")
putexcel B33= "LARC"
putexcel B34 = "Hormonal"
putexcel B35 = "LEM"
putexcel B36 = "None"
putexcel B37 = "Total"
putexcel C38 = ("Distribution") E38 = ("Rates") G38 = ("Interaction")
putexcel B39 = "% Change due to"

// Fill Shell with estimates

* Hispanic teens 2003-06 to 2007-10
putexcel C5=matrix(distearly47)
putexcel G5=matrix(pregsearly47)
putexcel C10=formula(+C5+C6+C7+C8+C9)
putexcel D10=formula(+D5+D6+D7+D8+D9)
putexcel G10=`nmfrateearly4711'
putexcel H10=`nmfrateearly4722' 

putexcel C12=`pdiff_distearly472'
putexcel E12=`pdiff_ratesearly472'
putexcel G12=`pdiff_intearly472'

* Hispanic 20s 2003-06 to 2007-10
putexcel C14=matrix(distearly48)
putexcel G14=matrix(pregsearly48)
putexcel C19=formula(+C14+C15+C16+C17+C18)
putexcel D19=formula(+D14+D15+D16+D17+D18)
putexcel G19=`nmfrateearly4811'
putexcel H19=`nmfrateearly4822' 

putexcel C21=`pdiff_distearly482'
putexcel E21=`pdiff_ratesearly482'
putexcel G21=`pdiff_intearly482'

* Black teens 0306-0710
putexcel C23=matrix(distearly67)
putexcel G23=matrix(pregsearly67)
putexcel C28=formula(+C23+C24+C25+C26+C27)
putexcel D28=formula(+D23+D24+D25+D26+D27)
putexcel G28=`nmfrateearly6711'
putexcel H28=`nmfrateearly6722' 

putexcel C30=`pdiff_distearly672'
putexcel E30=`pdiff_ratesearly672'
putexcel G30=`pdiff_intearly672'

* Black 20s 0306-0710
putexcel C32=matrix(distearly68)
putexcel G32=matrix(pregsearly68)
putexcel C37=formula(+C32+C33+C34+C35+C36)
putexcel D37=formula(+D32+D33+D34+D35+D36)
putexcel G37=`nmfrateearly6811'
putexcel H37=`nmfrateearly6822' 

putexcel C39=`pdiff_distearly682'
putexcel E39=`pdiff_ratesearly682'
putexcel G39=`pdiff_intearly682'
