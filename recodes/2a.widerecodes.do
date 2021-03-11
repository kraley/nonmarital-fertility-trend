* Load responses to the 2006-2010 data, females
// use "$NSFG06_10/nsfg0610fem.dta", clear
// use "$NSFG11_13/Female_NSFG1113.dta", clear
// use "$NSFG13_15/2013_2015_FemRespData.dta", clear
local wave `1'
di `wave'
local filepath "`2'"
use `filepath', clear

describe
global startingN `r(N)'

* Keep relevant variables
keep CASEID AGE_A AGE_R CMBIRTH CMINTVW SECU WGT* SEST CMJAN3YR 				/// administrative
MARDAT* 																		/// month of marriage - NSFG recoded variable
MAREND* MARDIS* 																/// how marriage ended and when it ended
FMARIT FMARNO MARSTAT															/// marital status at interview, number of marriages
CMHSBDIEX CMHSBDIEX*															/// month husband died
CMDIVORCX CMDIVORCX* 															/// month of divorice
CMSTPHSBX CMSTPHSBX* 															/// month of separation
CMPMCOHX CMPMCOHX*																/// premarital cohab dates
CMCOHSTX CMCOHSTX* CMSTRTCP 													/// nonmarital cohab dates
CMSTPCOHX CMSTPCOHX*     														/// cohab end dates
TIMESCOH PMARRNO NONMARR 														/// summary of cohabs
RHADSEX CMFSTSEX CMMONSX CMMONSX* MONSX* SEX3MO 								/// sexual activity dates
HISPRACE RSCRRACE HISP HISPGRP EDUCMOM HIEDUC 									/// demographic characteristics
PREGNUM																			/// number of pregnancies
DATEND* OUTCOM* DATCON*															/// pregnancy outcomes and dates
LBPREGS 																		/// pregnanices that end in live birth
CONSTAT* METHX* CMMHCALX* EVERUSED NUMMULTX* METH3M* CMFIRSM					/// contraceptive method each month 
BRNOUT																			/// immigration status
PSTRSTAT RSTRSTAT ADDEXP INTENT JINTENDN INTENDN PARITY RCURPREG 				///  additional fertility intentions
WYNOTUSE CONSTAT1 CMFSTSEX														///
CMTUBLIG CMHYST CMOVAREM CMOTSURG CMOPER1 CMLIGREV CMMALEOP CMVASREV RSTRSTAT FECUND //  sterilization dates

gen wave = `wave'																


*****************
** CURRENT AGE **
*****************

// Current age
// CM at time of interview minus CM of birth
// this is the maximum age that we've observed
// each person at.
gen curage = CMINTVW-CMBIRTH 

**************
** MARRIAGE ** 
**************

** Beginning of marriage - age (in months)
summarize MARDAT0?

summarize FMARNO // NSFG recoded number of marriages for all respondents
// for each possible marriage (1/`r(max)') subtract date of birth in CM from 
// marriage to calculate R's age (in months) at marriage
forvalues i=1/`r(max)' {
	recode MARDAT0`i' 9997/9999 = .
	gen agem`i'= MARDAT0`i'-CMBIRTH
}
summarize agem*


** End of marriage - age (in months)

// MAREND## -  How did each marriage end? divorced or annulled, separated, widowed
// MARDIS## - NSFG recoded century month for date of each marriage end
/* Note on MARDIS: the NSFG recode takes into account separations that ocurr prior to 
 * divorce, meaning that all of the dates that we're interested in are rolled up in this variable,
 * since we are interested in considering the date of separation as the date of marriage end
 * (there's a note in the prev PAA abstract that says this as a result of classed
 * differences in divorce due to costs).
 
 * See the NSFG codebook about which dates are incorporated in the MARDIS recode 
 * (below and available at https://www.icpsr.umich.edu/webdocs/jsp/documents/recode_specs/fem/MARDIS01.pdf):
 
	cmhsbdiex - if marriage ended when husband died.
	cmstphsbx - if marriage ended in separation, or if it ended in divorce/annulment but R stopped living with husband before
				divorce/annulment, or if DK/RF how it ended but valid date reported in this variable.
	cmdivorcx - if marriage ended in divorce/annulment and R did not stop living with
				husband prior to divorce/annulment.
 */

 
// FMARIT = current marital status
tab FMARIT 
recode FMARIT 2/3 = 2 4 = 3 5 = 4 , gen(curmar) // Collapse divorced and widowed, keep separated distinct from married
label define curmar 1 "Married" 2 "Divorced or widowed" 3 "Separated" 4 "Never married"
label values curmar curmar
tab curmar

summarize FMARNO
forvalues i=1/`r(max)' {
	// gen age in months that each marriage ended
	gen agendm`i' = .
	// subtract mardis date from birth date
	replace agendm`i' = MARDIS0`i' - CMBIRTH
	// if respondent is currently married, replace age of end of marriage
	// with age at current interview
	replace agendm`i' = CMINTVW - CMBIRTH if FMARNO == `i' & curmar == 1 
}
summarize agendm*



** Marital status at each month ** 

// Generate a person-month variable for every possible month that somone
// could have provided data about their marital status (up until the age at present interview)
forvalues i= 180/528 {
	// generate mar`i' for each possible age that each respondent was
	// observed
	gen mar`i' = 0
	// recode mar`i' as missing if it is after their age
	// at interview, i.e. it's in the future relative to the interview
	replace mar`i' = . if `i' > curage
}

// Fill in  marital status at each month for each person
// store max number of marriages and min/max current ages in locals
summarize FMARNO
local maxmarriage `r(max)'
// Outer loop: all possible marriages
forvalues M = 1/`r(max)' {
// Inner loop: all ages of interest (15 years * 12 = 180, 44 years * 12 = 528)
	forvalues A = 180/528 {
		// if age (in months) that a marriage began is less than month `A', and
		// a respondent has at least one recorded marriage, code them as "married"
		// during month `A'
		replace mar`A'= 1 if agem`M'   <= `A' & agendm`M' >  `A' & FMARNO != 0	
		// if a respondent is married and the marriage ends in the same month, 
		// code them as married during that month
		replace mar`A'= 1 if agem`M'   == `A' & agendm`M' == `A' & FMARNO != 0 
		// if a respondent is currently married and that marriage is ongoing
		// code them as married up until the final month that theyr'e observed.
		replace mar`A'= 1 if agendm`M' == `A' & FMARNO    == `M' & curmar == 1 
	}
}

******************
** Cohabitation **
******************

// Note the distinction between premarital & nonmarital cohabitations
// Also note that there are not NSFG recoded ages at each cohabitations, as there were
// for marriages, therefore we use the raw data (which is somewhat messier).


// PMARRNO - number of premarital cohabitations; cohab ended with marriage
// NONMARR - number of nonmarital cohabitations; cohab did not end with marriage (dissolved or ongoing)
// TIMESCOH - total number of cohabitations
summarize PMARRNO NONMARR TIMESCOH
codebook PMARRNO NONMARR TIMESCOH

 
// Recode all variables that don't end in a numeral to correspond to the
// relationship order they belong to
gen CMPMCOHX1 = CMPMCOHX  // CM began premarital cohabitation with 1st husband
gen CMCOHSTX1 = CMCOHSTX  // CM began NONmarital cohabitation with first partner
gen CMCOHSTX5 = CMSTRTCP  // CM began cohabiting with current cohabiting partner
// NOTE: we only have start dates for up to 4 non-marital cohabs, despite the fact that there are
// some respondents who have > 4 cohabs recorded (see tab NONMARR) 
// Because we only have data up to 4, we're calling the current non-marital cohab
// cohab 5. 
// This is resolved in the long recodes script by dropping all person-months after
// the conclusion of the 4th non-marital cohabitation if the respondent had more than 5 non-marital
// cohabitaitons. 
// If they had 4 NM cohabs, all are recorded. 
// If they had exactly 5, and one of them is ongoing, all are recorded
// If they had 5 or greater and none are ongoing, the 5th and above are not recorded.

// For documentation purposes: what percentage of our sample has more than 4 cohabitations? Note, could be missing cohabitations between 4-5.
tab NONMARR
// there are 35 cases where NONMARR > 4 in the NSFG 06-10; 0.03% of sample
// there are 17 cases where NONMARR > 4 in the NSFG 11-13
// there are 16 cases where NONMARR > 4 in the NSFG 13-15

** PREMARITAL COHABITATIONS -- i.e. cohabitations preceding a marriage ** 
// Age at beginning of premarital cohabitation in months

// NOTE:  premarital cohabs don't need an "end" date because
// they then become marriages-- if the marriage ends, we've already
// recorded their marriage end date

codebook CMPMCOHX
tab PMARRNO
// Note: there's a misalignment in number of premarital cohabs in PMARRNO
// and the number of dates we have (up to 6) in CMPMCOHX. This is likely due to recodes?
// This only exists for one observation in the 0610 data, which is among someone with
// some inconsistent reporting in dates and subsequently gets exlucded. All other iterations
// have max PMARRNO == number of pm cohabs for which there are dates stored in CMPMCOHX

// for each possible premarital cohab subtract date of birth in CM from 
// CM of PM cohab to calculate R's age (in months) at PM cohab
summarize PMARRNO
forvalues M = 1/`r(max)' {
	recode CMPMCOHX`M' 9998/9999 = .
	gen cohm`M' = CMPMCOHX`M' - CMBIRTH
}
gen premarcohnum = PMARRNO //total number of premarital cohabitation(cohabitation ended with marriage)


** NONMARITAL COHABITATIONS -- i.e. cohabitations that have not (yet) transitioned to marriage **
// Age at beginning of nonpremarital cohabitation 

codebook CMCOHSTX
tab CMCOHSTX
tab NONMARR
// Note a misalignment in the number of nonmarital cohabs recorded in NONMARR
// and the number of cohabs we have dates recorded for.

// For each possible non-marital cohab, subtract CM date of birth
// from CM of cohab start to get age (in months) respondent was at cohab start
forvalues C = 1/5 {
	recode CMCOHSTX`C' 9998/9999 = .
	gen cohp`C' = CMCOHSTX`C' - CMBIRTH
}

// Age at end of nonpremarital cohabitation 

// put a numeral at the end of nonmarital cohab 1 end
gen CMSTPCOHX1 = CMSTPCOHX

// only cohabs 1/4 here because 5 is the current cohab -- coded outside of loop
forvalues C= 1/4 {                 
	recode CMSTPCOHX`C' 9998/9999 = .
	gen cohpend`C' = CMSTPCOHX`C' - CMBIRTH
}
//age of ending nonpremarital cohabitation for current cohabitation
//for current cohabitation the ending date is the date of interview
gen cohpend5 = CMINTVW - CMBIRTH if CMCOHSTX5 != .
 
*********************************************************
** Person-Month level marriage and cohabitation status **
*********************************************************

** Marital-Cohab status at each person-month **

label define mcstatus 0 "Single" 1 "Married" 2 "Cohabiting"

// Number of marriges changes between waves;
// set local to max no of marriages, and use to to record premarital cohabs
// and marriages in subsequent loop
summarize PMARRNO
local maxnopmcohabs `r(max)'

forvalues A = 180/528 {
	gen mc`A' = 0
	label values mc`A' mcstatus
	gen cohdur`A' = 0 // a variable recoding the duration of the cohab at age i
		// for each possible premarital cohab
	forvalues M = 1/`maxnopmcohabs' {
		// replace mc status for person-month A to record cohab if
		// premarital cohab starts before month `A' and month `A' is less than
		// the beginning of the subsequent marriage
		replace mc`A' = 2 if cohm`M' <= `A' & `A' < agem`M' & agem`M' != .
		// if cohab and married coded in the same month, code as married
		replace mc`A' = 1 if cohm`M' == `A' & `A' == agem`M'
		
		// code as married if marriage began before month `A' and ended after `A'
		replace mc`A' = 1 if agem`M' <= `A' & agendm`M' > `A'
		// if marriage and end of marriage are recorded in the same month, record as married
		replace mc`A' = 1 if agem`M' == `A' & `A' == agendm`M'
		// code as married if still married at the time of interview
		replace mc`A' = 1 if agendm`M' == `A' & FMARNO == `M' & curmar == 1
		replace cohdur`A' = `A'-cohm`M' if mc`A' == 2 
		}
	forvalues k = 1/5 {
		// replace person-month cohab status to record cohab if 
		// nonmarital cohab began before person-month `A' and ends after 
		// person-month `A'
		replace mc`A' = 3 if cohp`k' <= `A' & `A'  < cohpend`k'
		// code as cohab if cohab begins and ends in same month
		replace mc`A' = 3 if cohp`k' == `A' & `A' == cohpend`k'
		// if cohab is onging (i.e. it's cohp5) code the month as cohabiting
		replace mc`A' = 3 if `A' == cohpend5
		replace cohdur`A' = `A' - cohp`k' if mc`A' == 3
		replace mc`A' = 2 if mc`A' == 3 
	}
	// code all months after interview (the future) as missing
	replace mc`A' = . if `A' > curage
}

// In the case that there is concurrent marriage and cohabitation going on, code as married
// KR: I agree with this choice. People's lives are messier than our coding will allow, but it is 
// quite possible  to be both married to someone and cohabiting wth someone else
// the birth would probably be coded as marital in official statistics
forvalues A = 180/528{
	// each possible marriage
	forvalues M = 1/`maxnopmcohabs' {
		// each possible cohab
		forvalues C = 1/5 {
			// if both the marriage and cohab dates include person-month `A', consider married
			replace mc`A'=1 if agem`M'<=`A' &  agendm`M'>`A'   & cohp`C' <= `A' & cohpend`C' > `A'
			replace mc`A'=1 if agem`M'==`A' & `A' == agendm`M' & cohp`C' <= `A' & cohpend`C' >= `A'
		}
	}
}


****************************
** Pregnancies and births **
****************************
// DATEND - CM end of pregnancy
// OUTCOM - how pregnancy ended (live birth, abortion, miscarriage, etc)
// DATCON - CM date pregnancy began

// In this loop, recode only 1-9 to eliminate leading zeros in varnames
forvalues i=1/9 {
	rename DATEND0`i' DATEND`i'
	rename OUTCOM0`i' OUTCOM`i'
	rename DATCON0`i' DATCON`i'
}
summarize DATEND1 OUTCOM1 DATCON1
codebook DATEND1 OUTCOM1 DATCON1

// CM age at each birth
summarize PREGNUM
forvalues i=1/`r(max)' {
	gen birth`i' = DATEND`i'- CMBIRTH if OUTCOM`i' == 1 	
}
summarize birth*
// no live births for preg 14 or 16-19

// CM age at beginning of each pregnancy that ended in live birth OR is current at time of interview
summarize PREGNUM
forvalues i = 1/`r(max)' {
	gen pg`i' = DATCON`i' - CMBIRTH if (OUTCOM`i' == 1 | OUTCOM`i' == 6) // for current pregnancy and live birth 
} 
summarize pg*

// CMs in which respondent was pregnant
// Alterative option is to censor all pregnant person-months, including those
// that don't end in a birth. We decided not to do this, and instead just censor
// those person-months that end in a birth (or presumably end in a birth) because
// of the incomplete reporting of pregnancies that don't end in a birth.
summarize PREGNUM 
forvalues i = 1/`r(max)' {
	gen censorpregstart`i'   = pg`i' + 1
	gen censorpregend`i' 	 = DATEND`i'- CMBIRTH if OUTCOM`i' == 1 	
	replace censorpregend`i' = CMINTVW if OUTCOM`i' == 6
}

** Age in months pregnancy status **

// Identify the age, in months, that each pregnancy starts in
// Note that this includes pregnancies that are ongoing at the time of interview
summarize PREGNUM
local maxpreg `r(max)'
forvalues i=180/528 {
	gen p`i' = 0 
		forvalues j = 1/`maxpreg' {
		replace p`i' = 1 if pg`j' == `i'
		replace p`i' = . if `i' > curage
	}
}

// Identify the age, in months, that each birth took place in 
forvalues i=180/528 {
	gen bm`i'=0 
	forvalues h=1/`maxpreg' {
		replace bm`i' = 1 if birth`h' == `i'
		replace bm`i' = . if `i' > curage
	}
}

// Identify age in person-months which a person was pregnant for -- 
// these months will be censored, as a woman isn't at risk of a pregnancy when she
// is pregnant
summarize PREGNUM
local maxpreg `r(max)'
forvalues A = 180/528 {
	gen censorpreg`A' = 0
	forvalues P = 1/`maxpreg' {
		replace censorpreg`A' = 1 if (censorpregstart`P' <= `A' & censorpregend`P' >= `A')
		replace censorpreg`A' = . if `A' > curage
	}
}



// KR: These are here partly because of a longstanding tradition in this area of identifying marital status 7 months
// prior to a pregnancy to determine "shotgun" marriages. Couples might not have been aware of pregnancy
// in the first month or so and thus the marriage couldn't have been in response to the pregnancy. Also,
// not all pregnanciies last 9 months. So, 7 months is used for a "conservative" estimate of the prevalence of
// shotgun marriage.
//
// But this paper has a slightly different purpose and thus it might be more appropriate to mark the pregnancy
// as 7 months prior to the birth. The different measures are for robustness checks. 

*age at each conception
summarize PREGNUM
forvalues A=1/`r(max)' {
		gen preg`A'  = birth`A' - 7  //birth-7
		gen rpreg`A' = birth`A' - 8 //birth-8
		gen rrpreg`A'= birth`A' - 9 //birth-9
}
summarize birth*
summarize pg*

summarize PREGNUM
local maxnumpregs `r(max)'
forvalues A = 180/528 {
	gen pm`A' = 0 
	forvalues P   = 1/`maxnumpregs' {
		replace pm`A' = 1 if preg`P' == `A'
		replace pm`A' = . if `A'> curage
    }
}

forvalues A = 180/528 {
	gen rpm`A' = 0 
	forvalues P = 1/`maxnumpregs' {
		replace rpm`A' = 1 if rpreg`P' == `A'
		replace rpm`A' = . if `A' > curage
    }
}


forvalues A = 180/528 {
	gen rrpm`A' = 0 
	forvalues P = 1/`maxnumpregs' {
		replace rrpm`A' = 1 if rrpreg`P' == `A'
		replace rrpm`A' = . if `A' > curage
    }
}


// Total number of live pregnancy
forvalues i=1/`maxnumpregs' {
	gen b`i'=0
	replace b`i'=1 if birth`i'!=.
}

// This check makes sure that pregnancies that ended in live births
// are counted correctly -- because we use all possible pregnancies and
// then determine which ones are live births. 
gen numlp = LBPREGS
summarize PREGNUM 
egen tlp = rowtotal(b1-b`r(max)')
gen d = numlp-tlp 
tab d //all d equals 0/
summarize d
if `r(mean)' != 0 {
		di as error "Miscounted live births"
		exit(1)
}

egen numbirths=rowtotal(bm180-bm528)
gen d2 = numbirths- tlp
tab d2 // some negative numbers; these are births that ocurr outside of ages 15-44 - uncomment line below
// list pg if d2 < 0

// Drop pregnancy variables that won't be used moving forward
drop d2 numlp tlp d b*


*********************
** Sexual Activity **
*********************


// MONSX  - did R have sex during this month?
// CMMONSX - the century month that each MONSX corresponds to
codebook CMMONSX MONSX

// replace the first in each series to have a numeral at the end
gen CMMONSX1 = CMMONSX
gen MONSX1 = MONSX

tab CMMONSX1
tab CMMONSX48

labelbook MONSX
// recode "No" to 0 (currently 5)
// recode missing values to 9
forvalues i=1/48 {
	recode MONSX`i' 5 = 0 7/9 = 9 
}
summarize MONSX*

// Create agemonth var to correspond with CM that sexual activity is recorded in
forvalues i = 1/48 {
	gen agemonthsex`i' = CMMONSX`i' - CMBIRTH
}
summarize agemonthsex1-agemonthsex48

// for each age-month, indicate whether or not R had sex
// from ages 15-44
label define sexmonth 0 "No" 1 "Yes" 9 "Don't know"
forvalues i = 180/528 {
	gen sexmonth`i' = .
		forvalues j = 1/48 {
			// if the agemonth that sex is recorded in (j) is the same as loop `i', replace with value of MONSX
			replace sexmonth`i' = MONSX`j' if agemonthsex`j' == `i'
	}
	label values sexmonth`i' sexmonth
}

// for respondents who had missing values, if they'd never had sex (RHADSEX == 5), replace those 
// values with 0 - no changes
forvalues i=180/528 {
	forvalues j=1/48 {
		replace sexmonth`i' = 0 if RHADSEX == 5 & agemonthsex`j' == `i'& MONSX`j' == 9
	}
} 


gen anysxmis = 0
// Identifying who has missing values for sexual activity
// we'll later drop person months where respondent was missing sexual activity and was
// not currently in a cohabiting or married relationship.
forvalues i = 1/48 {
	replace anysxmis = 1 if MONSX`i' == 9 
}
// Identifying cases where all data on sexual activity are missing
gen allsexmis = 1
forvalues i = 1/48 {
	replace allsexmis = 0 if MONSX`i' ==0 | MONSX`i' == 1
}

***********************
** Contraceptive use ** 
***********************

// 55 "empty if jan/same method used thru end of year" shows up in METHX5 +
// also 2 "Same as last month"
gen methis55 = .
gen methis2 = . 
forvalues i = 1/192 {
	replace methis55 = 1 if METHX`i' == 55
	replace methis2 = 1 if METHX`i' == 2
}
// This is for a very small number of people.
// This loop attempts to rectify this by carrying forward the
// contraceptive method from a previous month if the previous month had a valid
// method recorded (not missing, don't know, refused) in months that have a 55 or 2

// Loop over methods in a year (leaving out 48 because there is no "next month" within a year
// for method 48)
forvalues method = 1/47 {
	// loop over each of the four years for which there is possibly contraceptive method data
	forvalues year = 1/4 {
		// methodyear == the method number (1/47) * the year (for up to 4 years of cp data)
		local methodyear = `method' * `year'
		// nextmo is the methodyear + 4, because there are up to 4 methods stored per month
		local nextmo = `methodyear' + 4
		// Replace next months' method with the current method if next month is "same as last month" (2) or "same all year" (55)
		// so long as this month's method is a valid response-- not missing, not refused, and not a "same as last month" or "same all year"
		replace METHX`nextmo' = METHX`methodyear' if (METHX`nextmo' == 55 | METHX`nextmo' == 2) & (METHX`methodyear' != 55 & METHX`methodyear' != 2 & METHX`methodyear' != . & METHX`methodyear' != 99 & METHX`methodyear' != 98)
	}
}

// METHX* - cp method used -- up to 4 each month for a total of up to 192 methods recorded
// CMMHCALX* - the month that the contraceptive calendar calendar corresponds to
// EVERUSED - did R ever use cp? methx not recorded for those who did not.

// in each month in which a woman was sexually active, what method of contraception was she using?
// NSFG stores up to 4 methods per month

// recode all contraceptive methods so that the value assigned is in an order indicating efficacy
// Order drawn from Contraceptive Technology 2018, Trussell et al.
// Q of where EC should go-- not a very common method reported, but effective when used?

label define cpmethod 1 "Male sterilization" 2 "Female sterilization" 3 "Implant" 4 "IUD" 5 "Injectable" 6 "Pill" ///
					  7 "Patch" 8 "Ring" 9 "EC" 10 "Condoms" 11 "Natural" 12 "Sponge" 13 "Diaphragm" 14 "Withdrawal" ///
					  15 "Female condoms" 16 "Spermicide" 17 "Other" 18 "None" ///
					  99 "Refused/don't know"
					  
forvalues i = 1/192 {
	gen meth`i' = .
	replace meth`i' = 1  if METHX`i' == 5 | METHX`i' == 23						// male ster (incl for non cp reasons)
	replace meth`i' = 2  if METHX`i' == 6 | METHX`i' == 22						// fem ster  (incl for non cp reasons)
	replace meth`i' = 3  if METHX`i' == 9  										// implant
	replace meth`i' = 4  if METHX`i' == 19 										// IUD
	replace meth`i' = 5  if METHX`i' == 8 | METHX`i' == 24 						// shot -- depo and lunelle
	replace meth`i' = 6  if METHX`i' == 3 										// pill
	replace meth`i' = 7  if METHX`i' == 25										// patch
	replace meth`i' = 8  if METHX`i' == 26								 		// ring
	replace meth`i' = 9  if METHX`i' == 20										// EC
	replace meth`i' = 10 if METHX`i' == 4  										// condoms
	replace meth`i' = 11 if METHX`i' == 11 | METHX`i' == 10 					// natural - natural/temp, calendar
	replace meth`i' = 12 if METHX`i' == 18										// sponge
	replace meth`i' = 13 if METHX`i' == 12 | METHX`i' == 16						// diaphragm & cervical cap
	replace meth`i' = 14 if METHX`i' == 7										// withdrawal
	replace meth`i' = 15 if METHX`i' == 13										// female condoms
	replace meth`i' = 16 if METHX`i' == 14 | METHX`i' == 15						// spermicide -- jelly or foam
	replace meth`i' = 17 if METHX`i' == 21 | METHX`i' == 17						// other
	replace meth`i' = 18 if METHX`i' == 1										// none
	replace meth`i' = 99 if METHX`i' == 98 | METHX`i' == 99 					// refused don't know
	
	label values meth`i' cpmethod
}

** identify the most effective method used in each month-- up to 4 stored per woman
// Collapse the methods used within each month to identify the most efficactious 
// we're looping over 48 possible months of method use, which up to 4 methods reported per month
forvalues i = 1/48 {
	// Gen "topmeth`i'" var which is the most effective method used in that month; initiate as missing
	gen topmeth`i' = .
	label values topmeth`i' cpmethod
	
	// Replace it with the first method reported in that month -- this follows a different path
	// depending on whether it's the first month or a subsequent month
	
	// if mo == 1, then just use i = i (METHX1 == first method in mo 1)
	if `i' == 1 {
		replace topmeth`i' = meth`i'
		
		// if the 2nd through 4th mention of method in the first month is more effective (has a lower # value) than
		// the method stored in the previous mentions, replace the method in topmeth with that 
		// method.
		forvalues j = 2/4 {
			replace topmeth`i' = meth`j' if meth`j' < topmeth`i' & meth`j' != .
		}
	}
	
	// if mo > 1, then use the method that's in month * 4 + 1
	// e.g. the first method in the 2nd month is stored in the variable METHX5 ((2ndmo - 1) * 4) + 1 = 5
	// in the 3rd is METHX9 (3-1) * 4 + 1 = 9
	if `i' > 1  {
		local j = ((`i' - 1) * 4) + 1
		replace topmeth`i' = meth`j'
		
		// if the 2nd through 4th mention of the method in the 2nd+ mo is more effective (has a lower # value) than
		// the method stored in the first mention, then replace the method in topmet with that method.
		
		// make k = `j', the first method for that month, + 1, 2nd method for that month
		// e.g. for 2nd month, j == 5, so k will first take the value 6
		local k = `j' + 1
		// then loop through 2/4, three digits, incrementing k by one for each loop, so that k in the 2nd month
		// is first evalued at value 6 (when l = 2), at 7 when l = 3, and at 8 when l = 4
		forvalues l = 2/4 {
			replace topmeth`i' = meth`k' if meth`k' < topmeth`i' & meth`k' != .
			local k = `k' + 1
		}
	}
}

// identify the respondent's age-month that each month of contraceptive history
// corresponds to
// Create agemonthcp var to correspond with CM that contraceptive use is recorded in
forvalues i = 1/48 {
	gen agemonthcp`i' = CMMHCALX`i' - CMBIRTH
}
summarize agemonthcp1-agemonthcp48


// for ages 15-44 match most effective method onto agemonth
forvalues i = 180/528 {
	gen cpmethodmonth`i' = .
		forvalues j = 1/48 {
			// if the agemonth that sex is recorded in (j) is the same as loop `i', replace with value of MONSX
			replace cpmethodmonth`i' = topmeth`j' if agemonthcp`j' == `i'
		}
	label values cpmethodmonth`i' cpmethod
}

// for respondents who had missing values on cpuse...
// - if they'd never used a method (EVERUSED == 5), replace those values with 18 (none) - no changes
forvalues i = 180/528 {
	forvalues j = 1/48 {
		replace cpmethodmonth`i' = 18 if EVERUSED == 5 & agemonthcp`j' == `i' & cpmethodmonth`i' == .
	}
} 
// if they didn't start using a method until after the month that they're missing contraceptive use
// data for (i.e. CMFIRSM > CM of contraceptive use with missing data) replace with 0
gen agemonthfirstcp = CMFIRSM - CMBIRTH
replace agemonthfirstcp = . if (CMFIRSM == 9998 | CMFIRSM == 9999)
forvalues i = 180/528 {
	forvalues j = 1/48 {
		replace cpmethodmonth`i' = 18 if agemonthfirstcp > `i' & agemonthfirstcp != .
	}
}


** Compare with CONSTAT & reconclie our recodes with NCHS
// Identify current most effective method of contraception -

gen curmethod = .
forvalues i = 1/48 {
	replace curmethod = topmeth`i' if agemonthcp`i' == curage
}
label values curmethod cpmethod

gen curmethod2 = .
forvalues i = 180/528 {
	replace curmethod2 = cpmethodmonth`i' if `i' == curage
}
label values curmethod2 cpmethod
// has fewer responses beccause doesn't recode greater than age 528 months

tab CONSTAT1 curmethod, m
tab curmethod CONSTAT1, m
// Some discrepancies here are OK
// Pregnant- we'll censor currently pregnant months, and we're looking for
// an indication of which method a woman was using the month that she became
// pregnant.
// However, recode has more sterilization than we have, particularly in the
// non contraceptive sterilization. The CONSTAT recodes use data from sterilization history
// to assign ster.

// Variables of interest here:
// CMTUBLIG - century month of R's tubal ster
// CMHYST - cm of r's hysterectomy
// CMOVAREM - cm of ovary removal
// CMOTSURG - cm for other sterilizing op
// CMOPER1 - cm for R's 1st or only sterlziing op
// CMLIGREV - cm of r's reversal of tubal ster

// CMMALEOP - cm of males ster op
// CMVASREV - cm of husband or partner's vas reversal

gen agemonthster = .
replace agemonthster = CMOPER1 - CMBIRTH 
gen agemonthsterrev = . 
replace agemonthsterrev = CMLIGREV - CMBIRTH

// a handful of people have had more than one op
gen nster = 0
foreach var in CMTUBLIG CMHYST CMOVAREM CMOTSURG {
	replace nster = nster + 1 if `var' != .
}
// Some come after the sterilization reversal, which we'd want to caputre

// Go through each type of sterilizing op and identify if the date
// if it happened after sterilization reversal
gen cmtubalafterrev = .
replace cmtubalafterrev = CMTUBLIG if (CMTUBLIG > CMLIGREV) & CMTUBLIG != .
gen hystafterrev = .
replace hystafterrev = CMHYST if (CMHYST > CMLIGREV) & CMHYST != .
gen ovafterrev = .
replace ovafterrev = CMOVAREM if (CMOVAREM > CMLIGREV) & CMOVAREM != .
gen othsterafterrev = .
replace othsterafterrev = CMOTSURG if (CMOTSURG > CMLIGREV) & CMOTSURG != .

// Collapse into one variable that has that date
gen cm2ndster = .
replace cm2ndster = cmtubalafterrev if cmtubalafterrev != .
replace cm2ndster = hystafterrev if hystafterrev < cm2ndster
replace cm2ndster = ovafterrev if ovafterrev < cm2ndster
replace cm2ndster = othsterafterrev if othsterafterrev < cm2ndster
// convert date to age
gen agemonth2ndster = .
replace agemonth2ndster = cm2ndster - CMBIRTH


// Partner sterilization for those who are married or cohabiting 
gen agemonthvas = .
replace agemonthvas = CMMALEOP - CMBIRTH if CMMALEOP < 999
gen agemonthvasrev = . 
replace agemonthvasrev = CMVASREV - CMBIRTH if CMVASREV < 999



// Recode variable cpmethod month to account for these sterilization dates
// in cpmethodmonth 1 = male ster, 2 = fem ster
forvalues A = 180/528 {
	replace cpmethodmonth`A' = 2 if (agemonthster <= `A' & agemonthsterrev >= `A') & agemonthster != . & (`A' <= curage)
	replace cpmethodmonth`A' = 1 if (agemonthvas <= `A'  & agemonthvasrev >= `A') & agemonthvas != .   & (`A' <= curage)
}
// correct for those who had a second sterilization after a reversal - include their 2nd ster if they're ster at interview
forvalues A = 180/528 {
	replace cpmethodmonth`A' = 2 if (agemonth2ndster <= `A' ) & inlist(FECUND, 1, 2, 3) & (`A' <= curage)
}

// gen a variable called curmethodstercor to compare to curmethod post-correction
// to ensure that only ster is getting moved around
gen curmethodstercor = .
forvalues i = 180/528 {
	replace curmethodstercor = cpmethodmonth`i' if `i' == curage
}
label values curmethodstercor cpmethod
tab curmethod curmethodstercor if curage <= 528, m

// Check against constat
tab curmethodstercor CONSTAT1 if curage <= 528, m

// There are some who report infecundity but do not report a date of sterilization
// For those who report their infecundity in the contraceptive calendar, we'll trust that they
// input their date of sterilization w/in the last 4 years correctly
// For those who *aren't* reporting that their current method is sterilization, however, 
// we'll want to flag them because we don't know when they became sterile, i.e. we don't know
// when they switched contraceptive method use and/or stopped being at risk for pregnancy.
// FLAG FOR FOLLOW-UP: What to do with those who are sterile but we don't have dates associated with their sterility?
gen infecundnosterdate = .
replace infecundnosterdate = 0 if inlist(FECUND, 1, 2, 3)
replace infecundnosterdate = 1 if (agemonthster == . & agemonthvas == .) & infecundnosterdate == 0
// get rid of the flag for those who reported their sterility in the calendar
// who we're really trying to identify here is those who are infecund through some means but *aren't* reporting it
// in the calendar, therefore we might be coding them as no contraceptive use, for example.
replace infecundnosterdate = 0 if (curmethodstercor == 1 | curmethodstercor == 2) & infecundnosterdate != .
// Also get rid of flag for those who ever report sterilization in the calendar, which 
// also offers some indication that they're considering it a method.
gen everreportster = .
forvalues i = 1/48 {
	replace everreportster = 1 if inlist(topmeth`i', 1, 2) & infecundnosterdate == 1
}
replace infecundnosterdate = 0 if everreportster == 1 & infecundnosterdate != .


// bro curmethodstercor cpmethodmonth* sexmonth* if infecundnosterdate == 1 & curmethodstercor != 1 & curmethodstercor != 2 & curage <= 528 & everreportster == 1

tab infecundnosterdate
tab infecundnosterdate if curage <= 528
tab curmethodstercor CONSTAT1 if curage <= 528 & infecundnosterdate == 0, m
tab curmethodstercor CONSTAT1 if curage <= 528 & infecundnosterdate == 1, m

list CASEID cpmethodmonth* agemonthster* agemonthvas* CONSTAT* curmethod* RSTRSTAT FECUND ///
	 infecundnosterdate curage CMTUBLIG CMHYST CMOVAREM CMOTSURG CMOPER1 nster if curmethodstercor == 18 & CONSTAT1 == 2
list METHX* cpmethodmonth* agemonthster* agemonthvas* CONSTAT* curmethod* RSTRSTAT FECUND infecundnosterdate curage if inlist(CONSTAT1, 33,34,35,36,38) & curmethodstercor != 1 & curmethodstercor != 2
cou if inlist(CONSTAT1, 33,34,35,36,38) & curmethodstercor != 1 & curmethodstercor != 2 & infecundnosterdate == 1

** N methods used in a month

// Count number of contraceptive methods used in a given month, in case multiple method use is a possible
// explanation for declining fertility rates
// This uses similar logic to identifying "topmeth" above.
// There are up to 192 methods recorded for each person-- up to 4 methods for each of
// 48 months. We're trying to identify methods within month, and therefore loop through
// months 1/48
forvalues i = 1/48 {
	gen methodsinmo`i' = .
	
	// If we're in the first month, identifying the first four methods is fairly straightforward
	if `i' ==  1 {
		// Replace methodsinmo 1 = 1 if there is any value stored in meth1, and that isn't "none"
		replace methodsinmo`i' = `i' if (meth`i' != . & meth`i' != 18)
		// If there's no meth1, or if meth1 == none, set methodsinmo1 == 0
		replace methodsinmo`i' = 0 if (meth`i' == 18 | meth`i' == . )
		
		
		// loop over possible methods 2-4, the additional methods we may have information on
		// during month 1
		forvalues j = 2/4 {
			// if method 2-4 is not missing or "none", make the number of methods in that month == 
			// 2-4
			replace methodsinmo`i' = `j' if meth`j' != . & meth`j' != 18
			
			// if the first answer given in the first reported method is none, but in a second-fourth method they
			// actually report another method, like condoms, then roll the count of methods used back by 1
			local k = `j' - 1
			replace methodsinmo`i' = `k' if meth`k'== 18 & methodsinmo`i' == `j'
		}
	}
	
	// If we're in a month that's NOT the first month, the script operates effectively similarly,
	// but we're instead talking about, for exampmle, meth5 meth6 meth7 and meth8 for month 2
	// We use local j below to index these stored methods in increments of four
	if `i' > 1 {
			local j = ((`i' - 1)) * 4 + 1
			replace methodsinmo`i' = 1 if (meth`j' != . & meth`j' != 18)
			replace methodsinmo`i' = 0 if (meth`j' == 18 | meth`j' == .)
			
			local k = `j' + 1
			forvalues l = 2/4 {
				replace methodsinmo`i' = `l' if meth`k' != .
				
				// if the first answer they give is none, but then they give another answer, like condoms
				// then roll the count of methods used back by 1
				local m = `l' - 1
				local n = `k' - 1
				replace methodsinmo`i' = `m' if meth`n' == 18 & methodsinmo`i' == `l'
				local k = `k' + 1
			}
	
	}
}

// Make sure that methodsinmo == . if there is no method recorded in that month, and also
// record 0 as number of methods for people who report "refused/don't know"
forvalues i = 1/48 {
	replace methodsinmo`i' = . if topmeth`i' == .
	replace methodsinmo`i' = . if topmeth`i' == 99
}

// for ages 15-44 match most effective method onto agemonth
forvalues i = 180/528 {
	gen nmethodsmonth`i' = .
		forvalues j = 1/48 {
			// if the agemonth that contraceptive use is recorded in (j) is the same as loop `i', replace with value of methodsinmo
			replace nmethodsmonth`i' = methodsinmo`j' if agemonthcp`j' == `i'
		}
}

// for respondents who had missing values on cpuse, if they'd never used a method (EVERUSED == 5), replace those 
// values with 0 - no changes
forvalues i = 180/528 {
	forvalues j = 1/48 {
		replace nmethodsmonth`i' = 0 if EVERUSED == 5 & agemonthcp`j' == `i' & cpmethodmonth`i' == .
	}
} 

** Drop contraceptive variables that won't be used moving forwards
drop methodsinmo? methodsinmo??


**************************
** Fertility intentions **
**************************

// These measures are only at time of interview - how many (additional) children
// do you want, and if you're not currnetly contracepting, why? 
// We're adding them in an attempt to see if fertility intentions changed across
// the 3 periods that we study. 
// Do people want fewer children during recession?
// Given that preg rates among non-users go up and down significantly across the 3 periods,
// are there differences in how many people want to become preg among cp non-users?

** Intended Parity
// Survey asks at time of interview whether R wants to have any (more) children.
// Rounds after 2006-2010 ask *when* respondent would like to have their next child (2 years, 5 years)

/* 2006-2010 Q sequence -- J denotes joint, asked among those married/cohabiting; single responses are w/o J: 
 * RWANT - do you want to have more kids
 * (J)INTEND - given that we can't always do what we want, do you intend to have a(nother) baby? 
 * (J)SUREINT - how sure are you about your intention? (very, somewhat, not at all)
 * (J)INTENDN - how many more do you intend?
 * RECODE: INTENT - intentions for additional births (0/1) 
 * Intentions are 0 if partner or respondent are sterile - RSTRSTAT != 0 | PSTRSTAT != 0 */

gen ster = 0
replace ster = 1 if RSTRSTAT != 0 & RSTRSTAT != .
replace ster = 1 if PSTRSTAT != 0 & PSTRSTAT != .

// Possible vars: INTENT, INTENDN, total intended parity (current + intended)
fre INTENT

/* My attempt to recreate the recode in order to understand INTENT 
gen intent = .
// Intend more chidlren (J)INTEND == 1
replace intent = 1 if (MARSTAT == 1 | MARSTAT == 2) & JINTEND == 1 // if mar/coh, use JINTEND
replace intent = 1 if (MARSTAT != 1 | MARSTAT != 2) &  INTEND == 1 // if single use INTEND
// Don't intend more chidlren (J)INTEND == 5, or ster, OR didn't answer intentions but
// did say you didn't WANT more children (comes before intentions in q sequence)
replace intent = 2 if (MARSTAT == 1 | MARSTAT == 2) & JINTEND == 5
replace intent = 2 if (MARSTAT != 1 | MARSTAT != 2) &  INTEND == 5
replace intent = 2 if ster == 1
replace intent = 2 if intent == . & INTEND == . & RWANT == 5
replace intent = 2 if ster == 1
// DK if want more children - (J)INTEND == 0
replace intent = 3 if (MARSTAT == 1 | MARSTAT == 2) & JINTEND == 9
replace intent = 3 if (MARSTAT != 1 | MARSTAT != 2) &  INTEND == 9
tab INTENT intent, m // close enough */


** Intended additional parity
// N of additional children intended, collapsed into one var

// In Hartnett and Gemmill 2020 for those who dk or refused, they make additional
// children equal to the average of the high and low expected future number of kids
// ADDEXP represents the "central" n of addl kids wanted, which is stated addl n in (J)INTEND
// or an average of their max/min expected kids if unknown/refused intended n of add'l
fre ADDEXP // var is coded * 10, need to divide to get it into normal count
gen addexp = ADDEXP/10 

// gen a married/cohabiting dummy for using the (J)INTEND* vars
gen marcoh = 0
replace marcoh = 1 if MARSTAT == 1 | MARSTAT == 2

gen intendaddl = .
replace intendaddl = 0 if INTENT == 2 // if intending no more children
replace intendaddl = JINTENDN if marcoh == 1 & JINTENDN != .
replace intendaddl = INTENDN  if marcoh == 0 &  INTENDN != .
replace intendaddl = addexp if INTENT == 3 & intendaddl == .   & addexp != .
replace intendaddl = addexp if (intendaddl == 98 | intendaddl == 99) 
replace intendaddl = addexp if intendaddl == .

** Intended total parity
// current number of children + current pregnancy + intendedaddl
fre PARITY
fre RCURPREG
gen curpreg = 0
replace curpreg = 1 if RCURPREG == 1
gen intendparity = PARITY + curpreg + intendaddl

// Survey asks about those who are not using contraception at the time of interview
// why they're not contracepting. Assess whether people are not contracepting in 
// order to become pregnant.

// CONSTAT1 - current contraceptive method, has a cat for "seeking preg"
// WYNOTUSE - reason not using bc you want to become pregnant?
// HPPREGQ - reason not using bc partner wants you to become pregnant?
// WHYNOUSING1-5 - reason why not using contraception, which include "wouldn't mind getting preg"

// For now, using WYNOTUSE, but other options if we want to get some more nuance or specificity

fre WYNOTUSE

fre CONSTAT1 
/* Reasons why peopel may not be using contraception:
   - never had sex
   - has ever had sex but hasn't recently (3 mo)
   - recently gave birth
   - trying to become pregnant
   - not using for other reasons
   - currently pregnant (should be censored in our coding)
*/
gen reasonfornocp = .
replace reasonfornocp = 1 if CONSTAT1 == 31 // want preg
replace reasonfornocp = 2 if CONSTAT1 == 40 // never had sex
replace reasonfornocp = 3 if CONSTAT1 == 41 // no sex w/in last 3 mo
replace reasonfornocp = 4 if CONSTAT1 == 32 // postpartum
replace reasonfornocp = 5 if CONSTAT1 == 42 // none
label define reasonfornocp 1 "Seeking pregnancy" 2 "Never had sex" 3 "No sex in last 3 mo" 4 "Postpartum" 5 "No/other reason"
label values reasonfornocp reasonfornocp

// TODO: check these
tab curmethod2 reasonfornocp, m 
// why don't we have the never had sex covered w/ none? and do they get covered when long?
// OH! We don't care about cp use for those who have never hd sex because
// we only include those who had sex in a given month in our analysis of contraceptive use
tab curmethod2 reasonfornocp


******************************
** Check for missing values **
******************************

gen misschk = 0

** Check ordering of beginning and end of each marriage, premarital cohab, and nonmarital cohab

// Marriages
summarize FMARNO
forvalues i=1/`r(max)' {
	replace misschk = 1 if FMARNO >= `i' & agem`i'   == . 						// if number of marriages >= `i' but we haven't recorded an age for marriage `i's date
	replace misschk = 1 if FMARNO >= `i' & agendm`i' == .						// if number of marriages => `i' but we haven't recorded an end for marriage i (we record CM of interview as end of marriage is ongoing)
	replace misschk = 1 if agem`i' > agendm`i' & agem`i'!=. & agendm`i'!=.		// if the age at beginning of marriage is greater than age at end of marriage
	gen rmar`i'     = 1 if agem`i' != .											// flag for each of any marriage.
}	
tab misschk
// no flags after marriages; this makes sense because the marriage data are cleaned by the NSFG.

// Checks on premarital cohabs -- not previously included
codebook CMPMCOHX
summarize PMARRNO
// premarital cohabitation index should correspond directly w/ marriage -- i.e. pm cohab 1 corresponds w/ husband 1
forvalues i = 1/`r(max)' {
	replace misschk = 1 if cohm`i' > agem`i' & cohm`i' != . 					// if the date of premarital cohab is greater than (after) marriage date, flag
	gen rcohm`i'    = 1 if cohm`i' != .											// flag for each of any premarital cohabs
}
tab misschk

// nonmarital cohab		
forvalues i = 1/5 {	
	replace misschk = 1 if cohp`i' > cohpend`i' & cohp`i' != . & cohpend`i' != .		// if age at begninng of nonmarital cohab is greater than at the end
	gen rcohp`i'    = 1 if cohp`i' != .													// flag for each of any nonmarital cohabs
	gen rcohpend`i' = 1 if cohpend`i' != .												// flag for end of each nonmarital cohab
}
tab misschk

egen nomar = rowtotal(rmar?) // summarize total of marriages
// if the number of recorded marriages w/ dates is  < NSFG recorded, replace misschk
replace misschk = 1 if nomar < FMARNO

egen nocohm     = rowtotal(rcohm?) // summarize total of marital cohabs recorded
// if the number of cohabs recorded is less than NSFG recorded, replace misschk
// these are people who don't know/are missing their cohab dates
replace misschk = 1 if nocohm < PMARRNO   							  

egen nocohp     = rowtotal(rcohp?) 		  // summarize total of nonmarital cohabs recorded
replace misschk = 1 if nocohp < NONMARR & NONMARR <= 5

egen nocohpend = rowtotal(rcohpend?)
replace misschk = 1 if nocohpend < NONMARR & NONMARR <= 5

tab1 nomar nocohm nocohp nocohpend

tab misschk

// Generate variable called "exclude" that will be used to exclude observations
// from analysis without dropping them from the data set.
gen exclude = 0

replace exclude = 1  if misschk == 1 
count if exclude != 1
global naftermisordereddatesdrop `r(N)'

** check on marriage and cohab durations **
// these should have been resolved in the dropping of the misschk
// program will break if there are negative durations

// Marriage
summarize nomar
forvalues i = 1/`r(max)' {
	gen mardur`i' = agendm`i' - agem`i'
	summarize mardur`i' if exclude == 0
	if r(N) != 0 {
		if `r(min)' < 0 {
			display as error "Negative values for marriage duration"
			exit(1)
		}
	}
}
// Premarital cohabitation 
summarize nocohm
forvalues i = 1/`r(max)' {
	gen pmcohdur`i' = agem`i' - cohm`i'
	summarize pmcohdur`i' if exclude == 0
	if r(N) != 0 {
		if `r(min)' < 0 {
			display as error "Negative values for pm cohab duration"
			exit(1)
		}
	}
}
// Non-premarital Cohabitation 
summarize nocohp
forvalues i = 1/`r(max)' {
	gen cohdur`i' = cohpend`i' - cohp`i'
	summarize cohdur`i' if exclude == 0
	if r(N) != 0 {
		if `r(min)' < 0 {
			display as error "Negative values for nm cohab duration"
			exit(1)
		}
	}
}


// cou number of ongoing pregnancies
gen ongoingpregcount = 0
summarize PREGNUM
forvalues i = 1/`r(max)' {
	replace ongoingpregcount = 1 if OUTCOM`i' == 6
}


*********************
** RESCALE WEIGHTS **
*********************

// Rescale the weights on the wide data.

// Create weights that can be used across rounds - 
// currently, all of the weights are on different
// scales so we can't just collapse them into one variable without preparing the
// variables first. Kelly suggested in a previous commit that we could scale
// the weights so that they average to 1 within each round, and then pool them.

if `wave' == 610 {
	// 0610 = WGTQ1Q16, 255620 observations
	// summarize the weight variable in order to extract the number of observations
	// within the period (`r(N)') and the sum of those observations (`r(sum)')
	// Mean = sum of all observations/number of observations
	// If we invert that ratio, we get a factor that, if we multiply it by the weight variable,
	// will rescale the weight to have a mean of 1.
	summarize WGTQ1Q16
	local rescalefactor `r(N)'/`r(sum)'
	display `rescalefactor'
	gen weight06 = .
	replace weight06 = WGTQ1Q16*`rescalefactor'
	summarize weight06
}

if `wave' == 1113 {
	// 1113 = WGT2011_2013
	summarize WGT2011_2013 
	local rescalefactor `r(N)'/`r(sum)'
	display `rescalefactor'
	gen weight11 = .
	replace weight11 = WGT2011_2013*`rescalefactor'
	summarize weight11
}

if `wave' == 1315 {
	// 1315 = WGT2013_2015
	summarize WGT2013_2015 
	local rescalefactor `r(N)'/`r(sum)'
	display `rescalefactor'
	gen weight13 = .
	replace weight13 = WGT2013_2015*`rescalefactor'
	summarize weight13
}

********************
** Save wide data **
********************

save "$NSFGKeep/`wave'cohabfertwide.dta", replace
