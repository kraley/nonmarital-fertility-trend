local wave `1'

use "$NSFGKeep/`wave'cohabfertlong.dta", clear

label variable p "whether R became pregnant in this month according to DATCONX"

by CASEID, sort: gen nvals = _n == 1 
count if ongoingpregcount == 1 & nvals == 1 
global ongoingpregsbeforedrop `r(N)'

** Number of individuals observed
count if nvals == 1 
global nafterreshape `r(N)'
describe
global totalpm `r(N)'
display `totalpm'

*************
** RECODES **
*************

gen year = 1900 + floor((cmyear-1)/12)

// Gen marital status within the person-month, w singles broken out by sexual activity
// and married & cohabiting person-months without sexual activity or with unknown sexual
// activity coded as missing.
// Note: This is a change from previous iterations of the analysis in which we included
// all cohabiting and married person-months regardless of sexual activity.

// recode sex months with "don't know" to missing
recode sexmonth 9 = . 

tab mc

gen marst = mc
replace marst = . if mc == 1 & (sexmonth == 0 | sexmonth == .) 	// married person months w/o sexual activity or w/ unknown sexual activity
replace marst = . if mc == 2 & (sexmonth == 0 | sexmonth == .) 	// cohabiting person months w/o sexual activity or w/ unknown sexual activity
replace marst = 3 if mc == 0 &  sexmonth == 1 					// single perons-months in which there is sexual activity
replace marst = 4 if mc == 0 &  sexmonth == 0 					// single person-months in which there is no sexual activity
replace marst = . if mc == 0 &  sexmonth == . 					// single person months with months they "don't know" re sex

label define marst 1 "Married" 2 "Cohabiting" 3 "Single, sexually active" 4 "Single, not active"
label values marst marst

tab marst

// Gen a "not married status" variable, which will be used for analysis focused on the
// not married person-months.
gen nmarsta = .
replace nmarsta = 1 if marst == 2
replace nmarsta = 2 if marst == 3 
replace nmarsta = 3 if marst == 4
label define nmarsta 1 "Cohabiting" 2 "Single, sexually active" 3 "Single, not active"
label values nmarsta nmarsta

// gen notmarried flag
gen notmarried = .
replace notmarried = 1 if marst != 1 & marst != .


** Cp method month
// if EVERUSED == 5, cp method is always none if there is sexual activity recorded in the month
// i.e., if a respondent reports that they have NEVER used contraception, but have
// reported sexual activity, record no method in both contraception use and in number
// of methods used
replace cpmethodmonth = 18 if EVERUSED == 5 & sexmonth != .
replace nmethodsmonth = 0  if EVERUSED == 5 & sexmonth != .

// collapse cpmethodmonth into smaller categories
gen cpmethod = .
replace cpmethod = 1 if cpmethodmonth == 1  | cpmethodmonth == 2 				// permanent
replace cpmethod = 2 if cpmethodmonth == 3  | cpmethodmonth == 4 				// LARC
replace cpmethod = 3 if cpmethodmonth == 5  | cpmethodmonth == 6 | ///
					    cpmethodmonth == 7  | cpmethodmonth == 8 				// hormonal
replace cpmethod = 4 if cpmethodmonth == 10 | cpmethodmonth == 14				// condoms/wdrawal
replace cpmethod = 5 if cpmethodmonth == 9  | cpmethodmonth == 11 | ///
						cpmethodmonth == 12 | cpmethodmonth == 13 | ///
						cpmethodmonth == 15 | cpmethodmonth == 16 | ///
						cpmethodmonth == 17										// other-- EC, natural, sponge, diaphragm, fem condoms, sptermicide, other
replace cpmethod = 6 if cpmethodmonth == 18										// none					

label define cpmethodcollapsed 1 "Permanent" 2 "LARC" 3 "Hormonal" 4 "Condoms/withdrawal" 5 "Other" 6 "None"
label values cpmethod cpmethodcollapsed	

gen cpmethodcat5 = cpmethod
replace cpmethodcat5 = 4 if cpmethod == 5
replace cpmethodcat5 = 5 if cpmethod == 6
label define cpmethodcat5 1 "Permanent" 2 "LARC" 3 "Hormonal" 4 "Condoms/withdrawal/other" 5 "None"
label values cpmethodcat5 cpmethodcat5


************************
** Exclusion criteria **
************************

** Sexual Activity
// Sexual activity data are only collected for up to 48 months
// prior to interview; drop all person-months in which there
// are no data on sexual activity
replace exclude = 1 if sexmonth == .
drop nvals 
by CASEID, sort: gen nvals = _n == 1 
count if nvals == 1
global naftermissingsexdrops `r(N)'
describe 
global pmaftersexdrops `r(N)'

// Drop months in which there is no sexual activity among married and cohabiting couples, and
// drop months in which sexual activity is unknown among single people
replace exclude = 1 if marst == .

tab sexmonth if exclude == 0, m
drop nvals
by CASEID, sort: gen nvals = _n == 1 
count if nvals == 1 // drop to 8126
global naftersexmodrops `r(N)'
describe
global pmaftersexmodrops `r(N)'

** Contraceptive use
// If there is sexual activity, but unknown/refused contraceptive use, remove from analysis
replace exclude = 1 if cpmethodmonth == 99 & sexmonth == 1

** Contraceptive calendar
// Exclude person-months that are further back in time than cp calendar 
// these are likely already excluded based on sexmonth data, but double checking here
replace exclude = 1 if cmyear < CMJAN3YR
// also make sure we're only including observations in the period of interest
replace exclude = 1 if year < 2003 
replace exclude = 1 if year > 2015 & year < .

** Infecundity
// There are some people who are infecund but do not report a date for sterilization
// We'll exclude them from the analysis because we don't know when they stopped
// being able to concieve and therefore likely have their contraceptive history miscoded.
replace exclude = 1 if infecundnosterdate == 1

** Age
// Keep data for person months bewteen ages 15-29
replace exclude = 1 if (agemonth <= 180 | agemonth >= 360)
drop nvals 
by CASEID, sort: gen nvals = _n == 1 
count if nvals == 1
global nafteragedrops `r(N)'
describe
global pmafteragedrops `r(N)'

** Nonmarital cohabitation
// If there are greater than 4 non-marital cohabs,
// exclude person months after cohab 4 ends if there are more than 5 nonmarital cohabs (NONMARR >= 5) 
replace exclude = 1 if (cohpend4 < agemonth) & (NONMARR > 5 & NONMARR < .)
// If there are exactly 5 non-marital cohabs, exclude person-months after the 4th if the 5th 
// cohab is not ongoing (cohp5 == .)
replace exclude = 1 if (cohpend4 < agemonth) & (NONMARR == 5 & cohp5 == .)

** Marital status
// exclude all married person-months
replace exclude = 1 if mc == 1

** Immigration status
// sensitivity analysis; results do not change substantively if foreign-born women are excluded
// To assess whether immigrant women are contributing to the trends
// that we're observing, we'll re-run the analysis with them excluded 
// and compare the results
// replace exclude = 1 if BRNOUT == 1


** Drop the 3 months prior to interview 
// We consider whether or not to include current pregnancies in the calculation of
// pregnancy rates, as they are *not yet* live births.
// We (for now) have decided to consider current pregnancies equivalent to live births IF
// the pregnancy has lasted for at least 3 months/12 weeks, based on the assumptions that
// a pregnancy that is >= 12 weeks and is reported will likely not result in an abortion 
// or a miscarriage. 
// In order to do this, we drop all observations that are within 3 months of the interview,
// so that pregnancies that started within 3 mo of interview (i.e. were not yet 12 weeks
// by the time they were reported) are not considered in this analysis, nor are
// the person-months in the 3 months leading up to interivew in which women are not 
// pregnant included in pregnancy rate calculations
gen mobeforeinterview = CMINTVW - cmyear
tab mobeforeinterview // up to 47, which makes sense-- we only have sexual activity data up to 48 mo prior

// This could be 2 or 3 months, depending on whether you're including or excluding month of interview
// the more conservative route is 3
replace exclude = 1 if mobeforeinterview <= 3
//drop mobeforeinterview

// count pregs again after drop
count if p == 1
local pregcount_ongoing `r(N)'
// count if rrpm == 1
// local pregcount_woongoing `r(N)'
// display `pregcount_ongoing' - `pregcount_woongoing' // ongoing pregnancies that are counted in p
drop nvals 
by CASEID, sort: gen nvals = _n == 1 
count if nvals == 1
global naftermostrecent3modrops `r(N)'
describe
global pmaftermostrecent3modrops `r(N)'

count if ongoingpregcount == 1 & nvals == 1 
global ongoingpregsafter3modrop `r(N)'


** Drop person-months during ongoing pregnancies
replace exclude = 1 if censorpreg == 1


*********************************
** Demographic characteristics **
*********************************
 
// Generate dummy variables for demographic characteristics
// that will be included in regressions

** AGE
// Age1 - categorical variable
gen age1 = .
replace age1 = 1 if agemonth >= 180 & agemonth < 240
replace age1 = 2 if agemonth >= 240 & agemonth < 300
replace age1 = 3 if agemonth >= 300 & agemonth < 360
label define age1 1 "15-19" 2 "20-24" 3 "25-29"
label values age1 age1
tab age1

gen agecat2 = 1 if age1 == 1 
replace agecat2 = 2 if age1 ==2 | age1 == 3
label define agecat2 1 "15-19" 2 "20-29"
label values agecat2

// agegroup - dummy variables
tab age1, gen (agegroup)
rename agegroup1 age_15_19
rename agegroup2 age_20_24
rename agegroup3 age_25_29
tab1 age_15_19 age_20_24 age_25_29

** MARITAL STATUS
// marst - categorical variable
tab marst
tab marst, nolabel

// rmarst - dummy variables
tab marst,gen(rmarst)
rename rmarst1 married
rename rmarst2 cohabiting
rename rmarst3 single_not_active
rename rmarst4 single_active

** RACE
// race1 - categorical variable
// raceg - dummy variables
tab HISPRACE,gen(race)
rename race1 hispanic
rename race2 white
rename race3 black
rename race4 others

** MOTHER's EDUCATION
// EDUCMOM - categorical variable
tab EDUCMOM
tab EDUCMOM,nolabel

// meduc - dummy variables
tab EDUCMOM,gen(meduc)
rename meduc1 less_than_hs
rename meduc2 hs
rename meduc3 some_college
rename meduc4 college
rename meduc5 no_mother_figure

** INTERACTIONS **

// Race and marital status
gen black_married    = black    * married
gen hispanic_married = hispanic * married
gen others_married   = others   * married

gen black_single_notact    = black    * single_not_active
gen hispanic_single_notact = hispanic * single_not_active
gen others_single_notact   = others   * single_not_active

gen black_single_act       = black    * single_active
gen hispanic_single_act    = hispanic * single_active
gen others_single_act      = others   * single_active

// Mother's education and marital status
gen less_than_hs_married     = less_than_hs     * married
gen some_college_married     = some_college     * married
gen college_married          = college          * married
gen no_mother_figure_married = no_mother_figure * married

gen less_than_hs_single_notact = less_than_hs     * single_not_active
gen some_college_single_notact = some_college     * single_not_active
gen college_single_notact      = college          * single_not_active
gen no_mother_single_notact    = no_mother_figure * single_not_active

gen less_than_hs_single_act = less_than_hs     * single_active
gen some_college_single_act = some_college     * single_active
gen college_single_act      = college          * single_active
gen no_mother_single_act    = no_mother_figure * single_active

// Age and marital status
gen married_age20_24 = married * age_20_24
gen married_age25_29 = married * age_25_29

gen single_notact_age20_24 = single_not_active * age_20_24
gen single_notact_age25_29 = single_not_active * age_25_29

gen single_act_age20_24 = single_active * age_20_24
gen single_act_age25_29 = single_active * age_25_29

// Save recoded and long file; this will be used as the analytic data
save "$NSFGKeep/`wave'cohabfertlongrecodes.dta", replace
