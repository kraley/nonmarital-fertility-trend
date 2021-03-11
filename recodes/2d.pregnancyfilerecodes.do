// Pregnancy file recodes
// Pregnancy intentions are stored in the pregnancy file, which lists one record per pregnancy
// This script extracts pregnancy intention data from the pregnancy file, checks the coding,
// and prepares it for merging by CASEID and agemonth to the person-month, which serve as
// the main analysis files for this project.

// use "$NSFG06_10/2006_2010_FemPreg.dta", clear
// use "$NSFG11_13/2011_2013_FemPreg.dta", clear
// use "$NSFG13_15/2013_2015_FemPreg", clear
macro list
local wave `1'
di `wave'
local filepath "`2'"
use `filepath', clear

keep CASEID PREGORDR DATECON DATEND OUTCOME CMBIRTH MULTBRTH ///
     STOPDUSE WHYSTOPD RESNOUSE WANTBOLD WANTBLD2 PROBBABE CNFRMNO TIMINGOK TOOSOON_N TOOSOON_P ///
	 WANTRESP OLDWANTR WANTRESP_I
	 
**************************	 
** PREGNANCY INTENTIONS **
**************************	
 
// Pregnancy intention question sequence
// STOPDUSE - had you stopped using contraception prior to preg?
// WHYSTOPD - was the reason you stopped because you wanted to become pregnant?
// RESNOUSE [for those not using cp prior to preg] -  Was the reason you did not use bc bcause you wanted to become pregnant?

// if NO to WHYSTOPD or RESNOUSE
// WANTBOLD - did you want a[nother] baby any time int he future?
// PROBBABE [if dk to WANTBOLD] - it's sometimes hard to remember, but did you prob want a[nother] baby? 

// if NO to WANTBOLD or PROBBABE and younger than 20
// CNFRMNO - confirm you did not want a[nother] baby in future
// WANTBLD2 - circle back, did you want a[nother] baby in future?

// if stopped using bc wanted preg (STOPEDUSE) or wasn't using bc wanted preg (WHYSTOPD), or if she wanted a preg
// at some point in the future (WANTBOLD & PROBBABE)
// TIMINGOK - became preg too soon, right time, or later than wanted?
// TOOSOON_N [if too soon] - how much sooner than you wanted?
// TOOSOON_P [if too soon] - is TOOSOON_N recorded in months or years?

// WANTRESP and OLDWANTR are NSFG recodes of pregnancy intentions
// WANTRESP vs OLDWANTR - a shift in order of recode; results in a very slight difference in mistimed vs unwanted
// because it uses the CNFRMNO question among those under 20 years old. 
// Will use WANTRESP

// Replication of NSFG recode WANTRESP
gen wantedness = .
replace wantedness = 1 if TIMINGOK == 3 // later than wanted
replace wantedness = 2 if TIMINGOK == 2 // right time
replace wantedness = 3 if TIMINGOK == 1 // too soon
replace wantedness = 4 if TIMINGOK == 4 // don't care
replace wantedness = 5 if (WANTBOLD == 5 | PROBBABE == 5) & CNFRMNO != 2 // didn't want preg
replace wantedness = 6 if PROBBABE == 6 | PROBBABE == 9 | WANTBLD2 == 6  // dk

tab WANTRESP wantedness, m
// Some missing values, because they were imputed by the NCHS.
tab WANTRESP_I wantedness, m
// Based on this, feel confident in WANTRESP recode.

drop wantedness STOPDUSE WHYSTOPD RESNOUSE WANTBOLD WANTBLD2 PROBBABE CNFRMNO TIMINGOK TOOSOON_N TOOSOON_P OLDWANTR

// 3 cat - intended, mistimed, unwanted
gen intentioncat3 = .
replace intentioncat3 = 3 if WANTRESP == 5 // unwanted
replace intentioncat3 = 2 if WANTRESP == 3 // mistimed
replace intentioncat3 = 1 if inlist(WANTRESP, 1, 2, 4, 6) // intended, incl indifferent/dk
label define intentioncat3 1 "Intended" 2 "Mistimed" 3 "Unwanted"
label values intentioncat3 intentioncat3

// intended/unintended -- collapsing mistimed and unwanted
gen intention = .
replace intention = 1 if inlist(WANTRESP, 1, 2, 4, 6) // intended, incl indifferent/dk
replace intention = 2 if WANTRESP == 3 | WANTRESP == 5
label define intention 1 "Intended" 2 "Unintended"
label values intention intention

***************
** AGE MONTH **
***************

// Will use age in months at conception to merge pregnancies into our person-month file
gen agemonth = DATECON - CMBIRTH

sort CASEID agemonth
by CASEID agemonth:  gen dup = cond(_N==1,0,_n)

list CASEID PREGORDR OUTCOME WANTRESP MULTBRTH DATECON DATEND if dup != 0 
// Flag for follow-up -- right now, there are some cases where
// there are multiple pregnancies that are reported to have been conceived in the same
// month. Surprisingly, almost none of them are repored as multiple births? 
// I am still puzzling through what exactly these are. Some look like aborted twins, 
// others maybe like selective reduction? N is small, relatively -- in 0610, 76/20,000
// The concern is that many have different intentions depending on the pregnancy, even though
// they were conceived at the same time. Other papers suggest dropping multiple births, but this is 
// not a multiple birth problem, it seems.
// So the follow up is to make sure that, if one of the two+ pregnancies ends in a live birth, 
// we include the intention for that pregnancy, and not the intention for the pregnancy that ends. 
// Alternatively, potentially just dropping these from the analysis altogether.
drop if dup > 1


// Keep only vars of interest
keep CASEID PREGORDR OUTCOME MULTBRTH WANTRESP intentioncat3 intention agemonth dup

// Drop pregnancies that occur outside of the NSFG age range of 15-44
drop if agemonth < 180 | agemonth > 528

// Count the number of pregnancies and store them in a global;
// this will be used to check that they all there on the merge.
count
global pregfilecount `r(N)'

save "$NSFGKeep/`wave'pregintentions.dta", replace
