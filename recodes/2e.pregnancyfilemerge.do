// Merge pregnancy-level data into the person-month file --
// in particular, we're trying to incorporate pregnancy intentions from the
// pregnancy file into our person-month file in order to assess trends in intentions
// among those who become pregnant.
local wave `1'
di `wave'

// Read in recoded long data, in person-month format
use  "$NSFGKeep/`wave'cohabfertlongrecodes.dta", clear

// merge 1:1 on CASEID and agemonth
// matching person-months in the longrecodes data to additional information about 
// their pregnancy from the pregintentions only if the pregnancy was
// conceived in the agemonth of the person-month file
merge 1:1 CASEID agemonth using "$NSFGKeep/`wave'pregintentions.dta"

// _merge == 1 is a match, which is good
// _merge == 2 is an unmatched row from the pregnancy file, which is bad
// _merge == 3 is an unmatched row from the person-month file, which is ok (there are many months in which a pregnancy did not start)
// make sure there aren't any pregnancies that didn't merge from the
// pregnancy file-- these should all match
count if _merge == 2
assert `r(N)' == 0

// Check to see if merge is working properly
// p == pregnancy indicator, at month of conception
// for pregnancies that end in a live birth or are
// ongoing at the time o fthe interview
count if (OUTCOME == 1 & p != 1) | (OUTCOME == 6 & p != 1)
assert `r(N)' == 0

tab OUTCOME p , m
// FLAG FOR FOLLOW-UP: there are a few pregnancies that end in abortion, miscarriage,
// etc that are mapping onto p, which should only contain pregs that end in birth or
// are ongoing at the time of the interview. These are related to the duplicates from
// the pregnancy file! So, solving that problem will also solve this one :)
list dup if OUTCOME != 1 & OUTCOME != 6 & p == 1

drop _merge dup

// Replace the long recodes file with this file, which includes intentions data
save  "$NSFGKeep/`wave'cohabfertlongrecodes.dta", replace
