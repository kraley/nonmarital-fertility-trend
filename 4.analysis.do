** Analysis scripts for non-marital fertility analysis in the NSFG

********
** Ns **
********

// counts of Ns for manuscript text
do "analysis/4.sample_counts.do"

************
** TABLES **
************
// For manuscript

// Describe nonmarital fertility rates by age, race, period
do "analysis/4.Table1_descriptive.do"

// Distribution of person-months by relationship status and contraceptive use
// separately for teens (table 2) and twenties (table 3)
do "analysis/4.Table2and3_distribution_relstatcpuse_byage_blackandhispanic.do"

// Accompanying table of trends by period, which also assess statistical significance in changes
// between periods
do "analysis/4.Table4_pregratesbycp_blackhispanic.do"

// Appendix tables decomposing trends based on cp use separately for teens and twenties
do "analysis/4.Appendix_decomposition_cpuse_byage_blackhispanic.do"

*************
** FIGURES **
*************

// Figures are generated in R
do "analysis/FiguresinR.do"

*******************
** DECOMPOSITION **
*******************
// For description in text, decomposition focused on black and hispanic women

// Decompose changes due to changes in relationship status
do "analysis/4.decomposition_relstat_byage_blackhispanic.do"


**************************
** SENSITIVITY ANALYSES **
**************************

// Without immigrant women
// Relationship status
do "analysis/4.decomposition_relstat_byage_blackhispanic_immigrantsensitivity.do"
// Contraceptive trends
do "analysis/4.decomposition_cpuse_byage_blackhispanic_immigrantsensitivity.do"

** Separately for black and hispanic women
// Relationship status trends - file only decomposes periods 1 v 2
do "analysis/4.decomposition_relstat_byage_byrace.do"
// Contraceptive trends
// Does full decomp in stata window, only exports periods 1 v 2
do "analysis/4.decomposition_cpuse_byage_byrace.do"

** With only 1 year window of contraceptive and sexual activity reporting
// Relationship status trends
do "analysis/4.decomposition_cpuse_byage_blackhispanic_timesensitivity.do"
// Contraceptive trends
do "analysis/4.decomposition_cpuse_byage_blackhispanic_timesensitivity.do"
