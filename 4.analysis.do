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
// for Black women in their 20s
do "analysis/4.Table2_distribution_relstatcpuse_byage_blackwomen.do"

// Decomposition of trends by contraceptive use
// for Black women in their 20s 
do "analysis/4.Table3_decomposition_cpuse_byage_blackwomen.do"

// Appendix table 1: decomposition by relationship status
// for Black women in their 20s
do "analysis/4.Appendix_decomposition_relstat_blackwomen.do"

*************
** FIGURES **
*************

// Figures are generated in R
do "analysis/FiguresinR.do"

// Table of trends by period, which also assess statistical significance in changes
// between periods - used to accompany Figure 2 with values reported in text.
do "analysis/4.TableforFig2_pregratesbycp_blackwomen.do"


**************************
** SENSITIVITY ANALYSES **
**************************

// With only 1 year window of contraceptive and sexual activity reporting
// Relationship status trends
do "analysis/4.sensitivity_time_decomposition_relstat_blackwomen.do"
// Contraceptive trends
do "analysis/4.sensitivity_time_decomposition_cpuse_blackwomen.do"

// Multiple method use
do "analysis/4.sensitivity_nmethods_decomposition_cpuse_blackwomen.do"