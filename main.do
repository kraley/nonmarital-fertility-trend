/* Cohabiting Fertility with the NSFG - 
 * analyzing trends and levels in cohabiting fertility relative to single and
 * married fertility using the NSFG 2006-2015
 
 * Kelly Raley, Kristen Burke
 * Accepted to Population Research and Policy Review April 2022 */

clear all
set more off


** 1) Set up environment **
// Running "setup_cohab_fertility" will load user-specific globals
// for file locations; need to have a user-specific "setup_`user'" file
do "1.setup_cohab_fertility.do"

** 2) Run recodes **
// WARNING! This takes a long time to run.
// You can comment this out IF you've got the most recent
// version of the recoded data saved on your computer.
// But it must be run anytime the recode files change!
do "2.recodes.do"

** 3) Append files
// In order to analyze all rounds of the NSFG together, 
// we need to append the data files created from the recoding process.

do "3.append.do"

** 4) Analysis

do "4.analysis.do"

