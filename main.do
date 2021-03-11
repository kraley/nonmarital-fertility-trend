/* Cohabiting Fertility with the NSFG - 
 * analyzing trends and levels in cohabiting fertility relative to single and
 * married fertility using the NSFG 2006-201(5?)
 
 * Started May 2019, Kelly Raley, Kristen Burke
 * Reprise of 2013 project with Kelly Raley, Minle Xu */

clear all
set more off

** TO DO: 
// setup log


** 1) Set up environment **
// Running "setup_cohab_fertility" will load user-specific globals
// for file locations; need to have a user-specific "setup_`user'" file
do "1.setup_cohab_fertility.do"

** 2) Run recodes **
// WARNING! This takes a long time to run.
// You can comment this out when IF you've got the most recent
// version of the recoded data saved on your computer.
// But it must be run anytime the recode files change!
do "2.recodes.do"

** 3) Append files
// In order to analyze all rounds of the NSFG together, 
// we need to append the data files created from the recoding process.

do "3.append.do"

** 4) Analysis

do "4.analysis.do"

