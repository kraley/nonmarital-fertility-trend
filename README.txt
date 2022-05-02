US nonmarital fertility trends using the National Survey of Family Growth 2006-2015

The code in this directory runs analyses to identify the sources of decline in non-marital fertility rates between 2003 and 2015 using data from the National Survey of Family Growth (NSFG).
This analysis relies on the publicly available female respondent files from the NSFG cycles 2006-2010, 2011-2013, and 2013-2015.

In order to execute this code, you will need to have downloaded the appropriate NSFG data files.

You will need to edit the setup_example.do and setup_example.R files to identify locations in your local computing environment where you have original NSFG data files stored, where you'd like your temporary data files stored, and where you would like the results to go. Save these files as setup_<username>.do or .R with <username> as the username in your computing environment. (for example, setup_kburke.do)

Open stata and check that stata is in the same directory as this file and main.do.

Run main.do

main.do should recode each cycle of NSFG data, reshape it to person-month format, and then append all person-month files to one another. It will then execute analyses and export tables and figures that are presented in our manuscript.

If you encounter issues or errors, please contact Kristen Burke at kristenlburke@utexas.edu.
