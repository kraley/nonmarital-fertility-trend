local rounds 0610 1113 1315
local rawdatafiles $NSFG06_10/nsfg0610fem.dta $NSFG11_13/Female_NSFG1113.dta $NSFG13_15/2013_2015_FemRespData.dta
local rawpregfiles $NSFG06_10/2006_2010_FemPreg.dta $NSFG11_13/2011_2013_FemPreg.dta $NSFG13_15/2013_2015_FemPreg.dta
local columns B D F

forvalues i = 1/3 {
	local round:       		word `i' of `rounds'
	local rawdatafile: 		word `i' of `rawdatafiles'
	local rawpregfile: 	word `i' of `rawpregfiles'
	local column:      		word `i' of `columns'
	
	display "Recoding `round'"
	display "With `rawdatafile' and `rawpregdatafile'"
	
	// Recode raw data file
	  * The command below passes two arguments to the do widerecodes command.
	  * stata stores the first argument as local macro `1' and the second as macro `2'
	  * Thus, `round' carries to the do file as `1' and `rawdatafile' carries as `2'
	  * except that it isn't working for me. 
	do "recodes/2a.widerecodes.do" "`round'" " `rawdatafile'"
	// Read in saved wide data, reshape to long
	do "recodes/2b.reshape.do" "`round'"
	// Read in long data, recode and drop cases to generate and save analytic sample
	do "recodes/2c.longrecodes.do" "`round'"	
}
