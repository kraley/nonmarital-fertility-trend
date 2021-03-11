* Set up the base Cohab Fertility environment.

* The current directory is assumed to be the one with the base Cohab Fertility code.

* We expect to find your setup file, named setup_<username>.do
* in the base Cohab Fertility directory.
set varabbrev off, permanent
set maxvar 10000

* Find my home directory, depending on OS.
if ("`c(os)'" == "Windows") {
    local temp_drive : env HOMEDRIVE
    local temp_dir : env HOMEPATH
    global homedir "`temp_drive'`temp_dir'"
    macro drop _temp_drive _temp_dir`
}
else {
    if ("`c(os)'" == "MacOSX") | ("`c(os)'" == "Unix") {
        global homedir : env HOME
    }
    else {
        display "Unknown operating system:  `c(os)'"
        exit
    }
}


global cohabfert_base_code "`c(pwd)'"

do setup_`c(username)'

**********************************
* Check for package dependencies *
**********************************
* This checks for packages that the user should install prior to running the project do files.

capture : which tolong
if (_rc) {
    display as error in smcl `"Please install package {it:tolong} from SSC in order to run these do-files;"' _newline ///
        `"you can do so by clicking this link: {stata "ssc install tolong":auto-install tolong}"'
    log close
    exit 199
}

capture : which rsource
if (_rc) {
    display as error in smcl `"Please install package {it:rsource} from SSC in order to run these do-files;"' _newline ///
        `"you can do so by clicking this link: {stata "ssc install rsource":auto-install rsource}"'
    log close
    exit 199
}

* Files created from original data to be used by other project members or 
* to support analyses in papers are put in the "shared" directory.
* If a file is in the shared directory, there should be code that takes us from
* an original data file to the shared data file. The name of the file with 
* that code should be the same name as the shared data file.



