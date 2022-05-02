# This file sets up the base project environment. 
# It is the same for all project members

# We expect to find your setup file, named setup_<username>.do
# in the scripts/R directory.

# You should be working from the primary cohab_fertility scripts directory

username <- Sys.getenv("LOGNAME")
personal_setup <- paste0("setup_", username, ".R")

# scripts
scripts <- getwd()

source(paste0(scripts, "/", personal_setup))

# Check for necessary packages; install those that you don't have
# and load each.

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

if(!require(scales)){
  install.packages("scales")
  library(scales)
}

if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}