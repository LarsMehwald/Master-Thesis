######################################
# Master thesis
# Lars Mehwald
# Data gathering: OECD unemployment
# 4 April 2016
######################################

# API with OECD package is not working

# Hence manually downloaded from the website and then loaded into R 
# https://data.oecd.org/unemp/unemployment-rate.htm

# Load the data
OECDue <- read.csv("analysis/data/rawdata/OECDunemployment.csv")

# Getting to know the variables
# sapply(OECDue[,c(1:5, 7)], unique)

# Delete observations for monthly and quarterly variables 
OECDue <- OECDue[OECDue$FREQUENCY == "A",] # A for annual data

# Change country coding
OECDue$iso2c <- countrycode(OECDue$ï..LOCATION,
                                      origin = "iso3c",
                                      destination = "iso2c",
                                      warn = TRUE)

# Delete (now) redundant variables
OECDue <- OECDue[, names(OECDue) != "MEASURE"]
OECDue <- OECDue[, names(OECDue) != "SUBJECT"]
OECDue <- OECDue[, names(OECDue) != "INDICATOR"]
OECDue <- OECDue[, names(OECDue) != "FREQUENCY"]
OECDue <- OECDue[, names(OECDue) != "Flag.Codes"]
OECDue <- OECDue[, names(OECDue) != "ï..LOCATION"]

# Rename variables 
names(OECDue)[names(OECDue) == "TIME"] <- "year"
names(OECDue)[names(OECDue) == "Value"] <- "OECD.unemployment"

# Change the class of year
OECDue$year <- as.numeric(as.character(OECDue$year))
