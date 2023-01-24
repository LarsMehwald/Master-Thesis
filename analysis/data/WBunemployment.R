######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB (short-term) unemployment
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("unemployment")

# Loading the data
WBunemployment <- WDI(indicator = "SL.UEM.TOTL.ZS", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBunemployment, file = "analysis/data/rawdata/WBunemployment.csv")

# Loading the data
# WBunemployment <- read.csv(file = "analysis/data/rawdata/WBunemployment.csv")
# WBunemployment <- WBunemployment[,-1]

# Renaming one variable 
names(WBunemployment)[names(WBunemployment) == "SL.UEM.TOTL.ZS"] <- "unemployment"

# Removing country name (not iso2c), 
# in order not to show up in merged date frame
WBunemployment <- WBunemployment[,-2]
