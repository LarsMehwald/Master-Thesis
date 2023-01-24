######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB inflation
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("inflation")

# Loading the data
WBinflation <- WDI(indicator = "NY.GDP.DEFL.KD.ZG", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBinflation, file = "analysis/data/rawdata/WBinflation.csv")

# Loading the saved data
# WBinflation <- read.csv(file = "analysis/data/rawdata/WBinflation.csv")
# WBinflation <- WBinflation[,-1]

# Renaming one variable 
names(WBinflation)[names(WBinflation) == "NY.GDP.DEFL.KD.ZG"] <- "inflation"

# Removing country name (not iso2c), 
# in order not to show up in merged date frame
WBinflation <- WBinflation[,-2]
