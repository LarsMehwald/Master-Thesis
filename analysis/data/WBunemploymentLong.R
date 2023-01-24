######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB long-term unemployment
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("unemployment")

# Loading the data
WBunemploymentLong <- WDI(indicator = "SL.UEM.LTRM.ZS", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBunemploymentLong, file = "analysis/data/rawdata/WBunemploymentLong.csv")

# Loading the saved data
# WBunemploymentLong <- read.csv(file = "analysis/data/rawdata/WBunemploymentLong.csv")
# WBunemploymentLong <- WBunemploymentLong[,-1]

# Renaming one variable 
names(WBunemploymentLong)[names(WBunemploymentLong) == "SL.UEM.LTRM.ZS"] <- "unemployment.long"

# Removing country name (not iso2c), 
# in order not to show up in merged date frame
WBunemploymentLong <- WBunemploymentLong[,-2]
