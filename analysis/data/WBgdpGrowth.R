######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB gdp growth 
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("gdp") # 87

# Loading the data
WBgdpGrowth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBgdpGrowth, file = "analysis/data/rawdata/WBgdpGrowth.csv")

# Loading the saved data
# WBgdpGrowth <- read.csv(file = "analysis/data/rawdata/WBgdpGrowth.csv")
# WBgdpGrowth <- WBgdpGrowth[,-1]

# Renaming one variable 
names(WBgdpGrowth)[names(WBgdpGrowth) == "NY.GDP.MKTP.KD.ZG"] <- "GDPgrowth"

# Removing country name (not iso2c), 
# in order not to show up in merged date frame
WBgdpGrowth <- WBgdpGrowth[,-2]

# Getting to know the data
# lapply(WBgdpGrowth, class)

SEVEN <- WDI(indicator = "BN.KLT.PRVT.GD.ZS", country = "all", start = 1960, end = 2015)
