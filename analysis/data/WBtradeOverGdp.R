######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB tade openess
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("trade") # 108

# It is measured as imports and exports over total GDP
# http://data.worldbank.org/indicator/NE.TRD.GNFS.ZS

# Loading the data
WBtradeOverGdp <- WDI(indicator = "NE.TRD.GNFS.ZS", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBtradeOverGdp, file = "analysis/data/rawdata/WBtradeOverGdp.csv")

# Loading the saved data
# WBtradeOverGdp <- read.csv(file = "analysis/data/rawdata/WBtradeOverGdp.csv")
# WBtradeOverGdp <- WBtradeOverGdp[,-1]

# Renaming one variable 
names(WBtradeOverGdp)[names(WBtradeOverGdp) == "NE.TRD.GNFS.ZS"] <- "Trade"

# Removing country name (not iso2c), 
# in order not to show up in merged date frame
WBtradeOverGdp <- WBtradeOverGdp[,-2]
