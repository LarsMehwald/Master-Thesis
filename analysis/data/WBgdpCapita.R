######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB gdp capita 
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("gdp") # 94

# Loading the data
WBgdpCapita <- WDI(indicator = "NY.GDP.PCAP.KD", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBgdpCapita, file = "analysis/data/rawdata/WBgdpCapita.csv")

# Loading the saved data
# WBgdpCapita <- read.csv(file = "analysis/data/rawdata/WBgdpCapita.csv")
# WBgdpCapita <- WBgdpCapita[,-1]

# Renaming one variable 
names(WBgdpCapita)[names(WBgdpCapita) == "NY.GDP.PCAP.KD"] <- "GDPperCapita"

# Removing country name (not iso2c), 
# in order not to show up in merged date frame
WBgdpCapita <- WBgdpCapita[,-2]
