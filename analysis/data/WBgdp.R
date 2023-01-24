######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB gdp  
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("gdp") # 82 

# Loading the data
WBgdp <- WDI(indicator = "NY.GDP.MKTP.CD", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBgdp, file = "analysis/data/rawdata/WBgdp.csv")

# Loading the saved data
# WBgdp <- read.csv(file = "analysis/data/rawdata/WBgdp.csv")
# WBgdp <- WBgdp[,-1]

# Converting GDP into million USD
WBgdp$NY.GDP.MKTP.CD <- WBgdp$NY.GDP.MKTP.CD / 1000000

# Renaming one variable 
names(WBgdp)[names(WBgdp) == "NY.GDP.MKTP.CD"] <- "GDPinCurrentMioUSD"

# Removing country name (not iso2c), 
WBgdp <- WBgdp[,-2]
