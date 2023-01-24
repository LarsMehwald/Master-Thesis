######################################
# Master thesis
# Lars Mehwald
# Unused script: finding countries with multiple observations 
# 4 April 2016
######################################

# Aggregate data frame 
temp <- aggregate(df$year ~ df$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 57,]

# Solving South Sudan SD
summary(duplicated(df[df$iso2c == "SD","year"]))

summary(duplicated(IMFcapital[IMFcapital$iso2c == "SD","year"]))
summary(duplicated(KOFindex[KOFindex$iso2c == "SD","year"]))
summary(duplicated(PolityIV[PolityIV$iso2c == "SD","year"])) # Here is the problem 
summary(duplicated(WBgdp[WBgdp$iso2c == "SD","year"]))
summary(duplicated(WBgdpCapita[WBgdpCapita$iso2c == "SD","year"]))
summary(duplicated(WBgdpGrowth[WBgdpGrowth$iso2c == "SD","year"]))
summary(duplicated(WBinflation[WBinflation$iso2c == "SD","year"]))
summary(duplicated(WBInstitutions[WBInstitutions$iso2c == "SD","year"]))
summary(duplicated(WBtotalPopulation[WBtotalPopulation$iso2c == "SD","year"]))
summary(duplicated(WBtradeOverGdp[WBtradeOverGdp$iso2c == "SD","year"]))
summary(duplicated(WBunemployment[WBunemployment$iso2c == "SD","year"]))
summary(duplicated(WBunemploymentLong[WBunemploymentLong$iso2c == "SD","year"]))
summary(duplicated(WITS.bilateral.trade[WITS.bilateral.trade$iso2c == "SD","year"]))

# Individual data frames VN, KH, CY, SD
temp <- aggregate(IMFcapital$year ~ IMFcapital$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(KOFindex$year ~ KOFindex$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

# VN is problem
temp <- aggregate(PolityIV$year ~ PolityIV$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBgdp$year ~ WBgdp$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBgdpCapita$year ~ WBgdpCapita$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBgdpGrowth$year ~ WBgdpGrowth$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBinflation$year ~ WBinflation$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

# KH CY
temp <- aggregate(WBInstitutions$year ~ WBInstitutions$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBtotalPopulation$year ~ WBtotalPopulation$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBtradeOverGdp$year ~ WBtradeOverGdp$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBunemployment$year ~ WBunemployment$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WBunemploymentLong$year ~ WBunemploymentLong$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

temp <- aggregate(WITS.bilateral.trade$year ~ WITS.bilateral.trade$iso2c, FUN = length)
temp[,2] <- as.numeric(as.character(temp[,2]))
temp <- temp[temp[,2] > 56,]

rm(temp, temp_SD)
