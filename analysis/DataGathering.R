######################################
# Master thesis
# Lars Mehwald
# Data gathering 
# 4 April 2016
######################################

######################################
# Loading required data sets
######################################

source("analysis/data/PolityIV.R")
source("analysis/data/WBInstitutions.R")
source("analysis/data/WBgdpGrowth.R")
source("analysis/data/WBunemploymentLong.R")
source("analysis/data/WBunemployment.R")
source("analysis/data/WBinflation.R")
source("analysis/data/KOFindex.R")
source("analysis/data/WBgdpCapita.R")
source("analysis/data/WBtradeOverGdp.R")
source("analysis/data/WBtotalPopulation.R")
source("analysis/data/IMFcapital.R")
source("analysis/data/WBgdp.R")
source("analysis/data/WITSbilateralTrade.R")
source("analysis/data/FHdemocracy.R")
source("analysis/data/HallinMedia.R")
source("analysis/data/OECDunemployment.R")

######################################
# Data merging
######################################

df.1 <- merge(WBInstitutions, WBgdpGrowth, by=c("iso2c","year"), all = TRUE) 
df.2 <- merge(df.1, WBunemployment, by=c("iso2c","year"), all = TRUE) 
df.3 <- merge(df.2, WBunemploymentLong, by=c("iso2c","year"), all = TRUE) 
df.4 <- merge(df.3, WBinflation, by=c("iso2c","year"), all = TRUE) 
df.5 <- merge(df.4, PolityIV, by=c("iso2c","year"), all = TRUE) 
df.6 <- merge(df.5, KOFindex, by=c("iso2c","year"), all = TRUE) 
df.7 <- merge(df.6, WBgdpCapita, by=c("iso2c","year"), all = TRUE) 
df.8 <- merge(df.7, WBtradeOverGdp, by=c("iso2c","year"), all = TRUE) 
df.9 <- merge(df.8, WBtotalPopulation, by=c("iso2c","year"), all = TRUE) 
df.10 <- merge(df.9, IMFcapital, by=c("iso2c","year"), all = TRUE) 
df.11 <- merge(df.10, WBgdp, by=c("iso2c","year"), all = TRUE) 
df.12 <- merge(df.11, WITS.bilateral.trade, by=c("iso2c","year"), all = TRUE) 
df.13 <- merge(df.12, FHdemocracy, by=c("iso2c","year"), all.x = TRUE, all.y = FALSE) 
df.14 <- merge(df.13, HallinMedia, by= "iso2c", all.x = TRUE, all.y = FALSE) 
df.15 <- merge(df.14, OECDue, by= c("iso2c", "year"), all.x = TRUE, all.y = FALSE) 

# Removing redundant data frames
rm(df.1, df.2, df.3, df.4, df.5, df.6, df.7, df.8, df.9, df.10, df.11, df.12, df.13, df.14) 

# Renaming data frame
df <- df.15
rm(df.15)

# Delete regional observations (from World Bank)
df <- df[df$iso2c != "1A" & df$iso2c != "1W" & df$iso2c != "4E" &
           df$iso2c != "7E" & df$iso2c != "8S" & df$iso2c != "B8" &
           df$iso2c != "F1" & df$iso2c != "S1" & df$iso2c != "S2" &
           df$iso2c != "S3" & df$iso2c != "S4" & df$iso2c != "Z4" &
           df$iso2c != "Z7" & df$iso2c != "CS" & df$iso2c != "DD" &
           df$iso2c != "EU" & df$iso2c != "JG" & df$iso2c != "OE" &
           df$iso2c != "TW" & df$iso2c != "XC" & df$iso2c != "XD" &
           df$iso2c != "XE" & df$iso2c != "XJ" & df$iso2c != "XK" &
           df$iso2c != "XL" & df$iso2c != "XM" & df$iso2c != "XN" &
           df$iso2c != "XO" & df$iso2c != "XP" & df$iso2c != "XQ" &
           df$iso2c != "XR" & df$iso2c != "XS" & df$iso2c != "XT" &
           df$iso2c != "XU" & df$iso2c != "XY" & df$iso2c != "YD" &
           df$iso2c != "YU" & df$iso2c != "ZF" & df$iso2c != "ZG" &
           df$iso2c != "ZJ" & df$iso2c != "ZQ" , ]

# Saving gathered data
df.save.gather <- df
