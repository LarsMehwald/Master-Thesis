######################################
# Master thesis
# Lars Mehwald
# Data gathering: Language
# 4 April 2016
######################################

# http://www.systemicpeace.org/inscrdata.html

# Loading in the data - takes a very long time (2h)
URL <- "http://www.systemicpeace.org/inscr/p4v2014.xls"
PolityIV <- source_XlsxData(URL, header=TRUE, colIndex=c(1:21), rowIndex=c(1:16895))
rm(URL)

# Saving the raw data
# write.csv(PolityIV, file = "analysis/data/rawdata/PolityIV.csv")

# Loading the raw data to save time
# PolityIV <- read.csv(file = "analysis/data/rawdata/PolityIV.csv")
# PolityIV <- PolityIV[,-1]

# Remove observations/states that do not exist any more 
PolityIV <- PolityIV[PolityIV$country != "Baden",]
PolityIV <- PolityIV[PolityIV$country != "Bavaria",]
PolityIV <- PolityIV[PolityIV$country != "Modena",]
PolityIV <- PolityIV[PolityIV$country != "Orange Free State",]
PolityIV <- PolityIV[PolityIV$country != "Parma",]
PolityIV <- PolityIV[PolityIV$country != "Prussia",]
PolityIV <- PolityIV[PolityIV$country != "Sardinia",]
PolityIV <- PolityIV[PolityIV$country != "Saxony",]
PolityIV <- PolityIV[PolityIV$country != "Serbia and Montenegro",]
PolityIV <- PolityIV[PolityIV$country != "Tuscany",]
PolityIV <- PolityIV[PolityIV$country != "Two Sicilies",]
PolityIV <- PolityIV[PolityIV$country != "United Province CA",]
PolityIV <- PolityIV[PolityIV$country != "Wuerttemburg",]
PolityIV <- PolityIV[PolityIV$country != "Yemen North",]

# Probelmatic: Ethiopia in 1993 is recoreded twice (difference in polity2)
PolityIV <- PolityIV[PolityIV$country != "Ethiopia" | PolityIV$year != "1993" | PolityIV$polity2 != "1",]

# Problematic: Kosovo, there seems to be no iso2c country code 
# Temporary solution: exclusion
PolityIV <- PolityIV[PolityIV$country != "Kosovo",]

# Peoples Republic of Germany is causing problems 1990 (two observations),
# hence former East Germany is dropped from data frame -
# also low PolityIV score (=0)
# Also the coding of West Germany (1945-1990) and Germany 
# (1868-1945 and 1990-2014) is causing problems 
PolityIV <- PolityIV[PolityIV$country != "Germany East",]
PolityIV <- PolityIV[PolityIV$country != "Germany" | 
                       PolityIV$year != "1990",]

# Problem: until 1976 Vietnam North exists parallel to Vietnam South
# Solution: dropping Vietnamn North and coding Vietnam South as Vietnam
PolityIV <- PolityIV[PolityIV$country != "Vietnam North",]
PolityIV$country <- gsub("Vietnam South", "Vietnam", PolityIV$country)

# Problem with South Sudan, all three overlap in 2011
# Solution: Dropping South Sudan and changing (North) Sudan to Sudan
PolityIV <- PolityIV[PolityIV$country != "South Sudan",]
PolityIV <- PolityIV[PolityIV$country != "Sudan-North" | PolityIV$year != 2011,]
PolityIV$country <- gsub("Sudan-North", "Sudan", PolityIV$country)

# Changing the coding of the country names
PolityIV$iso2c <- countrycode(PolityIV$country, 
                              origin = "country.name",
                              destination = "iso2c", 
                              warn = TRUE)

# Keeping the relevant variables
PolityIV <- PolityIV[, names(PolityIV) == "iso2c" | 
                       names(PolityIV) == "year" | 
                       names(PolityIV) == "democ"]

# Rename key variable
names(PolityIV)[names(PolityIV) == "democ"] <- "polity.iv.democ"

# Changing class of year variable
PolityIV$year <- as.numeric(PolityIV$year)

# Getting rid of observations prior to 1960 
PolityIV <- PolityIV[PolityIV$year >= 1960,]
