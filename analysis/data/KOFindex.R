######################################
# Master thesis
# Lars Mehwald
# Data gathering: KOF index globalization
# 4 April 2016
######################################

# Loading in the data - takes a very long time (2h)
URL <- "http://globalization.kof.ethz.ch/media/filer_public/2015/03/04/globalization_2015_long.xls"
KOFindex <- source_XlsxData(URL, sheet = 2, header=TRUE, colIndex=c(1:12), rowIndex=c(2:8903), sha1 = "941bd9d7e1864ed2f390c2632e3d5dbc570ecf3b")
rm(URL)

# Saving the raw data
# write.csv(KOFindex, file = "analysis/data/rawdata/KOFindex.csv")

# Loading the raw data to save time
# KOFindex <- read.csv(file = "analysis/data/rawdata/KOFindex.csv")
# KOFindex <- KOFindex[,-1]

# Coding of NAs
KOFindex[,4] <- as.character(KOFindex[,4])
KOFindex[,4][KOFindex[,4] == "."] <- NA
KOFindex[,4] <- as.numeric(KOFindex[,4])

KOFindex[,5] <- as.character(KOFindex[,5])
KOFindex[,5][KOFindex[,5] == "."] <- NA
KOFindex[,5] <- as.numeric(KOFindex[,5])

KOFindex[,6] <- as.character(KOFindex[,6])
KOFindex[,6][KOFindex[,6] == "."] <- NA
KOFindex[,6] <- as.numeric(KOFindex[,6])

KOFindex[,7] <- as.character(KOFindex[,7])
KOFindex[,7][KOFindex[,7] == "."] <- NA
KOFindex[,7] <- as.numeric(KOFindex[,7])

KOFindex[,8] <- as.character(KOFindex[,8])
KOFindex[,8][KOFindex[,8] == "."] <- NA
KOFindex[,8] <- as.numeric(KOFindex[,8])

KOFindex[,9] <- as.character(KOFindex[,9])
KOFindex[,9][KOFindex[,9] == "."] <- NA
KOFindex[,9] <- as.numeric(KOFindex[,9])

KOFindex[,10] <- as.character(KOFindex[,10])
KOFindex[,10][KOFindex[,10] == "."] <- NA
KOFindex[,10] <- as.numeric(KOFindex[,10])

KOFindex[,11] <- as.character(KOFindex[,11])
KOFindex[,11][KOFindex[,11] == "."] <- NA
KOFindex[,11] <- as.numeric(KOFindex[,11])

KOFindex[,12] <- as.character(KOFindex[,12])
KOFindex[,12][KOFindex[,12] == "."] <- NA
KOFindex[,12] <- as.numeric(KOFindex[,12])

# Remove observations that cause problems: Channel Islands (belonging to UK) 
KOFindex <- KOFindex[KOFindex$country != "Channel Islands",]

# Other problems in other data frames: East Germany, Germany 1990, Kosovo

# Changing the coding of the country names
KOFindex$iso2c <- countrycode(KOFindex$country, 
                              origin = "country.name",
                              destination = "iso2c", 
                              warn = TRUE)

# Removing variables not needed
KOFindex <- KOFindex[, names(KOFindex) != "code"]
KOFindex <- KOFindex[, names(KOFindex) != "country"]
KOFindex <- KOFindex[, names(KOFindex) != "b"]
KOFindex <- KOFindex[, names(KOFindex) != "bi"]
KOFindex <- KOFindex[, names(KOFindex) != "bii"]
KOFindex <- KOFindex[, names(KOFindex) != "biii"]
KOFindex <- KOFindex[, names(KOFindex) != "c"]

# Renaming remaining variables
names(KOFindex)[names(KOFindex) == "a"] <- "KOF.econ.global"
names(KOFindex)[names(KOFindex) == "ai"] <- "KOF.econ.global.flows"
names(KOFindex)[names(KOFindex) == "aii"] <- "KOF.econ.global.restrictions"
names(KOFindex)[names(KOFindex) == "index"] <- "KOF.index"

# Getting rid of observations prior to 1960 
KOFindex <- KOFindex[KOFindex$year >= 1960,]

# Changing class of year variable
KOFindex$year <- as.numeric(KOFindex$year)
