######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB Institutions
# 4 April 2016
######################################

# http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/
# 0,,contentMDK:20649465~pagePK:64214825~piPK:64214943~theSitePK:469382,00.html

# Loading in the data 
URL <- "http://siteresources.worldbank.org/INTRES/Resources/469232-1107449512766/DPI2012.dta"
WBInstitutions <- read.dta(URL)
rm(URL)

# Saving the raw data
# write.csv(WBInstitutions, file = "analysis/data/rawdata/WBInstitutions.csv")

# Loading the saved data
# WBInstitutions <- read.csv(file = "analysis/data/rawdata/WBInstitutions.csv")
# WBInstitutions <- WBInstitutions[,-1]

# Changing the category of countryname to character 
WBInstitutions$countryname <- as.character(WBInstitutions$countryname)

# Renaming some of the country names
WBInstitutions[WBInstitutions$countryname == "S. Africa", "countryname"] <- "South Africa"
WBInstitutions[WBInstitutions$countryname == "Dom. Rep.", "countryname"] <- "Dominican Republic"

WBInstitutions[WBInstitutions$countryname == "P. N. Guinea", "countryname"] <- "Papua New Guinea"
WBInstitutions[WBInstitutions$countryname == "Eq. Guinea", "countryname"] <- "Equatorial Guinea"
WBInstitutions[WBInstitutions$countryname == "Eq. Guinea", "countryname"] <- "Equatorial Guinea"

# Problem: PRK (North Korea) is converted into KH (Cambodia)
WBInstitutions[WBInstitutions$countryname == "PRK", "countryname"] <- "Democratic Republic Korea"

# Problem: Turk Cyprus and Cyprus exist parallel and are coded as Cyprus by the countrycode package
WBInstitutions <- WBInstitutions[WBInstitutions$countryname != "Turk Cyprus",]

# Changing the coding of the country names
# Attention: Namibia is NA 
WBInstitutions$iso2c <- countrycode(WBInstitutions$countryname, 
                                    origin = "country.name",
                                    destination = "iso2c", 
                                    warn = TRUE)

# Changing the class of some variables
WBInstitutions$year <- as.numeric(WBInstitutions$year)
WBInstitutions$system <- as.factor(WBInstitutions$system)

# Keeping the relevant variables
WBInstitutions <- WBInstitutions[, names(WBInstitutions) == "iso2c" | 
                                   names(WBInstitutions) == "year" | 
                                   names(WBInstitutions) == "countryname" |
                                   names(WBInstitutions) == "system" |
                                   names(WBInstitutions) == "execme" |
                                   names(WBInstitutions) == "percent1" | 
                                   names(WBInstitutions) == "percentl" |
                                   names(WBInstitutions) == "gov1me" |
                                   names(WBInstitutions) == "gov1vote" |
                                   names(WBInstitutions) == "gov2me" |
                                   names(WBInstitutions) == "gov2vote" |
                                   names(WBInstitutions) == "gov3me" |
                                   names(WBInstitutions) == "gov3vote" |
                                   names(WBInstitutions) == "opp1me" |
                                   names(WBInstitutions) == "opp1vote" |
                                   names(WBInstitutions) == "opp2me" |
                                   names(WBInstitutions) == "opp2vote" |
                                   names(WBInstitutions) == "opp3me" |
                                   names(WBInstitutions) == "opp3vote" |                                   
                                   names(WBInstitutions) == "dateleg" |
                                   names(WBInstitutions) == "dateexec" |
                                   names(WBInstitutions) == "legelec" |
                                   names(WBInstitutions) == "exelec" |
                                   names(WBInstitutions) == "tensys" | # how long has the country been democratic? 
                                   names(WBInstitutions) == "govoth" | # number of gov parties
                                   names(WBInstitutions) == "oppoth" | # number of opp parties
                                   names(WBInstitutions) == "ulprty"  # number of indep parties
                                 ]

# Coding of NAs
WBInstitutions[,"system"] <- as.character(WBInstitutions[,"system"])
WBInstitutions[,"system"][WBInstitutions[,"system"] == "-999"] <- NA
WBInstitutions[,"system"] <- as.numeric(WBInstitutions[,"system"])

WBInstitutions[,"percent1"] <- as.character(WBInstitutions[,"percent1"])
WBInstitutions[,"percent1"][WBInstitutions[,"percent1"] == "-999"] <- NA
WBInstitutions[,"percent1"] <- as.numeric(WBInstitutions[,"percent1"])

WBInstitutions[,"percentl"] <- as.character(WBInstitutions[,"percentl"])
WBInstitutions[,"percentl"][WBInstitutions[,"percentl"] == "-999"] <- NA
WBInstitutions[,"percentl"] <- as.numeric(WBInstitutions[,"percentl"])

WBInstitutions[,"gov1vote"] <- as.character(WBInstitutions[,"gov1vote"])
WBInstitutions[,"gov1vote"][WBInstitutions[,"gov1vote"] == "-999"] <- NA
WBInstitutions[,"gov1vote"] <- as.numeric(WBInstitutions[,"gov1vote"])

WBInstitutions[,"gov2vote"] <- as.character(WBInstitutions[,"gov2vote"])
WBInstitutions[,"gov2vote"][WBInstitutions[,"gov2vote"] == "-999"] <- NA
WBInstitutions[,"gov2vote"] <- as.numeric(WBInstitutions[,"gov2vote"])

WBInstitutions[,"gov3vote"] <- as.character(WBInstitutions[,"gov3vote"])
WBInstitutions[,"gov3vote"][WBInstitutions[,"gov3vote"] == "-999"] <- NA
WBInstitutions[,"gov3vote"] <- as.numeric(WBInstitutions[,"gov3vote"])

WBInstitutions[,"opp1vote"] <- as.character(WBInstitutions[,"opp1vote"])
WBInstitutions[,"opp1vote"][WBInstitutions[,"opp1vote"] == "-999"] <- NA
WBInstitutions[,"opp1vote"] <- as.numeric(WBInstitutions[,"opp1vote"])

WBInstitutions[,"opp2vote"] <- as.character(WBInstitutions[,"opp2vote"])
WBInstitutions[,"opp2vote"][WBInstitutions[,"opp2vote"] == "-999"] <- NA
WBInstitutions[,"opp2vote"] <- as.numeric(WBInstitutions[,"opp2vote"])

WBInstitutions[,"opp3vote"] <- as.character(WBInstitutions[,"opp3vote"])
WBInstitutions[,"opp3vote"][WBInstitutions[,"opp3vote"] == "-999"] <- NA
WBInstitutions[,"opp3vote"] <- as.numeric(WBInstitutions[,"opp3vote"])

WBInstitutions[,"dateleg"] <- as.character(WBInstitutions[,"dateleg"])
WBInstitutions[,"dateleg"][WBInstitutions[,"dateleg"] == "-999"] <- NA
WBInstitutions[,"dateleg"] <- as.numeric(WBInstitutions[,"dateleg"])

WBInstitutions[,"dateexec"] <- as.character(WBInstitutions[,"dateexec"])
WBInstitutions[,"dateexec"][WBInstitutions[,"dateexec"] == "-999"] <- NA
WBInstitutions[,"dateexec"] <- as.numeric(WBInstitutions[,"dateexec"])

WBInstitutions[,"legelec"] <- as.character(WBInstitutions[,"legelec"])
WBInstitutions[,"legelec"][WBInstitutions[,"legelec"] == "-999"] <- NA
WBInstitutions[,"legelec"] <- as.numeric(WBInstitutions[,"legelec"])

WBInstitutions[,"exelec"] <- as.character(WBInstitutions[,"exelec"])
WBInstitutions[,"exelec"][WBInstitutions[,"exelec"] == "-999"] <- NA
WBInstitutions[,"exelec"] <- as.numeric(WBInstitutions[,"exelec"])

WBInstitutions[,"tensys"] <- as.character(WBInstitutions[,"tensys"])
WBInstitutions[,"tensys"][WBInstitutions[,"tensys"] == "-999"] <- NA
WBInstitutions[,"tensys"] <- as.numeric(WBInstitutions[,"tensys"])

WBInstitutions[,"govoth"] <- as.character(WBInstitutions[,"govoth"])
WBInstitutions[,"govoth"][WBInstitutions[,"govoth"] == "-999"] <- NA
WBInstitutions[,"govoth"] <- as.numeric(WBInstitutions[,"govoth"])

WBInstitutions[,"oppoth"] <- as.character(WBInstitutions[,"oppoth"])
WBInstitutions[,"oppoth"][WBInstitutions[,"oppoth"] == "-999"] <- NA
WBInstitutions[,"oppoth"] <- as.numeric(WBInstitutions[,"oppoth"])

WBInstitutions[,"ulprty"] <- as.character(WBInstitutions[,"ulprty"])
WBInstitutions[,"ulprty"][WBInstitutions[,"ulprty"] == "-999"] <- NA
WBInstitutions[,"ulprty"] <- as.numeric(WBInstitutions[,"ulprty"])
