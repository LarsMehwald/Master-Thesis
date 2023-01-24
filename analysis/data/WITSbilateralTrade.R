######################################
# Master thesis
# Lars Mehwald
# Data gathering: WITS bilateral trade
# 4 April 2016
######################################

######################################
# Data downloading process
######################################

# The data is generated from the WB WITS data base, accessed by user account
# http://wits.worldbank.org/
# lars.mehwald@posteo.de
# Data is made available from UN COMTRADE

# This does not include trade in services 

# To avoid the 50,000 row limit, one can select a large amount of data and reduce the 
# number of rows in the download process (by specifying pivotal header)

# To collect the data, following options have been inserted

# If large data amount is selected, then it takes longer (15 minutes)
# and another 2 minutes after the download options have been selected
# In the morning (at night in the USA), it is three to four times faster

# Products: Total Trade from the GTAP (HS 1988) has been selected, because 
# one needed to select one nomenclature system

# first try: GTAP
# second try: BEC - HS 1988
# third try: CCCN - HS 1988 (system from 1974)
# fourth try: SITC 1
# SITC 1 works before 1988

# Reporting countries: all OECD, including the the countries of that group
# (otherwise one would have gotten the data on the aggregate level)
# covers all countries in smaller sample of Kayser and a little beyond 
# In addition: composite country "Belgium-Luxembourg", because there was 
# no seperate reporting prior to 1990s

# Partner country: all WTO (again including the countries of that group)
# captures the main traders (since Russia and China joined)
# In addition: composite country "Belgium-Luxembourg"

# years: 1962-2015

# indicator: gross imports and exports

# When one downloaded the data, one needed to specify the pivotal header and data:
# pivotal header (the variable that is spread as columns): PartnerISO3
# pivotal data: TradeValue

# Download as a ZIP file (name on WITS: TotalDownloadTradeFlows), 
# then extracted and copied into the folder structure

######################################
# Data preparation process
######################################

# Load the data set from the locally stored file
WITS.bilateral.trade <- read.csv("analysis/data/rawdata/WITSbilateralTrade.csv", 
                                 header = TRUE)

# Deleting observations for OECD as a whole
WITS.bilateral.trade <- WITS.bilateral.trade[WITS.bilateral.trade$ReporterISO3 != "OECD",]

######################################
# Solving the problem with "Belgium-Luxemburg" and country codes 
######################################

# Adding the observations to the data frame and using one for Belgium, 
# the other for Luxemburg.
# Thereby I am overstating their trade volumes, but that ok, 
# because I divide it with a greater total trade volume 
# But I they dont show up as trading partners for the other, this is problematic
WITS.bilateral.trade.bel.lux <- WITS.bilateral.trade[WITS.bilateral.trade$ReporterISO3 == "BLX", ]

# The additional rows are used for Luxembourg
WITS.bilateral.trade.bel.lux$ReporterName <- gsub("Belgium-Luxembourg", "Luxembourg", WITS.bilateral.trade.bel.lux$ReporterName)
WITS.bilateral.trade.bel.lux$iso2c <- countrycode(WITS.bilateral.trade.bel.lux$ReporterName, 
                                          origin = "country.name",
                                          destination = "iso2c", 
                                          warn = TRUE)

# Creating iso2c country codes (the remaining BLX is used for Belgium)
WITS.bilateral.trade$ReporterName <- gsub("Belgium-Luxembourg", "Belgium", WITS.bilateral.trade$ReporterName)
WITS.bilateral.trade$iso2c <- countrycode(WITS.bilateral.trade$ReporterName, 
                                          origin = "country.name",
                                          destination = "iso2c", 
                                          warn = TRUE)

# Adding the intermediate object to the original data frame
WITS.bilateral.trade <- rbind(WITS.bilateral.trade, WITS.bilateral.trade.bel.lux)

# Remove intermediate object
rm(WITS.bilateral.trade.bel.lux)

######################################
# Data preparation process - continued
######################################

# Remove variables not needed
WITS.bilateral.trade <- WITS.bilateral.trade[, names(WITS.bilateral.trade) != "Nomenclature" &
                                               names(WITS.bilateral.trade) != "ProductCode" &
                                               names(WITS.bilateral.trade) != "TradeFlowCode" &
                                               names(WITS.bilateral.trade) != "ReporterISO3" &
                                               names(WITS.bilateral.trade) != "ReporterName"]

# Rename year variable
names(WITS.bilateral.trade)[names(WITS.bilateral.trade) == "Year"] <- "year"

# Reordering (getting iso2c in the front)
WITS.bilateral.trade <- WITS.bilateral.trade[, c(ncol(WITS.bilateral.trade), 
                                                 1:ncol(WITS.bilateral.trade)-1)]

# Turning data into long format (to aggregate exports and imports)
WITS.bilateral.trade.long <- gather(data = WITS.bilateral.trade,
                                    trade.partner, 
                                    trade.value, 
                                    4:ncol(WITS.bilateral.trade))

# Combine exports and imports
WITS.bilateral.trade.long <- aggregate(trade.value ~ trade.partner + year + iso2c,
          data = WITS.bilateral.trade.long,
          FUN = sum,
          na.rm=TRUE)

# Shortening the name of the trade partners
WITS.bilateral.trade.long$trade.partner <- gsub(".in.1000.USD.", 
                                                "", 
                                                WITS.bilateral.trade.long$trade.partner)

# Turning the data frame back into wide format
WITS.bilateral.trade <- spread(data = WITS.bilateral.trade.long,
                  trade.partner,
                  trade.value)
rm(WITS.bilateral.trade.long)

# Delete WTO
WITS.bilateral.trade <- WITS.bilateral.trade[, names(WITS.bilateral.trade) != "WTO.ALL"]

######################################
# Function to create top 10 trading partners 
######################################

ten.highest <- function(xx){

# Create data frame with 10 highest trading partners for the first row
df.sorted <- sort(WITS.bilateral.trade[xx,3:153], decreasing = TRUE)[,1:10]

# creating variables containing the name of the trading partner
abc <- data.frame(trade.1.name = names(df.sorted)[1], 
                  trade.1.value = df.sorted[,1],
                  trade.2.name = names(df.sorted)[2], 
                  trade.2.value = df.sorted[,2],
                  trade.3.name = names(df.sorted)[3], 
                  trade.3.value = df.sorted[,3],
                  trade.4.name = names(df.sorted)[4], 
                  trade.4.value = df.sorted[,4],
                  trade.5.name = names(df.sorted)[5], 
                  trade.5.value = df.sorted[,5],
                  trade.6.name = names(df.sorted)[6], 
                  trade.6.value = df.sorted[,6],
                  trade.7.name = names(df.sorted)[7], 
                  trade.7.value = df.sorted[,7],
                  trade.8.name = names(df.sorted)[8], 
                  trade.8.value = df.sorted[,8],
                  trade.9.name = names(df.sorted)[9], 
                  trade.9.value = df.sorted[,9],
                  trade.10.name = names(df.sorted)[10], 
                  trade.10.value = df.sorted[,10])

# Adding the country name and the year to the data frame 
def <- cbind(iso2c = WITS.bilateral.trade[xx, names(WITS.bilateral.trade) == "iso2c"],
                   year = WITS.bilateral.trade[xx, names(WITS.bilateral.trade) == "year"],
                   abc)

# Changing the class of the country name (otherwise all 802 observations reported as "1")
def$iso2c <- as.character(def$iso2c)
def$trade.1.name <- as.character(def$trade.1.name)
def$trade.2.name <- as.character(def$trade.2.name)
def$trade.3.name <- as.character(def$trade.3.name)
def$trade.4.name <- as.character(def$trade.4.name)
def$trade.5.name <- as.character(def$trade.5.name)
def$trade.6.name <- as.character(def$trade.6.name)
def$trade.7.name <- as.character(def$trade.7.name)
def$trade.8.name <- as.character(def$trade.8.name)
def$trade.9.name <- as.character(def$trade.9.name)
def$trade.10.name <- as.character(def$trade.10.name)

print(def)

}

######################################
# Function to execute (loop) the previous function 
######################################

ten.highest.loop <- function(yy){

ghi <- data.frame()
ghi <- rbind(ghi, ten.highest(yy))

}

######################################
# Data preparation continued
######################################

# Applying the functions 
jkl <- lapply(1:length(WITS.bilateral.trade$iso2c), ten.highest.loop)

# Creating a data frame from the new object (list)
mno <- data.frame(matrix(unlist(jkl), 
                         nrow = length(WITS.bilateral.trade$iso2c),
                         byrow = TRUE))

# Remove intermediate object
rm(jkl)

# Rename new data frame
names(mno) <- c("iso2c", "year", "trade.1.name", "trade.1.value", "trade.2.name",
                "trade.2.value", "trade.3.name", "trade.3.value", "trade.4.name", 
                "trade.4.value", "trade.5.name", "trade.5.value", "trade.6.name",
                "trade.6.value", "trade.7.name", "trade.7.value", "trade.8.name", 
                "trade.8.value", "trade.9.name", "trade.9.value", "trade.10.name", 
                "trade.10.value")

# Merge with original data frame - why? I can take the intermediate one
# WITS.bilateral.trade <- merge(WITS.bilateral.trade, mno, by = c("iso2c", "year"), all = TRUE)
WITS.bilateral.trade <- mno
rm(mno)

# I found out, that ROM was in use for Romania, now it is ROU
# https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
# Hence I change it prior to renaming
WITS.bilateral.trade$trade.1.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.1.name)
WITS.bilateral.trade$trade.2.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.2.name)
WITS.bilateral.trade$trade.3.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.3.name)
WITS.bilateral.trade$trade.4.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.4.name)
WITS.bilateral.trade$trade.5.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.5.name)
WITS.bilateral.trade$trade.6.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.6.name)
WITS.bilateral.trade$trade.7.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.7.name)
WITS.bilateral.trade$trade.8.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.8.name)
WITS.bilateral.trade$trade.9.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.9.name)
WITS.bilateral.trade$trade.10.name <- gsub("ROM", "ROU", WITS.bilateral.trade$trade.10.name)

# Same problem with Zaire (Democratic Republic of Congo)
WITS.bilateral.trade$trade.1.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.1.name)
WITS.bilateral.trade$trade.2.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.2.name)
WITS.bilateral.trade$trade.3.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.3.name)
WITS.bilateral.trade$trade.4.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.4.name)
WITS.bilateral.trade$trade.5.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.5.name)
WITS.bilateral.trade$trade.6.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.6.name)
WITS.bilateral.trade$trade.7.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.7.name)
WITS.bilateral.trade$trade.8.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.8.name)
WITS.bilateral.trade$trade.9.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.9.name)
WITS.bilateral.trade$trade.10.name <- gsub("ZAR", "COD", WITS.bilateral.trade$trade.10.name)

# Problem that trading partner "Belgium-Luxembourg" exists
# I could decide for one country: BE is larger economy (in 2014), 
# 531000 MIO USD (LU 64000 MIO USD), hence BE is 8 times bigger
# This has the additional advantage, that I am not exceeding beyond 
# the top 10 trading partners.
# Or create a mean (or weighted) growth rate and link this to BLX
WITS.bilateral.trade$trade.1.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.1.name)
WITS.bilateral.trade$trade.2.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.2.name)
WITS.bilateral.trade$trade.3.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.3.name)
WITS.bilateral.trade$trade.4.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.4.name)
WITS.bilateral.trade$trade.5.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.5.name)
WITS.bilateral.trade$trade.6.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.6.name)
WITS.bilateral.trade$trade.7.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.7.name)
WITS.bilateral.trade$trade.8.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.8.name)
WITS.bilateral.trade$trade.9.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.9.name)
WITS.bilateral.trade$trade.10.name <- gsub("BLX", "BEL", WITS.bilateral.trade$trade.10.name)

# Changing the coding of the country names in the variables 
WITS.bilateral.trade$trade.1.name <- countrycode(WITS.bilateral.trade$trade.1.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.2.name <- countrycode(WITS.bilateral.trade$trade.2.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.3.name <- countrycode(WITS.bilateral.trade$trade.3.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.4.name <- countrycode(WITS.bilateral.trade$trade.4.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.5.name <- countrycode(WITS.bilateral.trade$trade.5.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.6.name <- countrycode(WITS.bilateral.trade$trade.6.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.7.name <- countrycode(WITS.bilateral.trade$trade.7.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.8.name <- countrycode(WITS.bilateral.trade$trade.8.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.9.name <- countrycode(WITS.bilateral.trade$trade.9.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)
WITS.bilateral.trade$trade.10.name <- countrycode(WITS.bilateral.trade$trade.10.name, origin = "iso3c", 
                                                 destination = "iso2c", warn = TRUE)

# Changing the class of the values 
WITS.bilateral.trade$trade.1.value <- as.numeric(as.character(WITS.bilateral.trade$trade.1.value))
WITS.bilateral.trade$trade.2.value <- as.numeric(as.character(WITS.bilateral.trade$trade.2.value))
WITS.bilateral.trade$trade.3.value <- as.numeric(as.character(WITS.bilateral.trade$trade.3.value))
WITS.bilateral.trade$trade.4.value <- as.numeric(as.character(WITS.bilateral.trade$trade.4.value))
WITS.bilateral.trade$trade.5.value <- as.numeric(as.character(WITS.bilateral.trade$trade.5.value))
WITS.bilateral.trade$trade.6.value <- as.numeric(as.character(WITS.bilateral.trade$trade.6.value))
WITS.bilateral.trade$trade.7.value <- as.numeric(as.character(WITS.bilateral.trade$trade.7.value))
WITS.bilateral.trade$trade.8.value <- as.numeric(as.character(WITS.bilateral.trade$trade.8.value))
WITS.bilateral.trade$trade.9.value <- as.numeric(as.character(WITS.bilateral.trade$trade.9.value))
WITS.bilateral.trade$trade.10.value <- as.numeric(as.character(WITS.bilateral.trade$trade.10.value))

# Creating total amount of trade with top 10 trading partners 
WITS.bilateral.trade$trade.total.top10 <- WITS.bilateral.trade$trade.1.value + 
  WITS.bilateral.trade$trade.2.value + WITS.bilateral.trade$trade.3.value +
  WITS.bilateral.trade$trade.4.value + WITS.bilateral.trade$trade.5.value + 
  WITS.bilateral.trade$trade.6.value + WITS.bilateral.trade$trade.7.value + 
  WITS.bilateral.trade$trade.8.value + WITS.bilateral.trade$trade.9.value + 
  WITS.bilateral.trade$trade.10.value

# Creating trade shares 
WITS.bilateral.trade$trade.1.share <- WITS.bilateral.trade$trade.1.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.2.share <- WITS.bilateral.trade$trade.2.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.3.share <- WITS.bilateral.trade$trade.3.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.4.share <- WITS.bilateral.trade$trade.4.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.5.share <- WITS.bilateral.trade$trade.5.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.6.share <- WITS.bilateral.trade$trade.6.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.7.share <- WITS.bilateral.trade$trade.7.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.8.share <- WITS.bilateral.trade$trade.8.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.9.share <- WITS.bilateral.trade$trade.9.value / 
  WITS.bilateral.trade$trade.total.top10
WITS.bilateral.trade$trade.10.share <- WITS.bilateral.trade$trade.10.value / 
  WITS.bilateral.trade$trade.total.top10

# Changing the class of the identifier (in order to better merge them later)
WITS.bilateral.trade$iso2c <- as.character(WITS.bilateral.trade$iso2c)
WITS.bilateral.trade$year <- as.numeric(as.character(WITS.bilateral.trade$year))
