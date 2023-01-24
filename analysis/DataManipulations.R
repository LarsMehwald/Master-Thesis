######################################
# Master thesis
# Lars Mehwald
# Data manipulations 
# 4 April 2016
######################################

######################################
# Deleting unwanted variables 
######################################

df <- df[,names(df) != "unemployment.long"]
df <- df[,names(df) != "inflation"]

######################################
# Relative economic measures (adjusted)
######################################

# First differences of unemployment 
# Create new object for "previous" unemployment 
df.unemployment <- df[,c("iso2c", "year", "unemployment")]
df.unemployment$year <- df.unemployment$year + 1
names(df.unemployment)[names(df.unemployment) == "unemployment"] <- "unemployment.previous"
# Merge previous and current unemployment rates 
df <- merge(df, df.unemployment, by = c("iso2c", "year"), all.x = TRUE, all.y = FALSE)
# Create first-difference for unemployment
df$unemployment.fd <- df$unemployment - df$unemployment.previous
# Delete intermediary objects/ variables
rm(df.unemployment)
df <- df[, names(df) != "unemployment.previous"]

source("analysis/manipulations/RelativeAdjusted.R") 
source("analysis/manipulations/RelativeInternational.R") 
source("analysis/manipulations/RelativeRegional.R") 
source("analysis/manipulations/RelativeTrade.R") 
source("analysis/manipulations/RelativeBordering.R") 
source("analysis/manipulations/RelativeClosest.R")
source("analysis/manipulations/RelativeLanguage.R")

# Remove intermediary variables 
df <- df[, names(df) != "GDPgrowth.previous" & names(df) != "unemployment.fd.previous"]

# Shorten also name of other unemployment variables to ue
names(df)[names(df) == "unemployment"] <- "ue"
names(df)[names(df) == "unemployment.fd"] <- "ue.fd"

######################################
# Extending World Bank Institutions until 2015
######################################

# source("analysis/manipulations/ExtendingDataSet.R")

######################################
# Creating globalization indicator finance
######################################

df$IIP.assets.plus.liabilities.gdp <- (df$IPP.liabilities + df$IPP.assets) / df$GDPinCurrentMioUSD

######################################
# Adjusting the election outcomes and years
######################################

# Problem: WB Institutions records the vote shares for the 1st of January 
# of each year. Hence if an election occurs in a given year, then the 
# election outcome is recorded the following year. 

# I do not change the year coding of govoth and oppoth, but rather rename the 
# original variable (so I dont calculate the number of parties after an election)

df.intermediary <- df[, names(df) == "iso2c" | names(df) == "year" | 
                        names(df) == "execme" | names(df) == "percent1" | 
                        names(df) == "percentl" | names(df) == "gov1me" |
                        names(df) == "gov1vote" | names(df) == "gov2me" |
                        names(df) == "gov2vote" | names(df) == "gov3me" |
                        names(df) == "gov3vote" | names(df) == "opp1me" |
                        names(df) == "opp1vote" | names(df) == "opp2me" |
                        names(df) == "opp2vote" | names(df) == "opp3me" |
                        names(df) == "opp3vote"]

# Renaming original variables (in order to keep and 
# easily identify them after merging)
names(df)[names(df) == "execme"] <- "execme.1jan"
names(df)[names(df) == "percent1"] <- "percent1.1jan"
names(df)[names(df) == "percentl"] <- "percentl1jan"
names(df)[names(df) == "gov1me"] <- "gov1me.1jan"
names(df)[names(df) == "gov1vote"] <- "gov1vote.1jan"
names(df)[names(df) == "gov2me"] <- "gov2me.1jan"
names(df)[names(df) == "gov2vote"] <- "gov2vote.1jan"
names(df)[names(df) == "gov3me"] <- "gov3me.1jan"
names(df)[names(df) == "gov3vote"] <- "gov3vote.1jan"
names(df)[names(df) == "opp1me"] <- "opp1me.1jan"
names(df)[names(df) == "opp1vote"] <- "opp1vote.1jan"
names(df)[names(df) == "opp2me"] <- "opp2me.1jan"
names(df)[names(df) == "opp2vote"] <- "opp2vote.1jan"
names(df)[names(df) == "opp3me"] <- "opp3me.1jan"
names(df)[names(df) == "opp3vote"] <- "opp3vote.1jan"

# Adjusting the naming of govoth and oppoth (for consistency)
names(df)[names(df) == "govoth"] <- "govoth.1jan"
names(df)[names(df) == "oppoth"] <- "oppoth.1jan"

# Changing the year variable
df.intermediary$year <- df.intermediary$year - 1

# Merging
df <- merge(df, df.intermediary, 
            by=c("iso2c", "year"), 
            all.x = TRUE)

# Removing intermediary object
rm(df.intermediary)

######################################
# Number of parties in parliament 
######################################

# Needs to come after "Adjusting the election outcomes and years"
source("analysis/manipulations/NumberPartiesParliament.R")

######################################
# Incumbent (previous largest government) party vote in legislative
######################################

# Converting all party names into characters 
df$incumbent.legislative <- as.character(df$gov1me.1jan)
df$gov1me <- as.character(df$gov1me)
df$gov2me <- as.character(df$gov2me)
df$gov3me <- as.character(df$gov3me)
df$opp1me <- as.character(df$opp1me)
df$opp2me <- as.character(df$opp2me)
df$opp3me <- as.character(df$opp3me)

# Creating a new variable containing the vote share of the incumbent 

rownames(df) <- NULL
df$leg.incumbent.vote <- NA

for (i in 1:nrow(df)) {
  if (df[i, "legelec"]  == 1 & 
      is.na(df[i, "legelec"])  == FALSE & 
      df[i, "incumbent.legislative"] == df[i, "gov1me"] &
      is.na(df[i, "incumbent.legislative"]) == FALSE &
      is.na(df[i, "gov1me"]) == FALSE) {
    df[i, "leg.incumbent.vote"] <- df[i, "gov1vote"]
  }
  if (df[i, "legelec"]  == 1 & 
      is.na(df[i, "legelec"])  == FALSE & 
      df[i, "incumbent.legislative"] == df[i, "gov2me"] &
      is.na(df[i, "incumbent.legislative"]) == FALSE &
      is.na(df[i, "gov2me"]) == FALSE) {
    df[i, "leg.incumbent.vote"] <- df[i, "gov2vote"]
  }
  if (df[i, "legelec"]  == 1 & 
      is.na(df[i, "legelec"])  == FALSE & 
      df[i, "incumbent.legislative"] == df[i, "gov3me"] &
      is.na(df[i, "incumbent.legislative"]) == FALSE &
      is.na(df[i, "gov3me"]) == FALSE) {
    df[i, "leg.incumbent.vote"] <- df[i, "gov3vote"]
  }
  if (df[i, "legelec"]  == 1 & 
      is.na(df[i, "legelec"])  == FALSE & 
      df[i, "incumbent.legislative"] == df[i, "opp1me"] &
      is.na(df[i, "incumbent.legislative"]) == FALSE &
      is.na(df[i, "opp1me"]) == FALSE) {
    df[i, "leg.incumbent.vote"] <- df[i, "opp1vote"]
  }
  if (df[i, "legelec"]  == 1 & 
      is.na(df[i, "legelec"])  == FALSE & 
      df[i, "incumbent.legislative"] == df[i, "opp2me"] &
      is.na(df[i, "incumbent.legislative"]) == FALSE &
      is.na(df[i, "opp2me"]) == FALSE) {
    df[i, "leg.incumbent.vote"] <- df[i, "opp2vote"]
  }
  if (df[i, "legelec"]  == 1 & 
      is.na(df[i, "legelec"])  == FALSE & 
      df[i, "incumbent.legislative"] == df[i, "opp3me"] &
      is.na(df[i, "incumbent.legislative"]) == FALSE &
      is.na(df[i, "opp3me"]) == FALSE) {
    df[i, "leg.incumbent.vote"] <- df[i, "opp3vote"]
  }
}

rm(i)

# Manually deciding in cases where vote is missing 

df$year <- as.numeric(as.character(df$year))

# df[df$iso2c == "AU" & df$year == 2010, "leg.incumbent.vote"] <- # not yet coded
df[df$iso2c == "IT" & df$year == 1994, "leg.incumbent.vote"] <- 0
df[df$iso2c == "IT" & df$year == 1996, "leg.incumbent.vote"] <- 0
df[df$iso2c == "IT" & df$year == 2001, "leg.incumbent.vote"] <- 0
# df[df$iso2c == "IT" & df$year == 2008, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "AT" & df$year == 2012, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "JP" & df$year == 2009, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "JP" & df$year == 2010, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "ES" & df$year == 1977, "leg.incumbent.vote"] <- # first
df[df$iso2c == "ES" & df$year == 1986, "leg.incumbent.vote"] <- df[df$iso2c == "ES" & df$year == 1986, "gov1vote"] # Joined by other party
df[df$iso2c == "ES" & df$year == 1996, "leg.incumbent.vote"] <- df[df$iso2c == "ES" & df$year == 1986, "opp1vote"] # Detached by other party
# df[df$iso2c == "PT" & df$year == 1976, "leg.incumbent.vote"] <- # first election 
# df[df$iso2c == "PT" & df$year == 1983, "leg.incumbent.vote"] <- # split of party
df[df$iso2c == "CA" & df$year == 1993, "leg.incumbent.vote"] <- 0
df[df$iso2c == "FR" & df$year == 1978, "leg.incumbent.vote"] <- 0
df[df$iso2c == "FR" & df$year == 1986, "leg.incumbent.vote"] <- df[df$iso2c == "FR" & df$year == 1986, "opp1vote"] # Detached by other party
df[df$iso2c == "FR" & df$year == 1988, "leg.incumbent.vote"] <- df[df$iso2c == "FR" & df$year == 1986, "opp2vote"] # Detached by other party
# df[df$iso2c == "FR" & df$year == 2012, "leg.incumbent.vote"] <- # not yet coded
df[df$iso2c == "BE" & df$year == 1981, "leg.incumbent.vote"] <- 0
# df[df$iso2c == "BE" & df$year == 2007, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "BE" & df$year == 2010, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "NL" & df$year == 2012, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "GR" & df$year == 1989, "leg.incumbent.vote"] <- # strange coding
# df[df$iso2c == "GR" & df$year == 2012, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "US" & df$year == 2006, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "US" & df$year == 2008, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "US" & df$year == 2010, "leg.incumbent.vote"] <- # not yet coded
# df[df$iso2c == "US" & df$year == 2012, "leg.incumbent.vote"] <- # not yet coded

# Other measures are excluded
df$election <- df$leg.incumbent.vote

######################################
# Vote share in previous election
######################################

df$election.previous <- df$gov1vote.1jan

######################################
# Extending democracy score to 2015 
######################################

# Problem: the PolityIV democracy scores end in 2014
# hence 2015 is coded as NA and consequentially dropped
# when data is subsetted for democracy scores
# Solution: extending 2014 value to 2015

iso2c <- c(unique(df$iso2c))

for(i in 1:length(iso2c)) {
  df[which(df$iso2c == iso2c[i] & df$year == 2015), names(df) == "polity.iv.democ"] <- df[which(df$iso2c == iso2c[i] & df$year == 2014), names(df) == "polity.iv.democ"]
}

rm(iso2c, i)

######################################
# "Subsetting" data for legislative election years
######################################

# Subset data: only observations with elections occuring this year 
# either legislative elections or executive election (or both)
df$elec <- ifelse(df$legelec == 1 | df$exelec == 1, 1, 0)
# there are some observations that are straight NAs: this is caused by 
# the fact that these observations have NAs for legelec and/or exelec

# Alternative approach: coding of all other election observations as NAs,
# so only these observations are used for the regression, while all information
# is kept for other summary statistics and graphical output 
df$election <- ifelse(df$legelec == 1, df$election, NA)

######################################
# Creating the latural log of some variables 
######################################

# GDPperCapita
df$GDPperCapita.ln <- log(df$GDPperCapita)

# GDPinCurrentMioUSD
df$GDPinCurrentMioUSD.ln <- log(df$GDPinCurrentMioUSD)

# TradeOpenness
df$Trade.ln <- log(df$Trade)

# IIP.assets.plus.liabilities.gdp
df$IIP.assets.plus.liabilities.gdp.ln <- log(df$IIP.assets.plus.liabilities.gdp)

# population
df$population.ln <- log(df$population)

######################################
# Changing class of variables 
######################################

# system
df[which(df$system == 1), "system"] <- 0 # Assembly-elected President is coded like presidential 
df$system <- as.factor(df$system)

# others
df$region <- as.factor(as.character(df$region))
df$exelec <- as.factor(df$exelec)
