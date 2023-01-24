######################################
# Master thesis
# Lars Mehwald
# Dealing with NAs 
# 4 April 2016
######################################

# input data: data as of after manipulations.R

# Filling missing values with zoo package:
# http://publish.illinois.edu/spencer-guerrero/2014/12/11/2-dealing-with-missing-data-in-r-omit-approx-or-spline-part-1/

######################################
# Getting useful sample 
######################################

# Democracy
df$polity.iv.democ <- as.numeric(as.character(df$polity.iv.democ))
df$FHdemocracy <- as.numeric(as.character(df$FHdemocracy))
df.D <- df[df$polity.iv.democ >= 6 & 
             is.na(df$polity.iv.democ) == FALSE,] # Problems where NAs`
df.F <- df[df$FHdemocracy <= 3 & 
             is.na(df$FHdemocracy) == FALSE & 
             is.na(df$polity.iv.democ) == TRUE,] # Problems where NAs`
df.test <- rbind(df.D, df.F)
df.test <- unique(df.test)
rm(df.D, df.F)

# Small sample
df.test <- df.test[df.test$iso2c == "AU" | df.test$iso2c == "AT" | df.test$iso2c == "BE" |
                df.test$iso2c == "CA" | df.test$iso2c == "DK" | df.test$iso2c == "FI" |
                df.test$iso2c == "FR" | df.test$iso2c == "DE" | df.test$iso2c == "GR" | 
                df.test$iso2c == "IS" | df.test$iso2c == "IE" | df.test$iso2c == "IL" |
                df.test$iso2c == "IT" | df.test$iso2c == "JP" | df.test$iso2c == "LU" |
                df.test$iso2c == "NL" | df.test$iso2c == "NZ" | df.test$iso2c == "NO" |
                df.test$iso2c == "PT" | df.test$iso2c == "ES" | df.test$iso2c == "SE" |
                df.test$iso2c == "GB" | df.test$iso2c == "US",]

# Election year
df.test <- df.test[is.na(df.test$leg.incumbent.vote) == FALSE,]

######################################
# Manually detecting NAs in data frame
######################################

# So far: 239 observations 
df.test <- df.test[,c("iso2c", "year", "election" , "election.previous", "GDPgrowth", 
                      "ue", "TradeOpenness", "system", "GDPperCapita.ln", "parties")]

# GDPgrowth: NZ 1975 is NA, data provided later 
# TradeOpenness: complete
# parties: mostly early observations, but also FR 1993 and IT 2001
# GDPperCapita.ln: just NZ early 
# system: complete 
# ue: complete after beginning of reporting 
# election.previous: only early entries missing 

