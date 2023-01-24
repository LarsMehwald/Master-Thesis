######################################
# Master thesis
# Lars Mehwald
# Number of parties in parliament
# 4 April 2016
######################################

# Problem: Number of other parties, i.e. I need to add them to 
# existing government and opposition parties 
# non existing party coded as NA 
# ulprty: coding of non-aligned or independent parliamentarians  

for (i in 1:nrow(df)) {
  if(is.na(df[i, "gov1me.1jan"]) == TRUE){
    df[i, "NumberGovParties"] <- 0
  }
  if(is.na(df[i, "gov2me.1jan"]) == TRUE & 
     is.na(df[i, "gov1me.1jan"]) == FALSE){
    df[i, "NumberGovParties"] <- 1
  }
  if(is.na(df[i, "gov3me.1jan"]) == TRUE & 
     is.na(df[i, "gov2me.1jan"]) == FALSE & 
     is.na(df[i, "gov1me.1jan"]) == FALSE){
    df[i, "NumberGovParties"] <- 2
  }
  if (is.na(df[i, "gov1me.1jan"]) == FALSE &
      is.na(df[i, "gov2me.1jan"]) == FALSE &
      is.na(df[i, "gov3me.1jan"]) == FALSE){
    df[i, "NumberGovParties"] <- 3 + as.numeric(as.character(df[i, "govoth.1jan"]))
  }
}
rm(i)

for (i in 1:nrow(df)) {
  if(is.na(df[i, "opp1me.1jan"]) == TRUE){
    df[i, "NumberOppParties"] <- 0
  }
  if(is.na(df[i, "opp2me.1jan"]) == TRUE & 
     is.na(df[i, "opp1me.1jan"]) == FALSE){
    df[i, "NumberOppParties"] <- 1
  }
  if(is.na(df[i, "opp3me.1jan"]) == TRUE & 
     is.na(df[i, "opp2me.1jan"]) == FALSE & 
     is.na(df[i, "opp1me.1jan"]) == FALSE){
    df[i, "NumberOppParties"] <- 2
  }
  if (is.na(df[i, "opp1me.1jan"]) == FALSE &
      is.na(df[i, "opp2me.1jan"]) == FALSE &
      is.na(df[i, "opp3me.1jan"]) == FALSE){
    df[i, "NumberOppParties"] <- 3 + as.numeric(as.character(ifelse(is.na(df[i, "oppoth.1jan"]) == TRUE,
                                                                    0,
                                                                    df[i, "oppoth.1jan"])))
  }
}
rm(i)

df$NumberParties <- df$NumberGovParties + df$NumberOppParties

# Insert NAs if there is no first governing pary 
for (i in 1:nrow(df)){
  if (is.na(df[i, "gov1me"]) == TRUE){
    df[i, "NumberParties"] <- NA
  }
}
rm(i)

# Creating factor variables
# intervals: left bound not included, right bound included,
df$NumberParties.Ordinal <- cut(df$NumberParties, 
                                breaks = c(-1,7,14,
                                           max(df$NumberParties, na.rm = TRUE)),
                                labels = c(1,2,3),
                                ordered_result = TRUE)
# it is important to include "ordered_result = TRUE", because otherwise,
# it is an unordered factor (i.e. categorical variable)
# it could be done with an argument to the "factor" function, 
# and with the "ordered" function

# Remove intermediary variables 
df <- df[, names(df) != "NumberGovParties" & names(df) != "NumberOppParties" & 
           names(df) != "NumberParties"]

######################################
# Extending number of parties from 2012 to 2015 
######################################

# Problem: the number of parties can be consistently claculated until 2011, (before with my addition until 2012)
# from then on, I would need to do it on my own, creating a bias in the coding
# Hence I think the least invasive way to deal with it, is to extend the score of 2011 to 
# 2012, 2013, 2014 and 2015
# Leaving it out would reduce the number of observations in the regression analysis 

iso2c <- c(unique(df$iso2c))

for(i in 1:length(iso2c)){
  df[which(df$iso2c == iso2c[i] & df$year == 2012), names(df) == "NumberParties.Ordinal"] <- df[which(df$iso2c == iso2c[i] & df$year == 2011), names(df) == "NumberParties.Ordinal"]
  df[which(df$iso2c == iso2c[i] & df$year == 2013), names(df) == "NumberParties.Ordinal"] <- df[which(df$iso2c == iso2c[i] & df$year == 2011), names(df) == "NumberParties.Ordinal"]
  df[which(df$iso2c == iso2c[i] & df$year == 2014), names(df) == "NumberParties.Ordinal"] <- df[which(df$iso2c == iso2c[i] & df$year == 2011), names(df) == "NumberParties.Ordinal"]
  df[which(df$iso2c == iso2c[i] & df$year == 2015), names(df) == "NumberParties.Ordinal"] <- df[which(df$iso2c == iso2c[i] & df$year == 2011), names(df) == "NumberParties.Ordinal"]
}

rm(i, iso2c)

# Renaming variable for simplicity (especially in regression output)
names(df)[names(df) == "NumberParties.Ordinal"] <- "parties"
