######################################
# Master thesis
# Lars Mehwald
# Sample selection
# 4 April 2016
######################################

df.full <- df

######################################
# Democracy
######################################

# Democracy score: FreedomHouse just used in instances where no PolityIV indicator
# Thereby I give more weight to PolityIV, since now cases are excluded that fail the PolityIV indicator, while attaining the FreedomHouse indicator
df$polity.iv.democ <- as.numeric(as.character(df$polity.iv.democ))
df$FHdemocracy <- as.numeric(as.character(df$FHdemocracy))
df.D <- df[df$polity.iv.democ >= 6 & 
             is.na(df$polity.iv.democ) == FALSE,] # Problems where NAs`
df.F <- df[df$FHdemocracy <= 3 & 
             is.na(df$FHdemocracy) == FALSE & 
             is.na(df$polity.iv.democ) == TRUE,] # Problems where NAs`
df <- rbind(df.D, df.F)
df <- unique(df)
rm(df.D, df.F)

######################################
# Large Sample
######################################

# Country sample according to Hellwig 2007
df.large <- df[df$iso2c == "AR" | df$iso2c == "AU" | df$iso2c == "AT" | df$iso2c == "BD" | df$iso2c == "BE" | df$iso2c == "BJ" | 
                   df$iso2c == "BO" | df$iso2c == "BW" | df$iso2c == "BR" | df$iso2c == "BG" | df$iso2c == "CA" | df$iso2c == "CL" | 
                   df$iso2c == "CO" | df$iso2c == "CR" | df$iso2c == "CZ" | df$iso2c == "DK" | df$iso2c == "DO" | df$iso2c == "EC" | 
                   df$iso2c == "SV" | df$iso2c == "EE" | df$iso2c == "FI" | df$iso2c == "FR" | df$iso2c == "DE" | df$iso2c == "GR" | 
                   df$iso2c == "HN" | df$iso2c == "HU" | df$iso2c == "IN" | df$iso2c == "IE" | df$iso2c == "IL" | df$iso2c == "IT" | 
                   df$iso2c == "JM" | df$iso2c == "JP" | df$iso2c == "KP" | df$iso2c == "LV" | df$iso2c == "LS" | df$iso2c == "LT" | 
                   df$iso2c == "MK" | df$iso2c == "MG" | df$iso2c == "MW" | df$iso2c == "ML" | df$iso2c == "MX" | df$iso2c == "MD" | 
                   df$iso2c == "MZ" | df$iso2c == "NA" | df$iso2c == "NL" | df$iso2c == "NZ" | df$iso2c == "NI" | df$iso2c == "NO" | 
                   df$iso2c == "PA" | df$iso2c == "PG" | df$iso2c == "PY" | df$iso2c == "PE" | df$iso2c == "PH" | df$iso2c == "PL" | 
                   df$iso2c == "PT" | df$iso2c == "RO" | df$iso2c == "RU" | df$iso2c == "SN" | df$iso2c == "SK" |                   # Seychelles missing 
                   df$iso2c == "SI" | df$iso2c == "ZA" | df$iso2c == "ES" | df$iso2c == "LK" | df$iso2c == "SE" | df$iso2c == "CH" | 
                   df$iso2c == "TH" | df$iso2c == "TT" | df$iso2c == "TR" | df$iso2c == "UA" | df$iso2c == "GB" |                   # Taiwan missing
                   df$iso2c == "US" | df$iso2c == "UY" | df$iso2c == "VE" ,]

# Argentina, Australia, Austria, Bangladesh, Belgium, Benin, 
# Bolivia, Botswana, Brazil, Bulgaria, Canada, Chile, 
# Colombia, Costa Rica, Czech Republic, Denmark, Dominican Rep., Ecuador, 
# El Salvador, Estonia, Finland, France, Germany, Greece, 
# Honduras, Hungary, India, Ireland, Israel, Italy, 
# Jamaica, Japan, Korea Republic of, Latvia, Lesotho, Lithuania, 
# Macedonia, Madagascar, Malawi, Mali, Mexico, Moldova, 
# Mozambique, Namibia, Netherlands, New Zealand, Nicaragua, Norway, 
# Panama, Papua New Guinea, Paraguay, Peru, Philippines, Poland, 
# Portugal, Romania, Russia, Senegal, Seychelles, Slovakia, 
# Slovenia, South Africa, Spain, Sri Lanka, Sweden, Switzerland, 
# Taiwan, Thailand, Trinidad and Tobago, Turkey, Ukraine, United Kingdom, 
# United States, Uruguay, Venezuela

# Island? not included in Hellwig paper
# I dont have Seychelles, nor Taiwan 

######################################
# Small sample
######################################

# Country sample OECD (mirrowing Kayser 2012)
df <- df[df$iso2c == "AU" | df$iso2c == "AT" | df$iso2c == "BE" |
           df$iso2c == "CA" | df$iso2c == "DK" | df$iso2c == "FI" |
           df$iso2c == "FR" | df$iso2c == "DE" | df$iso2c == "GR" | 
           df$iso2c == "IS" | df$iso2c == "IE" | df$iso2c == "IL" |
           df$iso2c == "IT" | df$iso2c == "JP" | df$iso2c == "LU" |
           df$iso2c == "NL" | df$iso2c == "NZ" | df$iso2c == "NO" |
           df$iso2c == "PT" | df$iso2c == "ES" | df$iso2c == "SE" |
           df$iso2c == "GB" | df$iso2c == "US",]
