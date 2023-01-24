######################################
# Master thesis
# Lars Mehwald
# Extending data set
# 4 April 2016
######################################

######################################
# Extending World Bank Institutions until 2015
######################################

df$year <- as.numeric(as.character(df$year))
df$gov1me <- as.character(df$gov1me)
df$gov2me <- as.character(df$gov2me)
df$gov3me <- as.character(df$gov3me)
df$opp1me <- as.character(df$opp1me)
df$opp2me <- as.character(df$opp2me)
df$opp3me <- as.character(df$opp3me)

df.carry <- df[df$year == 2012 & df$iso2c == "IS", 
               names(df) == "iso2c" | names(df) == "year" | 
                 names(df) == "gov1me" | names(df) == "gov1vote" | 
                 names(df) == "gov2me" | names(df) == "gov2vote" | 
                 names(df) == "gov3me" | names(df) == "gov3vote" |
                 names(df) == "opp1me" | names(df) == "opp1vote" |
                 names(df) == "opp2me" | names(df) == "opp2vote" |
                 names(df) == "opp3me" | names(df) == "opp3vote" | 
                 names(df) == "legelec" | names(df) == "dateleg"]

df.leg.elec <- data.frame(iso2c = character(), year = numeric(),
                          gov1me = character(), gov1vote = numeric(), 
                          gov2me = character(), gov2vote = numeric(), 
                          gov3me = character(), gov3vote = numeric(), 
                          opp1me = character(), opp1vote = numeric(), 
                          opp2me = character(), opp2vote = numeric(), 
                          opp3me = character(), opp3vote = numeric(),
                          legelec = numeric(), dateleg = numeric())

# AU
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("AU"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# AT - done 
# http://www.ipu.org/parline-e/reports/2017_E.htm
# Total seats won as a share of total possible seats (, because vote share not reported)
# Social Democratic Party (SPÖ), People's Party (ÖVP), Freedom Party (FPÖ), Alliance for the Future of Austria (BZÖ), Greens (GAL)
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("AT"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("SPO", "SPO", "SPO", "SPO"), gov1vote = c(29.26,28.42,28.42,28.42), 
                                             gov2me = c("OVP", "OVP", "OVP", "OVP"), gov2vote = c(25.98,25.68,25.68,25.68), 
                                             gov3me = c(NA, NA, NA, NA), gov3vote = c(0,0,0,0), 
                                             opp1me = c("FPO", "FPO", "FPO", "FPO"), opp1vote = c(17.54,21.96,21.96,21.96), 
                                             opp2me = c("BZO", "GAL", "GAL", "GAL"), opp2vote = c(10.7,13.11,13.11,13.11), 
                                             opp3me = c("GAL", "Team Stronach for Austria (FRANK)", "Team Stronach for Austria (FRANK)", "Team Stronach for Austria (FRANK)"), opp3vote = c(10.43,6.01,6.01,6.01),
                                             legelec = c(1,0,0,0), dateleg = c(9,NA,NA,NA)))
# Mistake: AT in 2012, there was no election, but rather in 2013 
df[df$iso2c == "AT" & df$year == 2012, names(df) == "legelec"] <- 0
df[df$iso2c == "AT" & df$year == 2012, names(df) == "dateleg"] <- NA

# BE
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("BE"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# CA - done 
# http://www.ipu.org/parline-e/reports/2055_E.htm
# Total seats won as a share of total possible seats (, because vote share not reported)
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("CA"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("CPC", "CPC", "CPC", "LPC"), gov1vote = c(39.6,39.6,39.6,59.74), 
                                             gov2me = c(NA, NA, NA, NA), gov2vote = c(0,0,0,0), 
                                             gov3me = c(NA, NA, NA, NA), gov3vote = c(0,0,0,0), 
                                             opp1me = c("NDP", "NDP", "NDP", "CPC"), opp1vote = c(30.6,30.6,30.6,32.14), 
                                             opp2me = c("LPC", "LPC", "LPC", "NDP"), opp2vote = c(18.9,18.9,18.9,14.29), 
                                             opp3me = c("BQ", "BQ", "BQ", "BQ"), opp3vote = c(6,6,6,3.25),
                                             legelec = c(0,0,1,0), dateleg = c(NA,NA,10,NA)))

# DE, Germany: Bundestag, Zweitstimmen, Parties dropped out of parliament are still reported - done 
# https://www.bundeswahlleiter.de/de/bundestagswahlen/BTW_BUND_13/ergebnisse/bundesergebnisse/index.html
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("DE"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("CDU/CSU", "CDU/CSU", "CDU/CSU", "CDU/CSU"), gov1vote = c(33.8,41.5,41.5,41.5), 
                                             gov2me = c("SPD", "SPD", "SPD", "SPD"), gov2vote = c(23.03,25.7,25.7,25.7), 
                                             gov3me = c(NA, NA, NA, NA), gov3vote = c(0,0,0,0), # Votes for no party recorded as 0 
                                             opp1me = c("FDP", "Left", "Left", "Left"), opp1vote = c(14.56,8.6,8.6,8.6), 
                                             opp2me = c("Left", "Alliance90/Greens", "Alliance90/Greens", "Alliance90/Greens"), opp2vote = c(11.89,8.4,8.4,8.4), 
                                             opp3me = c("Alliance90/Greens", "FDP", "FDP", "FDP"), opp3vote = c(10.71,4.8,4.8,4.8),
                                             legelec = c(1,0,0,0), dateleg = c(9,NA,NA,NA)))

# DK - done 
# http://www.ipu.org/parline-e/reports/2087_E.htm
# Total seats won as a share of total possible seats (, because vote share not reported)
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("DK"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("SD", "SD", "SD", "V"), gov1vote = c(24.86,24.86,24.86,19), 
                                             gov2me = c("RV", "RV", "RV", NA), gov2vote = c(9.48,9.48,9.48,0), 
                                             gov3me = c("SF", "SF", "SF", NA), gov3vote = c(9.2,9.2,9.2,0), 
                                             opp1me = c("V", "V", "V", "SD"), opp1vote = c(26.74,26.74,26.74,26.25), 
                                             opp2me = c("LA", "LA", "LA", "Danish People's Party"), opp2vote = c(4.98,4.98,4.98,20.67), 
                                             opp3me = c("KF", "KF", "KF", "Unity List-Red-Green Alliance"), opp3vote = c(4.94,4.94,4.94,7.82),
                                             legelec = c(0,0,1,0), dateleg = c(NA,NA,6,NA)))

# FR - done 
# http://www.ipu.org/parline-e/reports/2113_E.htm
# Code book: "Thus runoffs are counted, but their prior elections are not."
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("FR"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("PS", "PS", "PS", "PS"), gov1vote = c(41.01,41.01,41.01,41.01), 
                                             gov2me = c("Europe-Ecology-The Greens", "Europe-Ecology-The Greens", "Europe-Ecology-The Greens", "Europe-Ecology-The Greens"), gov2vote = c(3.6,3.6,3.6,3.6), 
                                             gov3me = c("Left Radical", "Left Radical", "Left Radical", "Left Radical"), gov3vote = c(2.34,2.34,2.34,2.34), 
                                             opp1me = c("UMP", "UMP", "UMP", "UMP"), opp1vote = c(37.95,37.95,37.95,37.95), 
                                             opp2me = c("FN", "FN", "FN", "FN"), opp2vote = c(3.66,3.66,3.66,3.66), 
                                             opp3me = c("Other left wing parties", "Other left wing parties", "Other left wing parties", "Other left wing parties"), opp3vote = c(3.08,3.08,3.08,3.08),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# GR - done 
# http://www.ipu.org/parline-e/reports/arc/2125_12_June.htm
# Double elections in 2012 - the last is reported (June)
# Total seats won as a share of total possible seats (, because vote share not reported for 2015 election)
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("GR"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("ND", "ND", "ND", "SYRIZA"), gov1vote = c(29.66,29.66,29.66,48.33), 
                                             gov2me = c("PASOK", "PASOK", "PASOK", "AE"), gov2vote = c(12.28,12.28,12.28,3.33), 
                                             gov3me = c("Democratic Left (DA)", "Democratic Left (DA)", "Democratic Left (DA)", NA), gov3vote = c(6.26,6.26,6.26,0), 
                                             opp1me = c("SYRIZA", "SYRIZA", "SYRIZA", "ND"), opp1vote = c(26.89,26.89,26.89,25), 
                                             opp2me = c("AE", "AE", "AE", "Chryssi Ayghi"), opp2vote = c(7.51,7.51,7.51,6), 
                                             opp3me = c("Chryssi Ayghi", "Chryssi Ayghi", "Chryssi Ayghi", "PASOK"), opp3vote = c(6.92,6.92,6.92,5.56),
                                             legelec = c(0,0,1,0), dateleg = c(NA,NA,9,NA)))

# IS - done 
# http://www.ipu.org/parline-e/reports/2143_E.htm
# Total seats won as a share of total possible seats (, because vote share not reported for 2015 election)
# VGF is the left-green movement, The Alliance are the social democrats 
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("IS"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("The Alliance", "IP", "IP", "IP"), gov1vote = c(29.79,30.16,30.16,30.16), 
                                             gov2me = c("VGF", "PP", "PP", "PP"), gov2vote = c(21.68,30.16,30.16,30.16), 
                                             gov3me = c(NA, NA, NA, NA), gov3vote = c(0,0,0,0), 
                                             opp1me = c("IP", "The Alliance", "The Alliance", "The Alliance"), opp1vote = c(23.7,14.29,14.29,14.29), 
                                             opp2me = c("PP", "VGF", "VGF", "VGF"), opp2vote = c(14.8,11.11,11.11,11.11), 
                                             opp3me = c("Citizens' Movement", "Bright Future", "Bright Future", "Bright Future"), opp3vote = c(7.22,9.52,9.52,9.52),
                                             legelec = c(1,0,0,0), dateleg = c(4,NA,NA,NA)))

# IE
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("IE"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# IL
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("IL"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# IT
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("IT"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# JP
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("JP"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# LU
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("LU"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# NL
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("NL"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# NZ
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("NZ"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# NO - done 
# H are the conservatives, DNA Labour party 
# Total seats won as a share of total possible seats(, because vote share not reported)
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("NO"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("DNA", "H", "H", "H"), gov1vote = c(35.37,17.75,17.75,17.75), 
                                             gov2me = c("SV", "Frp", "Frp", "Frp"), gov2vote = c(6.2,17.16,17.16,17.16), 
                                             gov3me = c("SP", NA, NA, NA), gov3vote = c(6.15,0,0,0), 
                                             opp1me = c("Frp", "DNA", "DNA", "DNA"), opp1vote = c(22.91,32.54,32.54,32.54), 
                                             opp2me = c("H", "Centre Party", "Centre Party", "Centre Party"), opp2vote = c(17.24,5.82,5.82,5.82), 
                                             opp3me = c("KrF", "Christian Democratic Party", "Christian Democratic Party", "Christian Democratic Party"), opp3vote = c(5.54,5.82,5.82,5.82),
                                             legelec = c(1,0,0,0), dateleg = c(9,NA,NA,NA)))

# PT
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("PT"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# ES - need to follow-up
# http://www.ipu.org/parline-e/reports/2293_E.htm
# No valid coalition yet -> need to follow-up, intermediary solution adopted (PP reported as governmental party)
# Total seats won as a share of total possible seats, i.e. 350 (, because vote share not reported)
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("ES"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("PP", "PP", "PP", "PP"), gov1vote = c(44.62,44.62,44.62,35.14), 
                                             gov2me = c("CiU", "CiU", "CiU", NA), gov2vote = c(4.17,4.17,4.17,0), 
                                             gov3me = c(NA, NA, NA, NA), gov3vote = c(0,0,0,0), 
                                             opp1me = c("PSOE", "PSOE", "PSOE", NA), opp1vote = c(28.73,28.73,28.73,0), 
                                             opp2me = c("IU-LV", "IU-LV", "IU-LV", NA), opp2vote = c(6.92,6.92,6.92,0), 
                                             opp3me = c("Amaiur", "Amaiur", "Amaiur", NA), opp3vote = c(1.37,1.37,1.37,0),
                                             legelec = c(0,0,1,0), dateleg = c(NA,NA,12,NA)))

# SE
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("SE"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# GB
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("GB"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))

# US
df.leg.elec <- rbind(df.leg.elec, data.frame(iso2c = c("US"), year = c(2013, 2014, 2015, 2016),
                                             gov1me = c("name", "name", "name", "name"), gov1vote = c(0,0,0,0), 
                                             gov2me = c("name", "name", "name", "name"), gov2vote = c(0,0,0,0), 
                                             gov3me = c("name", "name", "name", "name"), gov3vote = c(0,0,0,0), 
                                             opp1me = c("name", "name", "name", "name"), opp1vote = c(0,0,0,0), 
                                             opp2me = c("name", "name", "name", "name"), opp2vote = c(0,0,0,0), 
                                             opp3me = c("name", "name", "name", "name"), opp3vote = c(0,0,0,0),
                                             legelec = c(0,0,0,0), dateleg = c(NA,NA,NA,NA)))
