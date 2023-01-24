######################################
# Master thesis
# Lars Mehwald
# Data manipulations : unused script
# 4 April 2016
######################################

######################################
# Creating globalization indicator finance
######################################

# df$FA.assets.plus.liabilities.gdp <- (df$financial.account.direct.investment.assets + df$financial.account.direct.investment.liabilities + df$financial.account.portfolio.investment.assets + df$financial.account.portfolio.investment.liabilities) / df$GDPinCurrentMioUSD

######################################
# Incumbent (previous largest government) party vote in legislative
######################################

# I commented out the next 3 paragraphs, because of the coding of election outcomes
# Then I adjusted parapgraph 4, formerly:
# df$incumbent.legislative <- as.character(df$incumbent.legislative)

# Creating a new variable (in a new data frame)
# for being an incumbent in a legislative election
# incumbent.legislative <- df[,"gov1me"]
# year <- df[,"year"] + 1 # this is the manipulated year variable
# iso2c <- df[,"iso2c"]
# df.incumbent <- as.data.frame(cbind(year, iso2c))
# df.incumbent <- cbind(df.incumbent, incumbent.legislative)
# df.incumbent$year <- as.numeric(as.character(df.incumbent$year))
# df.incumbent$iso2c <- as.character(df.incumbent$iso2c)

# Merging (and change of class)
# df <- merge(df.incumbent, df, by=c("iso2c","year"), all = TRUE) 
# df$incumbent.legislative <- as.character(df$incumbent.legislative)

# Remove intermediary objects 
# rm(df.incumbent)
# rm(incumbent.legislative)
# rm(iso2c)
# rm(year)

# df$leg.incumbent.vote <- ifelse(df$legelec == 1 & df$incumbent.legislative == df$gov1me, df$gov1vote, 
#                             ifelse(df$legelec == 1 & df$incumbent.legislative == df$gov2me, df$gov2vote, 
#                                    ifelse(df$legelec == 1 & df$incumbent.legislative == df$gov3me, df$gov3vote, 
#                                           ifelse(df$legelec == 1 & df$incumbent.legislative == df$opp1me, df$opp1vote, 
#                                                  ifelse(df$legelec == 1 & df$incumbent.legislative == df$opp2me, df$opp2vote, 
#                                                         ifelse(df$legelec == 1 & df$incumbent.legislative == df$opp3me, df$opp3vote, NA))))))

######################################
# Incumbent Executive vote in legislative
######################################

# Getting to know the coding of execme
abc <- df[, names(df) == "iso2c" |
            names(df) == "year" |
            names(df) == "system" | 
            names(df) == "execme" |
            names(df) == "exelec" |
            names(df) == "percent1" |
            names(df) == "percent1.1jan" ] 
rm(abc)

# execme is available for all systems: system == 0, 1, 2
# Hence I assume it to be the PM for parliamentary systems (system == 3), 
# or would it be the president even if he has no powers? 
# In the code book it clearly states "chief executive", hence for 
# parliamentary systems it is the PM 

# Creating a new variable containing the vote share of the incumbent 
df$leg.executive.vote <- ifelse(df$legelec == 1 & df$execme.1jan == df$gov1me, df$gov1vote, 
                                ifelse(df$legelec == 1 & df$execme.1jan == df$gov2me, df$gov2vote, 
                                       ifelse(df$legelec == 1 & df$execme.1jan == df$gov3me, df$gov3vote, 
                                              ifelse(df$legelec == 1 & df$execme.1jan == df$opp1me, df$opp1vote, 
                                                     ifelse(df$legelec == 1 & df$execme.1jan == df$opp2me, df$opp2vote, 
                                                            ifelse(df$legelec == 1 & df$execme.1jan == df$opp3me, df$opp3vote, NA))))))

######################################
# Executive election 
######################################

summary(df$execme.1jan)
summary(df$execme)
summary(df$percent1) # first round
summary(df$percentl)
summary(df$dateexec)

# Identify change in government for chief executive: for me problematic cases,
# since I dont know how much the incumbent got
df$exec.election.change <- ifelse(df$exelec == 1 & df$execme.1jan != df$execme, 1, 0)
table(df$exec.election.change) 
df.exec.lost <- df[df$exec.election.change == 1, c("iso2c", "countryname", "year", "execme", "execme.1jan", "exec.election.change")]
rm(df.exec.lost)
# 194 cases in overall sample, with Kayser sample: 13

# Manually adding the incumbent performance if office is lost
df <- cbind(df, percent1.if.lost = NA)

# FR, IE, IL, PT (since some time) are considered parliamentary systems, 
# hence have no presidential election results in data set 

# there was no incumbent
# df[df$iso2c == "PT" & df$year == 1976, "percent1.if.lost"] <- NA 

# http://www.presidency.ucsb.edu/showelection.php?year=1976
# popular vote
# df[df$iso2c == "US" & df$year == 1976, "percent1.if.lost"] <- 48 

# http://www.presidency.ucsb.edu/showelection.php?year=1980
# popular vote 
# df[df$iso2c == "US" & df$year == 1980, "percent1.if.lost"] <- 41

# http://www.france-politique.fr/elections-legislatives-1981.htm
# df[df$iso2c == "FR" & df$year == 1981, "percent1.if.lost"] <- 19.2

# http://www.france-politique.fr/elections-legislatives-1988.htm
# df[df$iso2c == "FR" & df$year == 1988, "percent1.if.lost"] <- 19.18

# http://www.presidency.ucsb.edu/showelection.php?year=1992
# popular vote 
# df[df$iso2c == "US" & df$year == 1992, "percent1.if.lost"] <- 37.4

# Israel is a parliamentary system: hence no presidential elections 
# df[df$iso2c == "IL" & df$year == 1996, "percent1.if.lost"] <- NA # test
# df[df$iso2c == "IL" & df$year == 1999, "percent1.if.lost"] <- NA # test

# http://www.presidency.ucsb.edu/showelection.php?year=2000
# popular vote
# df[df$iso2c == "US" & df$year == 2000, "percent1.if.lost"] <- 48.4

# http://eed.nsd.uib.no/webview/index.jsp?study=http://129.177.90.166:80/obj/fStudy/FRPA20021_Display&mode=cube&v=2&cube=http://129.177.90.166:80/obj/fCube/FRPA20021_Display_C1&top=yes
# Many candidates ran (and this was first round)
# df[df$iso2c == "FR" & df$year == 2002, "percent1.if.lost"] <- 23.78

# http://www.presidency.ucsb.edu/showelection.php?year=2008
# popular vote 
# df[df$iso2c == "US" & df$year == 2008, "percent1.if.lost"] <- 45.7

# http://eed.nsd.uib.no/webview/index.jsp?headers=par_name&par_nameslice=5&stubs=round&stubs=nuts_id&measure=common&virtualslice=pv_p_value&nuts_idsubset=FR1+-+FRZ&layers=virtual&study=http%3A%2F%2F129.177.90.166%3A80%2Fobj%2FfStudy%2FFRPR2012&mode=cube&nuts_idslice=FR1&virtualsubset=pv_p_value&v=2&roundsubset=1+-+2&measuretype=4&roundslice=1&cube=http%3A%2F%2F129.177.90.166%3A80%2Fobj%2FfCube%2FFRPR2012_C1&par_namesubset=5%2C7+-+8%2C10&top=yes
# no candidate ran for the conservative party Fianna Fail
# df[df$iso2c == "IE" & df$year == 2011, "percent1.if.lost"] <- 0

# http://eed.nsd.uib.no/webview/index.jsp?headers=par_name&par_nameslice=5&stubs=round&stubs=nuts_id&measure=common&virtualslice=pv_p_value&nuts_idsubset=FR1+-+FRZ&layers=virtual&study=http%3A%2F%2F129.177.90.166%3A80%2Fobj%2FfStudy%2FFRPR2012&mode=cube&nuts_idslice=FR1&virtualsubset=pv_p_value&v=2&roundsubset=1+-+2&measuretype=4&roundslice=1&cube=http%3A%2F%2F129.177.90.166%3A80%2Fobj%2FfCube%2FFRPR2012_C1&par_namesubset=5%2C7+-+8%2C10&top=yes
# df[df$iso2c == "PT" & df$year == 2011, "percent1.if.lost"] <- 19.76

# Vote for the incumbent executive 
df$exec.vote <- ifelse(df$exelec == 1 & df$execme.1jan == df$execme, # Reelection
                       df$percent1,
                       ifelse(df$exelec == 1 & df$execme.1jan != df$execme, df$percent1.if.lost, NA)) # Need to change this in the future

# So far percent1.if.lost is not used
# Problem with percent1.if.lost in PLM regression
df <- df[,names(df) != "percent1.if.lost"]

#####################################
# Integrating parliamentary and executive elections
#####################################

# Do I have obervations with both elections occuring at the same time? 
table(df$legelec == 1 & df$exelec == 1) # 20 elections with Kayser sample

# Solutions: just using one election, or shifting one elction into another year 
# (problematic since the economic variables need to relate to the election)

# df$election <- ifelse(df$legelec == 1 & df$exelec == 0, df$leg.executive.vote, # I could also use other measure
#                        ifelse(df$legelec == 0 & df$exelec == 1, df$exec.vote,
#                               ifelse(df$legelec == 1 & df$exelec == 1, df$exec.vote, NA)))
