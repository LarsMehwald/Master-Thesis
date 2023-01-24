######################################
# Master thesis
# Lars Mehwald
# Data manipulations: Relative economic variables, time adjustment 
# 4 April 2016
######################################

#####################################
# Economic variable close to election (adjusted)
####### ##############################

# For the first half of the year, the economic data from the previous year is used
# For the second half, the economic data from the year of the election is used 

# Did the election occur in the first half of the year? 1=first half, 2=second half
df$dateleg.half <- ifelse(df$dateleg <= 6, 1, 2)

# Creating values for economic data of the last year 
df.econ.previous <- as.data.frame(cbind(iso2c = df$iso2c, 
                                        year = df$year + 1, 
                                        GDPgrowth.previous = df$GDPgrowth, 
                                        unemployment.fd.previous = df$unemployment.fd))

# Merging the data frames 
df.econ.previous$iso2c <- as.character(df.econ.previous$iso2c)
df.econ.previous$year <- as.numeric(as.character(df.econ.previous$year))
df <- merge(df, df.econ.previous, by=c("iso2c","year"), all.x = TRUE, all.y = FALSE)
rm(df.econ.previous)

# Creating adjusted economic variables
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) 
  }
  else {
    df[i, "GDPgrowth.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"]))
  }
}

for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "ue.fd.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) 
  }
  else {
    df[i, "ue.fd.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"]))
  }
}

# Removing intermediate variables - it is better to keep them till the end for other calculations 
# df <- df[, names(df) !="GDPgrowth.previous"]
# df <- df[, names(df) !="unemployment.fd.previous"]
rm(i)
