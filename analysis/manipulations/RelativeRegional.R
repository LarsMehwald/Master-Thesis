######################################
# Master thesis
# Lars Mehwald
# Data manipulations: Relative economic variables, regional
# 4 April 2016
######################################

######################################
# Deviation from regional variables: growth (adjusted)
######################################

# Creation of continent and regional variable
df$continent <- countrycode(df$iso2c,
                            origin = "iso2c",
                            destination = "continent",
                            warn = TRUE)
df$region <- df$continent
df <- df[,names(df) != "continent"]
# df$region <- countrycode(df$iso2c, 
#                          origin = "iso2c",
#                          destination = "region", 
#                          warn = TRUE)

# Create median regional growth into new data frame
df.regional <- aggregate(df$GDPgrowth, 
                         by=list(df$region, df$year), 
                         FUN=median, 
                         na.rm=TRUE)
names(df.regional) <- c("region", "year", "GDPgrowth.regional")

# Getting rid of NaN coded NAs 
df.regional$GDPgrowth.regional <- gsub("NaN", NA, df.regional$GDPgrowth.regional)

# Create new data frame for previous regional growth rate 
df.regional.previous <- data.frame(region = as.character(df.regional$region),
                                   year = as.numeric(as.character(df.regional$year)) + 1,
                                   GDPgrowth.regional.previous = df.regional$GDPgrowth.regional)

# Merge it with the original data frame 
df <- merge(df, df.regional, by=c("region", "year"), all.x = TRUE, all.y = FALSE)
df <- merge(df, df.regional.previous, by=c("region", "year"), all.x = TRUE, all.y = FALSE)

# Create relative economic measurement (adjusted)
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.regional.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.regional.previous"]))
    df[i, "GDPgrowth.regional.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.regional.previous"]))
  }
  else {
    df[i, "GDPgrowth.regional.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.regional"]))
    df[i, "GDPgrowth.regional.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.regional"]))
  }
}

# Remove the intermediary variables
rm(df.regional, df.regional.previous, i)
df <- df[,names(df) != "GDPgrowth.regional"]
df <- df[,names(df) != "GDPgrowth.regional.previous"]

######################################
# Deviation from regional variables: unemployment.fd (adjusted)
######################################

# Create median regional unemployment rate into new data frame
df.regional <- aggregate(df$unemployment.fd, 
                         by=list(df$region, df$year), 
                         FUN=median, 
                         na.rm=TRUE)
names(df.regional) <- c("region", "year", "unemployment.fd.regional")

# Getting rid of NaN coded NAs 
df.regional$unemployment.fd.regional <- gsub("NaN", NA, df.regional$unemployment.fd.regional)

# Create new data frame for previous regional growth rate 
df.regional.previous <- data.frame(region = as.character(df.regional$region),
                                   year = as.numeric(as.character(df.regional$year)) + 1,
                                   unemployment.fd.regional.previous = df.regional$unemployment.fd.regional)

# Merge it with the original data frame 
df <- merge(df, df.regional, by=c("region", "year"), all.x = TRUE, all.y = FALSE)
df <- merge(df, df.regional.previous, by=c("region", "year"), all.x = TRUE, all.y = FALSE)

# Create relative economic measurement (adjusted)
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "ue.fd.regional.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.fd.regional.previous"]))
    df[i, "ue.fd.regional.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.regional.previous"]))
  }
  else {
    df[i, "ue.fd.regional.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.fd.regional"]))
    df[i, "ue.fd.regional.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.regional"]))
  }
}

# Remove the intermediary variables
rm(df.regional, df.regional.previous, i)
df <- df[,names(df) != "unemployment.fd.regional"]
df <- df[,names(df) != "unemployment.fd.regional.previous"]
