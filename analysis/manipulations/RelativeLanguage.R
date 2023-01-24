######################################
# Master thesis
# Lars Mehwald
# Data gathering: Language
# 4 April 2016
######################################

# http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=19

######################################
# Prepare data
######################################

# It is not possible to download the data dynamically from the web, since a login is required

CEPIIlanguage <- read.dta("analysis/data/rawdata/Language/ling_web.dta")

# Saving the raw data
# write.csv(CEPIIlanguage, file = "analysis/data/rawdata/CEPIIlanguage.csv")

# Solve problem of unified Belgium and Luxembourg
BelLux <- rbind(CEPIIlanguage[CEPIIlanguage$iso_o == "BLX",],
                CEPIIlanguage[CEPIIlanguage$iso_d == "BLX",])
BelLux$iso_o <- gsub("BLX", "BEL", BelLux$iso_o)
BelLux$iso_d <- gsub("BLX", "BEL", BelLux$iso_d)
CEPIIlanguage$iso_o <- gsub("BLX", "LUX", CEPIIlanguage$iso_o)
CEPIIlanguage$iso_d <- gsub("BLX", "LUX", CEPIIlanguage$iso_d)
CEPIIlanguage <- rbind(CEPIIlanguage, BelLux)
rm(BelLux)

# Change country code to iso2c
CEPIIlanguage$iso2c.o <- countrycode(CEPIIlanguage$iso_o, 
                                     origin = "iso3c",
                                     destination = "iso2c", 
                                     warn = TRUE)
CEPIIlanguage$iso2c.d <- countrycode(CEPIIlanguage$iso_d, 
                                     origin = "iso3c",
                                     destination = "iso2c", 
                                     warn = TRUE)

# Renaming key variables 
# prox1 and prox2 are used as intermediary variables 
names(CEPIIlanguage)[names(CEPIIlanguage) == "col"] <- "common.official.language"
names(CEPIIlanguage)[names(CEPIIlanguage) == "csl"] <- "common.spoken.language"
names(CEPIIlanguage)[names(CEPIIlanguage) == "cnl"] <- "common.native.language"
names(CEPIIlanguage)[names(CEPIIlanguage) == "lp1"] <- "linguistic.proximity.1"
names(CEPIIlanguage)[names(CEPIIlanguage) == "lp2"] <- "linguistic.proximity.2"
names(CEPIIlanguage)[names(CEPIIlanguage) == "cl"] <- "common.language.index"

# Keeping most relevant variables 
CEPIIlanguage <- CEPIIlanguage[, c("iso2c.o", "iso2c.d", "linguistic.proximity.1")]

# Rename key variable (linguistic.proximity)
names(CEPIIlanguage)[names(CEPIIlanguage) == "linguistic.proximity.1"] <- "linguistic.proximity"

# Adding LU-BE and BE-LU to the data frame 
CEPIIlanguage <- rbind(CEPIIlanguage,
                       c("BE", "LU", 0),
                       c("LU", "BE", 0))
row.names(CEPIIlanguage) <- NULL

# Reshape into wide format (to obtain a matrix)
CEPIIlanguage <- reshape(CEPIIlanguage,
                         idvar = "iso2c.o",
                         timevar = "iso2c.d",
                         direction = "wide")
names(CEPIIlanguage) <- gsub("linguistic.proximity.", "", names(CEPIIlanguage))

# Change the row names 
row.names(CEPIIlanguage) <- CEPIIlanguage$iso2c.o
CEPIIlanguage <- CEPIIlanguage[names(CEPIIlanguage) != "iso2c.o"]

# Sort the variable names and the row names 
CEPIIlanguage <- CEPIIlanguage[,order(names(CEPIIlanguage))]
CEPIIlanguage <- CEPIIlanguage[order(row.names(CEPIIlanguage)),]

# Turn the diagonale into NA
for(i in 1:length(CEPIIlanguage)){
  CEPIIlanguage[i,i] <- NA
}
rm(i)

# Checking the coding of proximity for similary countries
# temp <- CEPIIlanguage[row.names(CEPIIlanguage) == "AU" | row.names(CEPIIlanguage) == "GB" | row.names(CEPIIlanguage) == "US" |
#                         row.names(CEPIIlanguage) == "NZ" | row.names(CEPIIlanguage) == "CA" | row.names(CEPIIlanguage) == "IE", 
#                       c("AU", "GB", "US", "NZ", "CA", "IE")]

######################################
# Compute the 10 countries with highest proximity
######################################

# Inserting 10 new variables into df (country name)
language.names <- c("language.1.name", "language.2.name", "language.3.name", "language.4.name", "language.5.name", 
                   "language.6.name", "language.7.name", "language.8.name", "language.9.name", "language.10.name")
df[,language.names] <- NA

# Nested loop to create many subsets and transfer the values
# for each subset into the main data frame
for (i in 1:length(CEPIIlanguage)) {
  temp <- CEPIIlanguage[i,]
  temp <- sort(temp, decreasing = FALSE)
  temp <- temp[,1:10]
  for (j in 1:10) {
    df[df$iso2c == row.names(temp), language.names[j] ] <- names(temp)[j]
  }
}

# Remove intermediary variables 
rm(i, j, language.names, temp)
rm(CEPIIlanguage)

######################################
# Relative economic performance: GDP growth (adjusted)
######################################

df.growth.rates <- df[,c("iso2c", "year", "GDPgrowth")]

# Merging all 10 growth rates
for (i in 1:10) { 
  names(df.growth.rates) <- c(paste("language.", i, ".name", sep = ""),
                              "year",
                              paste("language.", i, ".GDP.growth", sep = ""))
  df <- merge(df, 
              df.growth.rates, 
              by = c(paste("language.", i, ".name", sep = ""), 
                     "year"),
              all.y = FALSE,
              all.x = TRUE)
}

# Remove intermediary objects
rm(i, df.growth.rates)

# Mean growth rate
df$GDPgrowth.average <- rowMeans(df[, c("language.1.GDP.growth", "language.2.GDP.growth", 
                                        "language.3.GDP.growth", "language.4.GDP.growth", 
                                        "language.5.GDP.growth", "language.6.GDP.growth", 
                                        "language.7.GDP.growth", "language.8.GDP.growth", 
                                        "language.9.GDP.growth", "language.10.GDP.growth")], 
                                 na.rm = TRUE)

# Creating values for economic data of the last year
df.econ.average.previous <- as.data.frame(cbind(iso2c = df$iso2c,
                                                year = df$year + 1,
                                                GDPgrowth.average.previous = df$GDPgrowth.average))

# Merging the data frames
df.econ.average.previous$iso2c <- as.character(df.econ.average.previous$iso2c)
df.econ.average.previous$year <- as.numeric(as.character(df.econ.average.previous$year))
df <- merge(df, df.econ.average.previous, 
            by=c("iso2c","year"), 
            all.x = TRUE, 
            all.y = FALSE)
rm(df.econ.average.previous)

# Deviating growth rate 
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.language.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.average.previous"]))
    df[i, "GDPgrowth.language.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.average.previous"]))
  }
  else {
    df[i, "GDPgrowth.language.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.average"]))
    df[i, "GDPgrowth.language.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.average"]))
  }
}

rm(i)

######################################
# Relative economic performance: unemployment.fd (adjusted)
######################################

df.unemployment.fd <- df[,c("iso2c", "year", "unemployment.fd")]

# Merging all 10 growth rates
for (i in 1:10) { 
  names(df.unemployment.fd) <- c(paste("language.", i, ".name", sep = ""),
                                 "year",
                                 paste("language.", i, ".unemployment.fd", sep = ""))
  df <- merge(df, 
              df.unemployment.fd, 
              by = c(paste("language.", i, ".name", sep = ""), 
                     "year"),
              #all.y = TRUE,
              all.x = TRUE)
}

# Remove intermediary objects
rm(i, df.unemployment.fd)

# Mean unemployment.fd rate
df$unemployment.fd.average <- rowMeans(df[, c("language.1.unemployment.fd", "language.2.unemployment.fd", 
                                              "language.3.unemployment.fd", "language.4.unemployment.fd", 
                                              "language.5.unemployment.fd", "language.6.unemployment.fd", 
                                              "language.7.unemployment.fd", "language.8.unemployment.fd", 
                                              "language.9.unemployment.fd", "language.10.unemployment.fd")], 
                                       na.rm = TRUE)

# Creating values for economic data of the last year
df.econ.average.previous <- as.data.frame(cbind(iso2c = df$iso2c,
                                                year = df$year + 1,
                                                unemployment.fd.average.previous = df$unemployment.fd.average))

# Merging the data frames
df.econ.average.previous$iso2c <- as.character(df.econ.average.previous$iso2c)
df.econ.average.previous$year <- as.numeric(as.character(df.econ.average.previous$year))
df <- merge(df, df.econ.average.previous, 
            by=c("iso2c","year"), 
            all.x = TRUE, 
            all.y = FALSE)
rm(df.econ.average.previous)

# Deviating unemployment.fd rate 
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "ue.fd.language.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.fd.average.previous"]))
    df[i, "ue.fd.language.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.average.previous"]))
  }
  else {
    df[i, "ue.fd.language.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.fd.average"]))
    df[i, "ue.fd.language.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.average"]))
  }
}

rm(i)

######################################
# Remove intermediary variables
######################################

df <- df[, names(df) != "language.1.name"]
df <- df[, names(df) != "language.2.name"]
df <- df[, names(df) != "language.3.name"]
df <- df[, names(df) != "language.4.name"]
df <- df[, names(df) != "language.5.name"]
df <- df[, names(df) != "language.6.name"]
df <- df[, names(df) != "language.7.name"]
df <- df[, names(df) != "language.8.name"]
df <- df[, names(df) != "language.9.name"]
df <- df[, names(df) != "language.10.name"]

df <- df[, names(df) != "language.1.unemployment.fd"]
df <- df[, names(df) != "language.2.unemployment.fd"]
df <- df[, names(df) != "language.3.unemployment.fd"]
df <- df[, names(df) != "language.4.unemployment.fd"]
df <- df[, names(df) != "language.5.unemployment.fd"]
df <- df[, names(df) != "language.6.unemployment.fd"]
df <- df[, names(df) != "language.7.unemployment.fd"]
df <- df[, names(df) != "language.8.unemployment.fd"]
df <- df[, names(df) != "language.9.unemployment.fd"]
df <- df[, names(df) != "language.10.unemployment.fd"]

df <- df[, names(df) != "language.1.GDP.growth"]
df <- df[, names(df) != "language.2.GDP.growth"]
df <- df[, names(df) != "language.3.GDP.growth"]
df <- df[, names(df) != "language.4.GDP.growth"]
df <- df[, names(df) != "language.5.GDP.growth"]
df <- df[, names(df) != "language.6.GDP.growth"]
df <- df[, names(df) != "language.7.GDP.growth"]
df <- df[, names(df) != "language.8.GDP.growth"]
df <- df[, names(df) != "language.9.GDP.growth"]
df <- df[, names(df) != "language.10.GDP.growth"]

df <- df[, names(df) != "GDPgrowth.average"]
df <- df[, names(df) != "GDPgrowth.average.previous"]
df <- df[, names(df) != "unemployment.fd.average"]
df <- df[, names(df) != "unemployment.fd.average.previous"]
