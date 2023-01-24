######################################
# Master thesis
# Lars Mehwald
# Bordering Countries 
# 4 April 2016
######################################

######################################
# Create variable with all relevant iso2c names
######################################

iso2c <- df$iso2c
iso2c <- as.data.frame(unique(iso2c))
names(iso2c) <- "iso2c"

# NA has caused problem: delete
iso2c <- as.data.frame(iso2c[!is.na(iso2c$iso2c),])
names(iso2c) <- "iso2c"

######################################
# Download bordering countries
######################################

# URL <- "http://api.geonames.org/neighboursJSON?country=XX&username=larsunterwegs"

BorderingCountries <- data.frame()

# Create function

for (i in 1:length(iso2c$iso2c)) {
  
  # Create link
  URL <- paste("http://api.geonames.org/neighboursJSON?country=", 
               iso2c[i,1], 
               "&username=larsunterwegs",
               sep = "")
  
  # Retrive content 
  bor <- GET(URL)
  bor <- content(bor, as = "parsed")
  
  # Problem with addtional vector in list (realised it with DK, i=8)
  # bor <- bor[names(bor) !="totalResultsCount"]
  
  # How many vectors are in the list? Russia has 14 bordering countries 
  # length.list <- bor$totalResultsCount
  BorderingCountries.temp <- data.frame(iso2c = NA) # Create one row so that characters can be added
  
  if (length(bor) >= 2 ){
  
  BorderingCountries.temp$iso2c <- as.character(iso2c[i,1])
  BorderingCountries.temp$Neighbor1 <- ifelse(bor$totalResultsCount >= 1, bor$geonames[[1]]$countryCode, NA) 
  BorderingCountries.temp$Neighbor2 <- ifelse(bor$totalResultsCount >= 2, bor$geonames[[2]]$countryCode, NA)
  BorderingCountries.temp$Neighbor3 <- ifelse(bor$totalResultsCount >= 3, bor$geonames[[3]]$countryCode, NA)
  BorderingCountries.temp$Neighbor4 <- ifelse(bor$totalResultsCount >= 4, bor$geonames[[4]]$countryCode, NA)
  BorderingCountries.temp$Neighbor5 <- ifelse(bor$totalResultsCount >= 5, bor$geonames[[5]]$countryCode, NA)
  BorderingCountries.temp$Neighbor6 <- ifelse(bor$totalResultsCount >= 6, bor$geonames[[6]]$countryCode, NA)
  BorderingCountries.temp$Neighbor7 <- ifelse(bor$totalResultsCount >= 7, bor$geonames[[7]]$countryCode, NA)
  BorderingCountries.temp$Neighbor8 <- ifelse(bor$totalResultsCount >= 8, bor$geonames[[8]]$countryCode, NA)
  BorderingCountries.temp$Neighbor9 <- ifelse(bor$totalResultsCount >= 9, bor$geonames[[9]]$countryCode, NA)
  BorderingCountries.temp$Neighbor10 <- ifelse(bor$totalResultsCount >= 10, bor$geonames[[10]]$countryCode, NA)
  BorderingCountries.temp$Neighbor11 <- ifelse(bor$totalResultsCount >= 11, bor$geonames[[11]]$countryCode, NA)
  BorderingCountries.temp$Neighbor12 <- ifelse(bor$totalResultsCount >= 12, bor$geonames[[12]]$countryCode, NA)
  BorderingCountries.temp$Neighbor13 <- ifelse(bor$totalResultsCount >= 13, bor$geonames[[13]]$countryCode, NA)
  BorderingCountries.temp$Neighbor14 <- ifelse(bor$totalResultsCount >= 14, bor$geonames[[14]]$countryCode, NA)
  
  BorderingCountries.temp$Border.success <- 1
  
  }
  
  else {
    BorderingCountries.temp$iso2c <- as.character(iso2c[i,1])
    BorderingCountries.temp$Neighbor1 <- NA
    BorderingCountries.temp$Neighbor2 <- NA
    BorderingCountries.temp$Neighbor3 <- NA
    BorderingCountries.temp$Neighbor4 <- NA
    BorderingCountries.temp$Neighbor5 <- NA
    BorderingCountries.temp$Neighbor6 <- NA
    BorderingCountries.temp$Neighbor7 <- NA
    BorderingCountries.temp$Neighbor8 <- NA
    BorderingCountries.temp$Neighbor9 <- NA
    BorderingCountries.temp$Neighbor10 <- NA
    BorderingCountries.temp$Neighbor11 <- NA
    BorderingCountries.temp$Neighbor12 <- NA
    BorderingCountries.temp$Neighbor13 <- NA
    BorderingCountries.temp$Neighbor14 <- NA
    
    BorderingCountries.temp$Border.success <- 0
    
  }

  BorderingCountries <- rbind(BorderingCountries, BorderingCountries.temp)
  
}

######################################
# Data manipulation
######################################

# Merge with original df 
df <- merge(df, BorderingCountries, by = "iso2c", all = TRUE)

# Remove intermediary objects
rm(URL, i, bor, iso2c, BorderingCountries, BorderingCountries.temp)

######################################
# Create relative economic measurements (and adjusted): growth
######################################

df.GDPgrowth <- df[, c("iso2c", "year", "GDPgrowth")]

for (i in 1:14) {
  
  Neighbor <- paste("Neighbor", i, sep = "")
  
  names(df.GDPgrowth) <- c(Neighbor, 
                           "year", 
                           paste("GDPgrowth.Neighbor", i, sep = ""))
  
  df <- merge(df, 
              df.GDPgrowth, 
              by = c(Neighbor, "year"),
              all.y = FALSE,
              all.x = TRUE)
  
  # This is necessary, because otherwise the GDP growth of Namibia (attributed to other countries with NAs) is kept 
  if (i == 1){df[is.na(df$Neighbor1) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 2){df[is.na(df$Neighbor2) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 3){df[is.na(df$Neighbor3) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 4){df[is.na(df$Neighbor4) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 5){df[is.na(df$Neighbor5) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 6){df[is.na(df$Neighbor6) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 7){df[is.na(df$Neighbor7) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 8){df[is.na(df$Neighbor8) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 9){df[is.na(df$Neighbor9) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 10){df[is.na(df$Neighbor10) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 11){df[is.na(df$Neighbor11) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 12){df[is.na(df$Neighbor12) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 13){df[is.na(df$Neighbor13) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  if (i == 14){df[is.na(df$Neighbor14) == TRUE, paste("GDPgrowth.Neighbor", i, sep = "")] <- NA}
  
}

rm(df.GDPgrowth,i,Neighbor)

# Average bordering growth rate, islands are assigned NaN
df$GDPgrowth.average <- rowMeans(df[, names(df) == "GDPgrowth.Neighbor1" | 
                                      names(df) == "GDPgrowth.Neighbor2" |
                                      names(df) == "GDPgrowth.Neighbor3" |
                                      names(df) == "GDPgrowth.Neighbor4" |
                                      names(df) == "GDPgrowth.Neighbor5" |
                                      names(df) == "GDPgrowth.Neighbor6" |
                                      names(df) == "GDPgrowth.Neighbor7" |
                                      names(df) == "GDPgrowth.Neighbor8" |
                                      names(df) == "GDPgrowth.Neighbor9" |
                                      names(df) == "GDPgrowth.Neighbor10" |
                                      names(df) == "GDPgrowth.Neighbor11" |
                                      names(df) == "GDPgrowth.Neighbor12" |
                                      names(df) == "GDPgrowth.Neighbor13" |
                                      names(df) == "GDPgrowth.Neighbor14" ], 
                                 na.rm = TRUE)

# Island countries (Australia, Japan, New Zealand):
# They now are assigned NAs, I want to change it to 0,
# so I can calculate the bordering growth rate 
df$GDPgrowth.average[df$Border.success == 1 & df$GDPgrowth.average == "NaN"] <- 0
df$GDPgrowth.average[df$Border.success == 0] <- NA

# Creating values for economic data of the last year
df.econ.average.previous <- as.data.frame(cbind(iso2c = df$iso2c,
                                                year = df$year + 1,
                                                GDPgrowth.average.previous = df$GDPgrowth.average))

# Merging the data frames
df.econ.average.previous$iso2c <- as.character(df.econ.average.previous$iso2c)
df.econ.average.previous$year <- as.numeric(as.character(df.econ.average.previous$year))
df <- merge(df, df.econ.average.previous, by=c("iso2c","year"), all.x = TRUE, all.y = FALSE)
rm(df.econ.average.previous)

# Deviating growth rate 
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.bordering.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.average.previous"]))
    df[i, "GDPgrowth.bordering.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.average.previous"]))
  }
  else {
    df[i, "GDPgrowth.bordering.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.average"]))
    df[i, "GDPgrowth.bordering.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.average"]))
  }
}

rm(i)

######################################
# Create relative economic measurements (and adjusted): unemployment.fd
######################################

df.unemployment.fd <- df[, c("iso2c", "year", "unemployment.fd")]

for (i in 1:14) {
  
  Neighbor <- paste("Neighbor", i, sep = "")
  
  names(df.unemployment.fd) <- c(Neighbor, 
                                 "year", 
                                 paste("unemployment.fd.Neighbor", i, sep = ""))
  
  df <- merge(df, 
              df.unemployment.fd, 
              by = c(Neighbor, "year"),
              #              all.y = TRUE,
              all.x = TRUE)
  
  # This is necessary, because otherwise unemployment.fd of Namibia (attributed to other countries with NAs) is kept 
  if (i == 1){df[is.na(df$Neighbor1) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 2){df[is.na(df$Neighbor2) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 3){df[is.na(df$Neighbor3) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 4){df[is.na(df$Neighbor4) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 5){df[is.na(df$Neighbor5) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 6){df[is.na(df$Neighbor6) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 7){df[is.na(df$Neighbor7) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 8){df[is.na(df$Neighbor8) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 9){df[is.na(df$Neighbor9) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 10){df[is.na(df$Neighbor10) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 11){df[is.na(df$Neighbor11) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 12){df[is.na(df$Neighbor12) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 13){df[is.na(df$Neighbor13) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  if (i == 14){df[is.na(df$Neighbor14) == TRUE, paste("unemployment.fd.Neighbor", i, sep = "")] <- NA}
  
}

rm(df.unemployment.fd,i,Neighbor)

# Average bordering unemployment.fd, islands are assigned NaN
df$unemployment.fd.average <- rowMeans(df[, names(df) == "unemployment.fd.Neighbor1" | 
                                            names(df) == "unemployment.fd.Neighbor2" |
                                            names(df) == "unemployment.fd.Neighbor3" |
                                            names(df) == "unemployment.fd.Neighbor4" |
                                            names(df) == "unemployment.fd.Neighbor5" |
                                            names(df) == "unemployment.fd.Neighbor6" |
                                            names(df) == "unemployment.fd.Neighbor7" |
                                            names(df) == "unemployment.fd.Neighbor8" |
                                            names(df) == "unemployment.fd.Neighbor9" |
                                            names(df) == "unemployment.fd.Neighbor10" |
                                            names(df) == "unemployment.fd.Neighbor11" |
                                            names(df) == "unemployment.fd.Neighbor12" |
                                            names(df) == "unemployment.fd.Neighbor13" |
                                            names(df) == "unemployment.fd.Neighbor14" ], 
                                       na.rm = TRUE)

# Island countries (Australia, Japan, New Zealand):
# They now are assigned NAs, I want to change it to 0,
# so I can calculate the bordering unemployment.fd rate 
df$unemployment.fd.average[df$Border.success == 1 & df$unemployment.fd.average == "NaN"] <- 0
df$unemployment.fd.average[df$Border.success == 0] <- NA

# Creating values for economic data of the last year
df.econ.average.previous <- as.data.frame(cbind(iso2c = df$iso2c,
                                                year = df$year + 1,
                                                unemployment.fd.average.previous = df$unemployment.fd.average))

# Merging the data frames
df.econ.average.previous$iso2c <- as.character(df.econ.average.previous$iso2c)
df.econ.average.previous$year <- as.numeric(as.character(df.econ.average.previous$year))
df <- merge(df, df.econ.average.previous, by=c("iso2c","year"), all.x = TRUE, all.y = FALSE)
rm(df.econ.average.previous)

# Deviating unemployment.fd rate 
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "ue.fd.bordering.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.fd.average.previous"]))
    df[i, "ue.fd.bordering.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.average.previous"]))
  }
  else {
    df[i, "ue.fd.bordering.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.fd.average"]))
    df[i, "ue.fd.bordering.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.average"]))
  }
}

rm(i)

######################################
# Removing additional variables
######################################

df <- df[, names(df) != "GDPgrowth.average"]
df <- df[, names(df) != "GDPgrowth.average.previous"]
df <- df[, names(df) != "unemployment.fd.average"]
df <- df[, names(df) != "unemployment.fd.average.previous"]
df <- df[, names(df) != "Border.success"]

df <- df[, names(df) != "GDPgrowth.Neighbor1"]
df <- df[, names(df) != "unemployment.fd.Neighbor1"]
df <- df[, names(df) != "Neighbor1"]
df <- df[, names(df) != "GDPgrowth.Neighbor2"]
df <- df[, names(df) != "unemployment.fd.Neighbor2"]
df <- df[, names(df) != "Neighbor2"]
df <- df[, names(df) != "GDPgrowth.Neighbor3"]
df <- df[, names(df) != "unemployment.fd.Neighbor3"]
df <- df[, names(df) != "Neighbor3"]
df <- df[, names(df) != "GDPgrowth.Neighbor4"]
df <- df[, names(df) != "unemployment.fd.Neighbor4"]
df <- df[, names(df) != "Neighbor4"]
df <- df[, names(df) != "GDPgrowth.Neighbor5"]
df <- df[, names(df) != "unemployment.fd.Neighbor5"]
df <- df[, names(df) != "Neighbor5"]
df <- df[, names(df) != "GDPgrowth.Neighbor6"]
df <- df[, names(df) != "unemployment.fd.Neighbor6"]
df <- df[, names(df) != "Neighbor6"]
df <- df[, names(df) != "GDPgrowth.Neighbor7"]
df <- df[, names(df) != "unemployment.fd.Neighbor7"]
df <- df[, names(df) != "Neighbor7"]
df <- df[, names(df) != "GDPgrowth.Neighbor8"]
df <- df[, names(df) != "unemployment.fd.Neighbor8"]
df <- df[, names(df) != "Neighbor8"]
df <- df[, names(df) != "GDPgrowth.Neighbor9"]
df <- df[, names(df) != "unemployment.fd.Neighbor9"]
df <- df[, names(df) != "Neighbor9"]
df <- df[, names(df) != "GDPgrowth.Neighbor10"]
df <- df[, names(df) != "unemployment.fd.Neighbor10"]
df <- df[, names(df) != "Neighbor10"]
df <- df[, names(df) != "GDPgrowth.Neighbor11"]
df <- df[, names(df) != "unemployment.fd.Neighbor11"]
df <- df[, names(df) != "Neighbor11"]
df <- df[, names(df) != "GDPgrowth.Neighbor12"]
df <- df[, names(df) != "unemployment.fd.Neighbor12"]
df <- df[, names(df) != "Neighbor12"]
df <- df[, names(df) != "GDPgrowth.Neighbor13"]
df <- df[, names(df) != "unemployment.fd.Neighbor13"]
df <- df[, names(df) != "Neighbor13"]
df <- df[, names(df) != "GDPgrowth.Neighbor14"]
df <- df[, names(df) != "unemployment.fd.Neighbor14"]
df <- df[, names(df) != "Neighbor14"]
