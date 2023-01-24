######################################
# Master thesis
# Lars Mehwald
# Distance between capitals 
# 4 April 2016
######################################

######################################
# Download capitals of countries
######################################

# Empty data frame to add capitals to
Capital <- data.frame(iso2c = as.character(), capital = as.character())

# Loop to gather capitals 
for (i in 1:length(unique(df$iso2c))) { 
  URL <- "http://api.geonames.org/countryInfo?country=XX&username=larsunterwegs"
  URL <- gsub("XX", unique(df$iso2c)[i], URL)
  temp <- xmlToDataFrame(URL)
  if(length(temp) == 18){
    Capital <- rbind(Capital, data.frame(iso2c = as.character(unique(df$iso2c)[i]),
                                         capital = temp[1,"capital"]))
  }
}

# PS (Palestine State), IL (Israel) missing, one country skipped (length == 18)
# Both assigned Jerusalem as capital 
Capital$iso2c <- as.character(Capital$iso2c)
Capital$capital <- as.character(Capital$capital)
Capital[Capital$iso2c == "IL", names(Capital) == "capital"] <- "Jerusalem"
Capital[Capital$iso2c == "PS", names(Capital) == "capital"] <- "Jerusalem"

######################################
# Download coordinates of capitals 
######################################

# Setting the help objects for location of uni
number.row <- nrow(Capital)
counter <- 1
Capital$capital.lon <- 0
Capital$capital.lat <- 0

# Get coordinates for capitals
while (counter <= number.row){ 
  URL <- paste(
    "http://nominatim.openstreetmap.org/search?city=",
    Capital$capital[counter],
    "&countrycodes=",
    Capital$iso2c[counter],
    "&limit=9&format=json",
    sep="")
  x <- fromJSON(URL)
  if(is.list(x) & length(x) > 0){ # changed this from vector to list 
    Capital$capital.lon[counter] <- x[[1]]$lon
    Capital$capital.lat[counter] <- x[[1]]$lat    
  }
  else{
    Capital$capital.lon[counter] <- NA
    Capital$capital.lat[counter] <- NA    
    
  }
  counter <- counter + 1
}

# London is not downloaded properly (probably the iso2c code did not match)
# http://www.geonames.org/search.html?q=london&country=
Capital[Capital$iso2c == "GB", "capital.lon"] <- 0 + 7/60 + 32/3600
Capital[Capital$iso2c == "GB", "capital.lat"] <- 51 + 30/60 + 30/3600

# Problem: does not know PS for Palestine 
Capital[Capital$iso2c == "PS", "capital.lon"] <- Capital[Capital$iso2c == "IL", "capital.lon"]
Capital[Capital$iso2c == "PS", "capital.lat"] <- Capital[Capital$iso2c == "IL", "capital.lat"]

# Changing the class of the coordinates 
Capital$capital.lon <- as.numeric(as.character(Capital$capital.lon))
Capital$capital.lat <- as.numeric(as.character(Capital$capital.lat))

# Remove countries with NA as coordinates
# This saved to manually extract NAs in the matrix
# that contains the distance between the cities 
Capital <- Capital[is.na(Capital$capital.lon) == FALSE & is.na(Capital$capital.lat) == FALSE,]

# Remove intermediary objects 
rm(x, URL, counter, number.row)

######################################
# Calculate the distance between capitals  
######################################

# Create empty data frame (matrix like)
Capital.distance <- data.frame()
Capital.distance[1:length(Capital$iso2c),] <- NA
Capital.distance[,1:length(Capital$iso2c)] <- NA
row.names(Capital.distance) <- Capital$iso2c
colnames(Capital.distance) <- Capital$iso2c

# Calculating the distance (in meters), column by column
for (i in 1:length(Capital.distance)) {
  Capital.distance[,i] <- distGeo(p1 = data.matrix(Capital[names(Capital) == "capital.lon" | names(Capital) == "capital.lat"]), 
                                  p2 = c(Capital[i,"capital.lon"], Capital[i, "capital.lat"]))
}

# Turn the diagonale into NA
for(i in 1:length(Capital.distance)){
  Capital.distance[i,i] <- NA
}

# Transforming the distance from meters to km 
Capital.distance <- Capital.distance / 1000 

######################################
# Calculating 10 closest countries 
######################################

# Closest.dist is not used any more, because I am not calculating a distance weighted growth rate

# Inserting 20 new variables into df (country name and distance)
closest.names <- c("closest.1.name", "closest.2.name", "closest.3.name", "closest.4.name", "closest.5.name", 
                   "closest.6.name", "closest.7.name", "closest.8.name", "closest.9.name", "closest.10.name")
# closest.dist <- c("closest.1.dist", "closest.2.dist", "closest.3.dist", "closest.4.dist", "closest.5.dist", 
#                   "closest.6.dist", "closest.7.dist", "closest.8.dist", "closest.9.dist", "closest.10.dist")
df[,closest.names] <- NA
# df[, closest.dist] <- NA

# Nested loop to create many subsets and transfer the values
# for each subset into the main data frame
for (i in 1:length(Capital.distance)) {
  temp <- Capital.distance[i,]
  temp <- sort(temp, decreasing = FALSE)
  temp <- temp[,1:10]
  for (j in 1:10) {
    df[df$iso2c == row.names(temp), closest.names[j] ] <- names(temp)[j]
#    df[df$iso2c == row.names(temp), closest.dist[j] ] <- temp[1,j] # Not really used afterwards
  }
}

# Remove intermediary variables 
rm(i, j, closest.names, temp)
rm(Capital, Capital.distance)

######################################
# Calculating relative growth rate (adjusted)
######################################

# Adding yearly growth rates to closest countries
df.growth.rates <- df[,c("iso2c", "year", "GDPgrowth")]

# Merging all 10 growth rates
for (i in 1:10) { 
  names(df.growth.rates) <- c(paste("closest.", i, ".name", sep = ""),
                              "year",
                              paste("closest.", i, ".GDP.growth", sep = ""))
  df <- merge(df, 
              df.growth.rates, 
              by = c(paste("closest.", i, ".name", sep = ""), 
                     "year"),
              #all.y = TRUE,
              all.x = TRUE)
}

# Remove intermediary objects
rm(i, df.growth.rates)

# Mean growth rate
df$GDPgrowth.average <- rowMeans(df[, c("closest.1.GDP.growth", "closest.2.GDP.growth", 
                                        "closest.3.GDP.growth", "closest.4.GDP.growth", 
                                        "closest.5.GDP.growth", "closest.6.GDP.growth", 
                                        "closest.7.GDP.growth", "closest.8.GDP.growth", 
                                        "closest.9.GDP.growth", "closest.10.GDP.growth")], 
                                 na.rm = TRUE)

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
    df[i, "GDPgrowth.closest.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.average.previous"]))
    df[i, "GDPgrowth.closest.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.average.previous"]))
  }
  else {
    df[i, "GDPgrowth.closest.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.average"]))
    df[i, "GDPgrowth.closest.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.average"]))
  }
}

rm(i)

######################################
# Calculating relative unemployment.fd rate (adjusted)
######################################

# Adding yearly growth rates to closest countries
df.unemployment.fd <- df[,c("iso2c", "year", "unemployment.fd")]

# Merging all 10 growth rates
for (i in 1:10) { 
  names(df.unemployment.fd) <- c(paste("closest.", i, ".name", sep = ""),
                              "year",
                              paste("closest.", i, ".unemployment.fd", sep = ""))
  df <- merge(df, 
              df.unemployment.fd, 
              by = c(paste("closest.", i, ".name", sep = ""), 
                     "year"),
              #all.y = TRUE,
              all.x = TRUE)
}

# Remove intermediary objects
rm(i, df.unemployment.fd)

# Mean unemployment.fd rate
df$unemployment.fd.average <- rowMeans(df[, c("closest.1.unemployment.fd", "closest.2.unemployment.fd", 
                                              "closest.3.unemployment.fd", "closest.4.unemployment.fd", 
                                              "closest.5.unemployment.fd", "closest.6.unemployment.fd", 
                                              "closest.7.unemployment.fd", "closest.8.unemployment.fd", 
                                              "closest.9.unemployment.fd", "closest.10.unemployment.fd")], 
                                       na.rm = TRUE)

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
    df[i, "ue.fd.closest.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.fd.average.previous"]))
    df[i, "ue.fd.closest.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.average.previous"]))
  }
  else {
    df[i, "ue.fd.closest.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.fd.average"]))
    df[i, "ue.fd.closest.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.average"]))
  }
}

rm(i)

######################################
# Getting rid of intermediate variables 
######################################

df <- df[, names(df) != "closest.1.GDP.growth"]
df <- df[, names(df) != "closest.2.GDP.growth"]
df <- df[, names(df) != "closest.3.GDP.growth"]
df <- df[, names(df) != "closest.4.GDP.growth"]
df <- df[, names(df) != "closest.5.GDP.growth"]
df <- df[, names(df) != "closest.6.GDP.growth"]
df <- df[, names(df) != "closest.7.GDP.growth"]
df <- df[, names(df) != "closest.8.GDP.growth"]
df <- df[, names(df) != "closest.9.GDP.growth"]
df <- df[, names(df) != "closest.10.GDP.growth"]

df <- df[, names(df) != "closest.1.unemployment.fd"]
df <- df[, names(df) != "closest.2.unemployment.fd"]
df <- df[, names(df) != "closest.3.unemployment.fd"]
df <- df[, names(df) != "closest.4.unemployment.fd"]
df <- df[, names(df) != "closest.5.unemployment.fd"]
df <- df[, names(df) != "closest.6.unemployment.fd"]
df <- df[, names(df) != "closest.7.unemployment.fd"]
df <- df[, names(df) != "closest.8.unemployment.fd"]
df <- df[, names(df) != "closest.9.unemployment.fd"]
df <- df[, names(df) != "closest.10.unemployment.fd"]

df <- df[, names(df) != "closest.1.name"]
df <- df[, names(df) != "closest.2.name"]
df <- df[, names(df) != "closest.3.name"]
df <- df[, names(df) != "closest.4.name"]
df <- df[, names(df) != "closest.5.name"]
df <- df[, names(df) != "closest.6.name"]
df <- df[, names(df) != "closest.7.name"]
df <- df[, names(df) != "closest.8.name"]
df <- df[, names(df) != "closest.9.name"]
df <- df[, names(df) != "closest.10.name"]

df <- df[, names(df) != "GDPgrowth.average"]
df <- df[, names(df) != "GDPgrowth.average.previous"]
df <- df[, names(df) != "unemployment.fd.average"]
df <- df[, names(df) != "unemployment.fd.average.previous"]
