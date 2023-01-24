######################################
# Master thesis
# Lars Mehwald
# Bordering Countries 
# 4 April 2016
######################################

# Steps ahead: 
# Need to check object, whether download was successful
# Create a dummy variable that indicates successful neighbor,
# thereby I can differentiate the cases when there is no neighbor for cases,
# where I just have been unable to retrieve the information.
# This can be included into the 0 coding (like Japan and Australia)

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

URL <- "http://api.geonames.org/neighboursJSON?country=XX&username=larsunterwegs"

# Create function

border <- function(zz) {

# Create link
URL <- sub("XX", iso2c[zz,1], URL)

# Retrive content 
bor <- GET(URL)
bor <- content(bor, as = "parsed")

# Problem with addtional vector in list (realised it with DK, zz=8)
# bor <- bor[names(bor) !="totalResultsCount"]

# How many vectors are in the list? Russia has 14 bordering countries 
# length.list <- bor$totalResultsCount
bor.1.name <- ifelse(bor$totalResultsCount >= 1, bor$geonames[[1]]$countryCode, NA) 
bor.2.name <- ifelse(bor$totalResultsCount >= 2, bor$geonames[[2]]$countryCode, NA)
bor.3.name <- ifelse(bor$totalResultsCount >= 3, bor$geonames[[3]]$countryCode, NA)
bor.4.name <- ifelse(bor$totalResultsCount >= 4, bor$geonames[[4]]$countryCode, NA)
bor.5.name <- ifelse(bor$totalResultsCount >= 5, bor$geonames[[5]]$countryCode, NA)
bor.6.name <- ifelse(bor$totalResultsCount >= 6, bor$geonames[[6]]$countryCode, NA)
bor.7.name <- ifelse(bor$totalResultsCount >= 7, bor$geonames[[7]]$countryCode, NA)
bor.8.name <- ifelse(bor$totalResultsCount >= 8, bor$geonames[[8]]$countryCode, NA)
bor.9.name <- ifelse(bor$totalResultsCount >= 9, bor$geonames[[9]]$countryCode, NA)
bor.10.name <- ifelse(bor$totalResultsCount >= 10, bor$geonames[[10]]$countryCode, NA)
bor.11.name <- ifelse(bor$totalResultsCount >= 11, bor$geonames[[11]]$countryCode, NA)
bor.12.name <- ifelse(bor$totalResultsCount >= 12, bor$geonames[[12]]$countryCode, NA)
bor.13.name <- ifelse(bor$totalResultsCount >= 13, bor$geonames[[13]]$countryCode, NA)
bor.14.name <- ifelse(bor$totalResultsCount >= 14, bor$geonames[[14]]$countryCode, NA)

# Combine values into new row
bor.zz <- cbind(iso2c = as.character(iso2c[zz,1]),
                ifelse(bor$totalResultsCount >= 1, bor.1.name, NA), 
                ifelse(bor$totalResultsCount >= 2, bor.2.name, NA),
                ifelse(bor$totalResultsCount >= 3, bor.3.name, NA),
                ifelse(bor$totalResultsCount >= 4, bor.4.name, NA),
                ifelse(bor$totalResultsCount >= 5, bor.5.name, NA),
                ifelse(bor$totalResultsCount >= 6, bor.6.name, NA),
                ifelse(bor$totalResultsCount >= 7, bor.7.name, NA),
                ifelse(bor$totalResultsCount >= 8, bor.8.name, NA),
                ifelse(bor$totalResultsCount >= 9, bor.9.name, NA),
                ifelse(bor$totalResultsCount >= 10, bor.10.name, NA),
                ifelse(bor$totalResultsCount >= 11, bor.11.name, NA),
                ifelse(bor$totalResultsCount >= 12, bor.12.name, NA),
                ifelse(bor$totalResultsCount >= 13, bor.13.name, NA),
                ifelse(bor$totalResultsCount >= 14, bor.14.name, NA)
                )

# Retrun to original link 
URL <- sub(as.character(iso2c[zz,1]), "XX", URL)

return(bor.zz)

}

# Perform function with apply
BorderingCountries <- lapply(1:length(iso2c$iso2c), border)

# Converting list into data frame (one more column is ok due to iso2c)
BorderingCountries <- data.frame(matrix(unlist(BorderingCountries), 
                                        nrow=length(iso2c$iso2c), 
                                        byrow=T))

# Not a good idea, because Namibia gets problematic 
# Save the data
# write.csv(BorderingCountries, file = "analysis/data/rawdata/BorderingCountries.csv")
# Read the data 
# BorderingCountries <- read.csv(file = "analysis/data/rawdata/BorderingCountries.csv")
# Remove first variable that was added in the reading process
# BorderingCountries <- BorderingCountries[,-1]

######################################
# Data manipulation
######################################

# Renaming country ID 
names(BorderingCountries)[names(BorderingCountries) == "X1"] <- "iso2c"

# Merge with original iso2c data frame
iso2c <- merge(iso2c, BorderingCountries, by="iso2c", all = TRUE)

# Rename the variables 
names(iso2c) <- c("iso2c", "Neighbor1", "Neighbor2", "Neighbor3", "Neighbor4",
                  "Neighbor5", "Neighbor6", "Neighbor7", "Neighbor8", 
                  "Neighbor9", "Neighbor10", "Neighbor11", "Neighbor12",
                  "Neighbor13", "Neighbor14")

# Merge with original df 
df <- merge(df, iso2c, by = "iso2c", all = TRUE)

######################################
# Create relative economic measurements: growth
######################################

# Merging

df.GDPgrowth <- df[, c("iso2c", "year", "GDPgrowth")]

borderGDP <- function(yy){

  Neighbor <- paste("Neighbor", yy, sep = "")
  
  GDPgrowth.Neighbor <- paste("GDPgrowth.Neighbor", yy, sep = "")

  names(df.GDPgrowth) <- c(Neighbor, "year", GDPgrowth.Neighbor)
  
  df <- merge(df, 
              df.GDPgrowth, 
              by = c(Neighbor, "year"),
#              all.y = TRUE,
              all.x = TRUE)
  return(df)
}

# somehow the number of observations increased from 8776 to 8939

df <- borderGDP(1) # Works by manually typing in numbers 
df <- borderGDP(2)
df <- borderGDP(3)
df <- borderGDP(4)
df <- borderGDP(5)
df <- borderGDP(6)
df <- borderGDP(7)
df <- borderGDP(8)
df <- borderGDP(9)
df <- borderGDP(10)
df <- borderGDP(11)
df <- borderGDP(12)
df <- borderGDP(13)
df <- borderGDP(14)

# Average bordering growth rate 
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
df$GDPgrowth.average[df$iso2c == "AU"| df$iso2c == "NZ" | df$iso2c == "JP"] <- 0

# Deviating growth rate 
df$GDPgrowth.deviation.bordering <- df$GDPgrowth - df$GDPgrowth.average

######################################
# Removing additional variables
######################################

rm(df.GDPgrowth, BorderingCountries, URL, iso2c)

df <- df[, names(df) != "GDPgrowth.average"]

df <- df[, names(df) != "GDPgrowth.Neighbor1"]
df <- df[, names(df) != "Neighbor1"]
df <- df[, names(df) != "GDPgrowth.Neighbor2"]
df <- df[, names(df) != "Neighbor2"]
df <- df[, names(df) != "GDPgrowth.Neighbor3"]
df <- df[, names(df) != "Neighbor3"]
df <- df[, names(df) != "GDPgrowth.Neighbor4"]
df <- df[, names(df) != "Neighbor4"]
df <- df[, names(df) != "GDPgrowth.Neighbor5"]
df <- df[, names(df) != "Neighbor5"]
df <- df[, names(df) != "GDPgrowth.Neighbor6"]
df <- df[, names(df) != "Neighbor6"]
df <- df[, names(df) != "GDPgrowth.Neighbor7"]
df <- df[, names(df) != "Neighbor7"]
df <- df[, names(df) != "GDPgrowth.Neighbor8"]
df <- df[, names(df) != "Neighbor8"]
df <- df[, names(df) != "GDPgrowth.Neighbor9"]
df <- df[, names(df) != "Neighbor9"]
df <- df[, names(df) != "GDPgrowth.Neighbor10"]
df <- df[, names(df) != "Neighbor10"]
df <- df[, names(df) != "GDPgrowth.Neighbor11"]
df <- df[, names(df) != "Neighbor11"]
df <- df[, names(df) != "GDPgrowth.Neighbor12"]
df <- df[, names(df) != "Neighbor12"]
df <- df[, names(df) != "GDPgrowth.Neighbor13"]
df <- df[, names(df) != "Neighbor13"]
df <- df[, names(df) != "GDPgrowth.Neighbor14"]
df <- df[, names(df) != "Neighbor14"]
