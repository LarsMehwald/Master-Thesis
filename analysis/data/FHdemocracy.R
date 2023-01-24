######################################
# Master thesis
# Lars Mehwald
# Data gathering: Freedom House democracy
# 4 April 2016
######################################

# Loading in the data
URL <- "https://freedomhouse.org/sites/default/files/Country%20Ratings%20and%20Status%2C%201973-2016%20%28FINAL%29.xlsx"
FHdemocracy <- source_XlsxData(URL)
rm(URL)

# Saving the raw data
# write.csv(FHdemocracy, file = "analysis/data/rawdata/FHdemocracy.csv")
# Loading the raw data to save time
# FHdemocracy <- read.csv(file = "analysis/data/rawdata/FHdemocracy.csv")
# FHdemocracy <- FHdemocracy[,-1]

# Extracting the right rows
FHdemocracy <- FHdemocracy[7:211,]

# Extracting the right columns
counter <- c(1,2)
for (i in 1:42) {counter <- c(counter, 2+i*3)}
counter <- as.vector(counter)
FHdemocracy <- FHdemocracy[,counter]
rm(counter, i)

# Renaming columns
# Years have been assigned based on in which year the longest period falls  
names(FHdemocracy) <- c("country", 1972:1980, 1981, 1983:2015)

# Solving the issue of 1981 and 1982 
FHdemocracy <- cbind(FHdemocracy, FHdemocracy$`1981`)
names(FHdemocracy)[names(FHdemocracy) == "FHdemocracy$`1981`"] <- 1982

# Coding of NAs
for (i in 2:length(FHdemocracy)) {
  FHdemocracy[,i] <- as.character(FHdemocracy[,i])
  FHdemocracy[,i][FHdemocracy[,i] == ".."] <- NA
  FHdemocracy[,i] <- as.numeric(FHdemocracy[,i])
}
rm(i)

# Deleting countries that cause problems 
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Kosovo",]

# Duplicates 
# FHdemocracy$iso2c[duplicated(FHdemocracy$iso2c)]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Germany, E. ",]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Germany, W. ",]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "USSR",]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Vietnam, N.",]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Vietnam, S.",]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Yemen, N.",]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Yemen, S.",]
FHdemocracy <- FHdemocracy[FHdemocracy$country != "Yugoslavia (Serbia & Montenegro)",]

# Changing country names to iso2c code 
FHdemocracy$iso2c <- countrycode(FHdemocracy$country, 
                                 origin = "country.name",
                                 destination = "iso2c", 
                                 warn = TRUE)

# Getting rid of former country variable 
FHdemocracy <- FHdemocracy[,names(FHdemocracy) != "country"]
FHdemocracy <- FHdemocracy[,c(45,1:44)]

# Reshaping into long format
FHdemocracy <- reshape(FHdemocracy,
                       varying = c(2:length(FHdemocracy)),
                       v.names = "FHdemocracy",
                       timevar = "year",
                       times = names(FHdemocracy)[2:length(FHdemocracy)],
                       new.row.names = 1:10000,
                       direction = "long") 

# Delete additional variable from reshape process
FHdemocracy <- FHdemocracy[,1:3]

# Changing the class of year
FHdemocracy$year <- as.numeric(as.character(FHdemocracy$year))
