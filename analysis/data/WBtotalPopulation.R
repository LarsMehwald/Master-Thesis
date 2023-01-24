######################################
# Master thesis
# Lars Mehwald
# Data gathering: Population size
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("Population, total")

# Total population is based on the de facto definition of population, 
# which counts all residents regardless of legal status or citizenship - 
# except for refugees not permanently settled in the country of asylum, 
# who are generally considered part of the population of their country of origin. 
# The values shown are midyear estimates.

# Loading the data
WBtotalPopulation <- WDI(indicator = "SP.POP.TOTL", country = "all", start = 1960, end = 2015)

# Saving the raw data
# write.csv(WBtotalPopulation, file = "analysis/data/rawdata/WBtotalPopulation.csv")

# Loading the saved data
# WBtotalPopulation <- read.csv(file = "analysis/data/rawdata/WBtotalPopulation.csv")
# WBtotalPopulation <- WBtotalPopulation[,-1]

# Getting rid of country name 
WBtotalPopulation <- WBtotalPopulation[, names(WBtotalPopulation) != "country"]

# Changing the name of relevant variable
names(WBtotalPopulation)[names(WBtotalPopulation) == "SP.POP.TOTL"] <- "population"
