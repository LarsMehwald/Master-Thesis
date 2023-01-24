######################################
# Master thesis
# Lars Mehwald
# Data gathering: Population size
# 4 April 2016
######################################

# Retrieving the latest version of the Penn World Table via the pwt package
data(pwt7.1)
PopulationSize <- pwt7.1
rm(pwt7.1)

# Saving the raw data
# write.csv(PopulationSize, file = "analysis/data/rawdata/PopulationSize.csv")

# Checking the data, benchmark January 2016
sha1.PopulationSize <- as.data.frame(cbind(VariableName = "PopulationSize", 
                                     sha1.old = "b8e25607d0f1a62d8be0bf846101e150cb0482b5", 
                                     sha1.new = digest(PopulationSize, algo = "sha1")))
sha1.PopulationSize$sha1.old <- as.character(sha1.PopulationSize$sha1.old)
sha1.PopulationSize$sha1.new <- as.character(sha1.PopulationSize$sha1.new)
sha1.PopulationSize <- cbind(sha1.PopulationSize, 
                       IsTheSame = ifelse(sha1.PopulationSize$sha1.old == sha1.PopulationSize$sha1.new, 
                                          TRUE, 
                                          FALSE))
sha1 <- rbind(sha1, sha1.PopulationSize)
rm(sha1.PopulationSize)

# Introducing iso2c country names
PopulationSize$iso2c <- countrycode(PopulationSize$country, 
                                    origin = "country.name",
                                    destination = "iso2c", 
                                    warn = TRUE)

# Getting rid of row names
row.names(PopulationSize) <- NULL

# Keeping the relevant variables
PopulationSize <- PopulationSize[, names(PopulationSize) == "iso2c" | 
                                   names(PopulationSize) == "year" | 
                                   names(PopulationSize) == "pop" ] # population

# Changing the name of relevant variable
names(PopulationSize)[names(PopulationSize) == "pop"] <- "population.thousands"

# Changing class of year variable
PopulationSize$year <- as.numeric(PopulationSize$year)

# Saving the processed data
# write.csv(PopulationSize, file = "analysis/data/PopulationSize.csv")
