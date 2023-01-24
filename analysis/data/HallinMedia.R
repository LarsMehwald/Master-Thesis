######################################
# Master thesis
# Lars Mehwald
# Data gathering: Hallin & Mancini 2004
# 4 April 2016
######################################

# Based on Hallin & Mancini 2004, page 67 the following table is generated 
HallinMedia <- data.frame(country = c("France", "Greece", "Italy", "Portugal", "Spain",
                                      "Austria", "Belgium", "Denmark", "Finland", "Germany", "Netherlands", "Norway", "Sweden", "Switzerland",
                                      "Britain", "United States", "Canada", "Ireland"),
                          media.system = c(rep("Polarized Pluralist Model", 5),
                                           rep("Democratic Corporatist Model", 9),
                                           rep("Liberal Model", 4)))

# Changing media.system into factor
HallinMedia$media.system <- as.factor(HallinMedia$media.system)

# Changing the coding of the country names
HallinMedia$iso2c <- countrycode(HallinMedia$country, 
                                 origin = "country.name",
                                 destination = "iso2c", 
                                 warn = TRUE)

# Removing variables not needed
HallinMedia <- HallinMedia[, names(HallinMedia) != "country"]

# Saving the processed data
# write.csv(KOFindex, file = "analysis/data/KOFindex.csv")
