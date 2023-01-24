######################################
# Master thesis
# Lars Mehwald
# Creation of citation for R and packages
# 4 April 2016
######################################

# Load required packages
library("repmis")

# Create an object with all packages used
UsedPackages <- c("foreign", "repmis", "WDI", "countrycode", "plyr",
                  "plm", "car", "xlsx", "httr", "digest", "interplot",
                  "tidyr", "stargazer", "XML", "RJSONIO", "geosphere",
                  "lmtest", "zoo", "png", "knitr", "gridExtra",
                  "grid", "sandwich", "rmarkdown")

# add and describe the packages right in the appendix 

# Create a bib file that quotes all packages
# A quote for R is inserted automatically
LoadandCite(UsedPackages, file = "references/RandPackages.bib")

# Load all required packages
lapply(UsedPackages, require, character.only = TRUE)

# Remove object
rm(UsedPackages)
