######################################
# Master thesis
# Lars Mehwald
# Data gathering: WB gross private capital flows
# 4 April 2016
######################################

# Searching for required data
# WDIsearch("private capital") # 2
# Problematic, because restricted to "Africa Development Indicators"
# Other indicator also restricted to Africa

# This paper https://www.imf.org/external/pubs/ft/wp/2014/wp14196.pdf
# directed me to the IMF ISF, there I downloaded bulk data and xls files for 
# the Kayser sample from 1990 on (screenshot)

# PT had additional value for 2015Q3: needed to be removed

######################################
# Loading all data with loop
######################################

IMFcapital.loading <- function(){
  
  v <- c("AU", "AT", "BE", "CA", "DK", "FI", "FR", "DE", "GR", "IS", "IE", "IL", 
         "IT", "JP", "LU", "NL", "NZ", "NO", "PT", "ES", "SE", "GB", "US")
  IMFcapital <- data.frame(NULL)
  
  for(i in 1:length(v)){
    
    link <- "analysis/data/rawdata/InternationalFinancialStatistics/XX.xls"
    link <- gsub("XX", v[i], link)
    
    # Load the data set from the locally saved xls file
    IMFcapital.raw <- read.xlsx(link, 
                                sheetIndex = 1, startRow = 7, 
                                endRow = NULL, header = FALSE)
    
    # Remove additional rows and columns
    IMFcapital.raw <- IMFcapital.raw[-c(14:26),-1]
    
    # Iversion of data frame (year variables as rows)
    IMFcapital.raw <- as.data.frame(t(IMFcapital.raw))
    
    # Renaming of variables
    names(IMFcapital.raw) <- c("year", "external.sector", "current.account",
                              "current.account.goods.balance", 
                              "current.account.goods.services.balance",
                              "financial.account", 
                              "financial.account.direct.investment.assets",
                              "financial.account.direct.investment.liabilities",
                              "financial.account.portfolio.investment.assets",
                              "financial.account.portfolio.investment.liabilities",
                              "IPP", "IPP.assets", "IPP.liabilities")
    
    # Getting rid of first row (before "header")
    IMFcapital.raw <- IMFcapital.raw[-1,]
    
    # Getting rid of row names
    rownames(IMFcapital.raw) <- NULL
    
    # Adding country iso2c ID
    IMFcapital.raw <- cbind(iso2c = v[i], IMFcapital.raw)
    
    # Getting rid of within year observations
    IMFcapital.raw <- IMFcapital.raw[IMFcapital.raw$year != "2014Q1" & 
                                     IMFcapital.raw$year != "2014Q2" &
                                     IMFcapital.raw$year != "2014Q3" &
                                     IMFcapital.raw$year != "2014Q4" &
                                     IMFcapital.raw$year != "2015Q1" &
                                     IMFcapital.raw$year != "2015Q2" &
                                     IMFcapital.raw$year != "2015Q3" &
                                     IMFcapital.raw$year != "2015",]
    
    # Getting rid of category names (variables)
    IMFcapital.raw <- IMFcapital.raw[, names(IMFcapital.raw) != "external.sector" &
                                     names(IMFcapital.raw) != "current.account" & 
                                     names(IMFcapital.raw) != "financial.account" &
                                     names(IMFcapital.raw) != "IPP"]
    
    # Returning to original link
    link <- gsub(v[i], "XX", link)
    
    #Combining it with main IMF data frame 
    IMFcapital <- rbind(IMFcapital, IMFcapital.raw)
  }
  return(IMFcapital)
}

IMFcapital <- IMFcapital.loading()

######################################
# Minor manipulations 
######################################

# Changing the class of the year and IPP variables (before factor)
IMFcapital$year <- as.numeric(as.character(IMFcapital$year))
IMFcapital$IPP.assets <- as.numeric(as.character(IMFcapital$IPP.assets))
IMFcapital$IPP.liabilities <- as.numeric(as.character(IMFcapital$IPP.liabilities))

######################################
# Remove unused variables  
######################################

IMFcapital <- IMFcapital[, names(IMFcapital) != "current.account.goods.balance" & names(IMFcapital) != "current.account.goods.services.balance" & 
                           names(IMFcapital) != "financial.account.direct.investment.assets" & names(IMFcapital) != "financial.account.direct.investment.liabilities" & 
                           names(IMFcapital) != "financial.account.portfolio.investment.assets" & names(IMFcapital) != "financial.account.portfolio.investment.liabilities"]
