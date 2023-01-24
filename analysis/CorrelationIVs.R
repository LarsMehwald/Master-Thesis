######################################
# Master thesis
# Lars Mehwald
# Correlation IVs
# 4 April 2016
######################################

# Data frame containing names of local and international component
econ.vars <- data.frame(international = c("GDPgrowth.bordering.int.adj", "GDPgrowth.regional.int.adj",
                                          "GDPgrowth.intern.int.adj", "GDPgrowth.trade.int.adj",
                                          "GDPgrowth.closest.int.adj", "GDPgrowth.language.int.adj"),
                        local = c("GDPgrowth.bordering.adj", "GDPgrowth.regional.adj",
                                  "GDPgrowth.intern.adj", "GDPgrowth.trade.adj",
                                  "GDPgrowth.closest.adj", "GDPgrowth.language.adj"))

# Target data frame to save country and overall correlation numbers 
df.corr.iv <- data.frame(country = rep(NA, length(unique(df$iso2c)) + 1),
                         Border = rep(NA, length(unique(df$iso2c)) + 1),
                         Region = rep(NA, length(unique(df$iso2c)) + 1),
                         Intern = rep(NA, length(unique(df$iso2c)) + 1),
                         Trade = rep(NA, length(unique(df$iso2c)) + 1),
                         Close = rep(NA, length(unique(df$iso2c)) + 1),
                         Language = rep(NA, length(unique(df$iso2c)) + 1))

# Loop (vertical)
for (i in 1:length(unique(df$iso2c))) {
  
  # Country subset 
  temp <- df[df$iso2c == unique(df$iso2c)[i],]
  
  # Subset election years
  temp <- temp[is.na(temp$leg.incumbent.vote) == FALSE, ]
  
  # Save country name 
  df.corr.iv[i, "country"] <- countrycode(unique(df$iso2c)[i],
                                          origin = "iso2c",
                                          destination = "country.name",
                                          warn = TRUE)
  
  # Additional loop for each relative economic performance (filling in the rows)
  for (j in 1:nrow(econ.vars)) {
    temp2 <- temp[ , c(as.character(econ.vars[j,1]), 
                       "GDPgrowth.adj")] # as.character(econ.vars[j,2]) # Change to to the fact that international is compared to national performance 
    temp2 <- na.omit(temp2)
    df.corr.iv[i, j+1] <- cor(temp2[,1], temp2[,2], method = "pearson")
  }
  
  # Adding another line for overall correlation of each relative measurement 
  if (i == length(unique(df$iso2c))) {
    
    # name "overall"
    df.corr.iv[i+1, "country"] <- "overall"
    
    for (k in 1:nrow(econ.vars)) {
      
      # Subset for election years and two variables 
      temp2 <- df[is.na(df$leg.incumbent.vote) == FALSE, 
                  c(as.character(econ.vars[k,1]), 
                    "GDPgrowth.adj")] # as.character(econ.vars[k,2])
      temp2 <- na.omit(temp2)
      df.corr.iv[i+1, k+1] <- cor(temp2[,1], temp2[,2], method = "pearson")
    }
  }
}

# Remove intermediary objects 
rm(i,j,k,temp,temp2)

# Rename variable names with latex code
df.corr.iv <- rename(df.corr.iv,
                     c("Border" = "BEGINNGDPgrowth.bordering.adjEND",
                       "Region" = "BEGINNGDPgrowth.regional.adjEND",
                       "Intern" = "BEGINNGDPgrowth.intern.adjEND",
                       "Trade" = "BEGINNGDPgrowth.trade.adjEND",
                       "Close" = "BEGINNGDPgrowth.closest.adjEND",
                       "Language" = "BEGINNGDPgrowth.language.adjEND"))

# Create stargazer output 
temp <- stargazer(df.corr.iv, 
                  summary = FALSE, 
                  rownames = FALSE,
                  digits = 2,
                  type = "latex",
                  title = "Pearson correlation of international component (x) and national economic performance (y)",
                  font.size = "small",
                  header = FALSE,
                  no.space = TRUE)

temp <- gsub("BEGINN", "\\\\begin{sideways}", temp)
temp <- gsub("END", "\\\\end{sideways}", temp)

# cat() has been moved into other code chunk in order to avoid printing of stargazer output (when assigned)
