######################################
# Master thesis
# Lars Mehwald
# Multicollinearity and VIF
# 4 April 2016
######################################

v.GDP <- data.frame(local = c("GDPgrowth", "GDPgrowth.adj", "GDPgrowth.bordering.adj", 
                              "GDPgrowth.regional.adj", "GDPgrowth.intern.adj", 
                              "GDPgrowth.trade.adj", "GDPgrowth.closest.adj", 
                              "GDPgrowth.language.adj"),
                    international = c(NA, NA, "GDPgrowth.bordering.int.adj", 
                                      "GDPgrowth.regional.int.adj", "GDPgrowth.intern.int.adj", 
                                      "GDPgrowth.trade.int.adj", "GDPgrowth.closest.int.adj", 
                                      "GDPgrowth.language.int.adj"))

######################################
# Loop to create multi objects, without interaction 
######################################

for (j in 1:nrow(v.GDP)) {
  
  # Drop observations where one value is NA  
  if (j<=2){
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), 
                      "election.previous", "system", "GDPperCapita.ln", "parties")]
  }
  if (j>2) {
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), as.character(v.GDP[j, 2]), 
                      "election.previous", "system", "GDPperCapita.ln", "parties")]
  }
  
  df.temp <- na.omit(df.temp)
  
  # Get rid of outliers
  df.temp <- df.temp[which(df.temp$iso2c != "IL" | df.temp$year != 1996),]
  
  # Getting rid of year and iso2c
  df.temp <- df.temp[, names(df.temp) != "year" & names(df.temp) != "iso2c"]
  
  # Estimating linear model
  temp <- lm(election ~ ., data = df.temp)
  
  # Calculating VIF and ordering data frame
  temp <- vif(temp)
  temp <- as.data.frame(temp)
  temp$variable <- row.names(temp)
  rownames(temp) <- NULL
  temp <- temp[,c("variable", "GVIF")]
  
  # Correct naming 
  names(temp)[names(temp) == "GVIF"] <- as.character(v.GDP[j, 1])
  if (j<=2){
    temp[temp$variable == as.character(v.GDP[j, 1]), "variable"] <- "GrowthNat"
  }
  if (j>2) {
    temp[temp$variable == as.character(v.GDP[j, 1]), "variable"] <- "GrowthLoc"
    temp[temp$variable == as.character(v.GDP[j, 2]), "variable"] <- "GrowthInt"
  }
  
  # Saving the objects correctly 
  if (j==1){
    df.vif.1 <- temp
  }
  else {
    df.vif.1 <- merge(df.vif.1, temp, by = "variable", all = TRUE)
  }
}
rm(j,temp,df.temp)

######################################
# Loop to create multi objects, with interaction 
######################################

for (j in 1:nrow(v.GDP)) {
  
  # Drop observations where one value is NA  
  if (j<=2){
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), 
                     "election.previous", "Trade", "system", "GDPperCapita.ln", "parties")]
  }
  if (j>2) {
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), as.character(v.GDP[j, 2]), 
                     "election.previous", "Trade", "system", "GDPperCapita.ln", "parties")]
  }
  
  df.temp <- na.omit(df.temp)
  
  # Get rid of outliers
  df.temp <- df.temp[df.temp$iso2c != "LU",]
  df.temp <- df.temp[which(df.temp$iso2c != "IL" | df.temp$year != 1996),]
  
  # Getting rid of year and iso2c
  df.temp <- df.temp[, names(df.temp) != "year" & names(df.temp) != "iso2c"]
  
  # Renaming variables 
  if (j<=2){
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 1])] <- "GrowthNat"
  }
  if (j>2) {
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 1])] <- "GrowthLoc"
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 2])] <- "GrowthInt"
  }

  # Estimating linear model
  if (j<=2){
    temp <- lm(election ~ GrowthNat*Trade + election.previous + system + GDPperCapita.ln + parties, 
               data = df.temp)
  }
  if (j>2) {
    temp <- lm(election ~ GrowthLoc*Trade + GrowthInt + election.previous + system + GDPperCapita.ln + parties, 
               data = df.temp)
  }
  
  # Calculating VIF and ordering data frame
  temp <- vif(temp)
  temp <- as.data.frame(temp)
  temp$variable <- row.names(temp)
  rownames(temp) <- NULL
  temp <- temp[,c("variable", "GVIF")]
  
  # Correct naming 
  names(temp)[names(temp) == "GVIF"] <- as.character(v.GDP[j, 1])

  # Saving the objects correctly 
  if (j==1){
    df.vif.2 <- temp
  }
  else {
    df.vif.2 <- merge(df.vif.2, temp, by = "variable", all = TRUE)
  }
}
rm(j, temp, df.temp)

######################################
# Presenting output 
######################################

df.vif <- rbind(c("LEFT1Regression without interactionLEFT2", rep(NA, length(df.vif.1)-1)),
                df.vif.1,
                c(rep(NA, length(df.vif.1))),
                c("LEFT1Regression with interactionLEFT2", rep(NA, length(df.vif.1)-1)),
                df.vif.2)

for(i in 2:length(df.vif)) {
  df.vif[,i] <- as.numeric(as.character(df.vif[,i]))
  df.vif[,i] <- round(df.vif[,i], digits = 2)
}
rm(i)

df.vif <- rename(df.vif,
                 c("GDPgrowth" = "BEGINNGDPgrowthEND",
                   "GDPgrowth.adj" = "BEGINNGDPgrowth.adjEND",
                   "GDPgrowth.bordering.adj" = "BEGINNGDPgrowth.bordering.adjEND",
                   "GDPgrowth.regional.adj" = "BEGINNGDPgrowth.regional.adjEND",
                   "GDPgrowth.intern.adj" = "BEGINNGDPgrowth.intern.adjEND",
                   "GDPgrowth.trade.adj" = "BEGINNGDPgrowth.trade.adjEND",
                   "GDPgrowth.closest.adj" = "BEGINNGDPgrowth.closest.adjEND",
                   "GDPgrowth.language.adj" = "BEGINNGDPgrowth.language.adjEND"))

# Remove intermediary objects
rm(df.vif.1, df.vif.2, v.GDP)

temp <- stargazer(df.vif, 
                  summary = FALSE, 
                  rownames = FALSE,
                  digits = 2,
                  type = "latex",
                  title = "Variance inflation factors for independent variables",
                  font.size = "small",
                  header = FALSE,
                  no.space = TRUE)

# Latex for variable names
temp <- gsub("BEGINN", "\\\\begin{sideways}", temp)
temp <- gsub("END", "\\\\end{sideways}", temp)

# Latex for intermediary headers 
temp <- gsub("LEFT1", "\\\\multicolumn{1}{l}{", temp) # begin{flushleft}
temp <- gsub("LEFT2", "}", temp)

rm(df.vif)
