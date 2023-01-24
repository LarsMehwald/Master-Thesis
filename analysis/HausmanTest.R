######################################
# Master thesis
# Lars Mehwald
# Hausman Test
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

df.hausman <- data.frame(model = c("without interaction", "with interaction"))

######################################
# Loop to create Hausman test values, without interaction 
######################################

for (j in 1:nrow(v.GDP)) {
  
  if (j<=2){
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), 
                      "election.previous", "GDPperCapita.ln", "parties")]
  }
  if (j>2) {
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), as.character(v.GDP[j, 2]), 
                      "election.previous", "GDPperCapita.ln", "parties")]
  }
  
  # Drop observations where one value is NA  
  df.temp <- na.omit(df.temp)
  
  # Get rid of outliers
  df.temp <- df.temp[which(df.temp$iso2c != "IL" | df.temp$year != 1996),]
  
  # Renaming variables 
  if (j<=2){
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 1])] <- "GrowthNat"
  }
  if (j>2) {
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 1])] <- "GrowthLoc"
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 2])] <- "GrowthInt"
  }
  
  # Estimating panel model
  if (j<=2){
    temp.fe <- plm(election ~ GrowthNat + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "within")
    temp.re <- plm(election ~ GrowthNat + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "random")
  }
  if (j>2) {
    temp.fe <- plm(election ~ GrowthLoc + GrowthInt + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "within")
    temp.re <- plm(election ~ GrowthLoc + GrowthInt + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "random")
  }
  
  # Calculating the Hausman test and saving its value into a data frame 
  df.hausman <- cbind(df.hausman, c(NA, NA))
  names(df.hausman)[j+1] <- as.character(v.GDP[j, 1])
  df.hausman[1,j+1] <- round(as.numeric(phtest(temp.fe, temp.re)[2]), 3)
}

rm(j,temp.fe,temp.re)

######################################
# Loop to create Hausman test values, with interaction 
######################################

for (j in 1:nrow(v.GDP)) {
  
  if (j<=2){
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), 
                     "election.previous", "Trade", "GDPperCapita.ln", "parties")]
  }
  if (j>2) {
    df.temp <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), as.character(v.GDP[j, 2]), 
                     "election.previous", "Trade", "GDPperCapita.ln", "parties")]
  }
  
  # Drop observations where one value is NA  
  df.temp <- na.omit(df.temp)
  
  # Get rid of outliers
  df.temp <- df.temp[which(df.temp$iso2c != "IL" | df.temp$year != 1996),]
  df.temp <- df.temp[df.temp$iso2c != "LU",]
  
  # Renaming variables 
  if (j<=2){
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 1])] <- "GrowthNat"
  }
  if (j>2) {
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 1])] <- "GrowthLoc"
    names(df.temp)[names(df.temp) == as.character(v.GDP[j, 2])] <- "GrowthInt"
  }
  
  # Estimating panel model
  if (j<=2){
    temp.fe <- plm(election ~ GrowthNat*Trade + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "within")
    temp.re <- plm(election ~ GrowthNat*Trade + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "random")
  }
  if (j>2) {
    temp.fe <- plm(election ~ GrowthLoc*Trade + GrowthInt + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "within")
    temp.re <- plm(election ~ GrowthLoc*Trade + GrowthInt + election.previous + GDPperCapita.ln + parties, 
                   data = df.temp, index = c("iso2c", "year"), model = "random")
  }
  
  # Calculating the Hausman test and saving its value into a data frame 
  df.hausman[2,j+1] <- round(as.numeric(phtest(temp.fe, temp.re)[2]), 3)
}

rm(j,temp.fe,temp.re)

######################################
# Presenting output 
######################################

df.hausman <- rename(df.hausman,
                     c("GDPgrowth" = "BEGINNGDPgrowthEND",
                       "GDPgrowth.adj" = "BEGINNGDPgrowth.adjEND",
                       "GDPgrowth.bordering.adj" = "BEGINNGDPgrowth.bordering.adjEND",
                       "GDPgrowth.regional.adj" = "BEGINNGDPgrowth.regional.adjEND",
                       "GDPgrowth.intern.adj" = "BEGINNGDPgrowth.intern.adjEND",
                       "GDPgrowth.trade.adj" = "BEGINNGDPgrowth.trade.adjEND",
                       "GDPgrowth.closest.adj" = "BEGINNGDPgrowth.closest.adjEND",
                       "GDPgrowth.language.adj" = "BEGINNGDPgrowth.language.adjEND"))

temp <- stargazer(df.hausman, 
                  summary = FALSE, 
                  rownames = FALSE,
                  digits = 2,
                  type = "latex",
                  title = "Hausman Tests comparing fixed and random effects for different models",
                  font.size = "small",
                  header = FALSE,
                  no.space = TRUE)

# Latex for variable names
temp <- gsub("BEGINN", "\\\\begin{sideways}", temp)
temp <- gsub("END", "\\\\end{sideways}", temp)

