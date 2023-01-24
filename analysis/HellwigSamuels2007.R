######################################
# Master thesis
# Lars Mehwald
# Data gathering: Hellwig & Samuels 2007
# 4 April 2016
######################################

######################################
# Prepare reference data
######################################

# Loading in the data 
URL <- "http://mypage.iu.edu/~thellwig/research/HellwigSamuelsCPS2007.dta"
df.raw.hellwig <- read.dta(URL)
rm(URL)

# Saving the raw data
write.csv(df.raw.hellwig, file = "analysis/data/rawdata/HellwigSamuels2007.csv")

# Loading the data
# df.raw.hellwig <- read.csv("analysis/data/rawdata/HellwigSamuels2007.csv")
# df.raw.hellwig <- df.raw.hellwig[,-1]

# Some observations have no names and rest NAs
df.raw.hellwig <- df.raw.hellwig[is.na(df.raw.hellwig$case) == FALSE,]

# Rename country name
df.raw.hellwig$iso2c <- countrycode(df.raw.hellwig$country,
                                    origin = "country.name",
                                    destination = "iso2c",
                                    warn = TRUE)

# Rename variables
names(df.raw.hellwig)[names(df.raw.hellwig) == "yrelec"] <- "year"
names(df.raw.hellwig)[names(df.raw.hellwig) == "dgdp"] <- "GDPgrowth.hellwig"

# Keep relevant variables 
df.raw.hellwig <- df.raw.hellwig[,c("iso2c", "year", "electype", "GDPgrowth.hellwig")]

######################################
# GDP measures
######################################

# Searching for required data
# WDIsearch("gdp")

# Object containing code of World Bank indicators and description, later correlations are added
df.WB.gdp.names <- data.frame(code = c("NY.GDP.MKTP.CD",
                                       "NY.GDP.MKTP.CN",
                                       "NY.GDP.MKTP.KD",
                                       "NY.GDP.MKTP.KN",
                                       "NY.GDP.MKTP.PP.CD",
                                       "NY.GDP.MKTP.PP.KD"),
                              description = c("GDP (current US$)", # 82
                                              "GDP (current LCU)", # 84 
                                              "GDP (constant 2005 US$)", # 86
                                              "GDP (constant LCU)", # 88
                                              "GDP, PPP (current international $)", # 89
                                              "GDP, PPP (constant 2005 international $)")) # 90
# No cases used that contain "GDP deflator" in the name, because it is actually the deflator

# Download all indicators and convert them into growth rates, also for previous year 
for (i in 1:nrow(df.WB.gdp.names)) {
  # Loading the data
  temp <- WDI(indicator = as.character(df.WB.gdp.names[i,1]),
              country = "all", start = 1970, end = 2010)

  # Removing country name (not iso2c),
  temp <- temp[,-2]

  # Transforming into growth rates
  temp[,3] <- as.numeric(as.character(temp[,3]))
  temp2 <- temp
  temp2$year <- temp2$year + 1
  temp <- merge(temp, temp2, by = c("iso2c", "year"), all.x = TRUE, all.y = FALSE)
  rm(temp2)
  temp$growth <- (temp[,3] - temp[,4]) / temp[,4]

  # Removing intermediary variables
  temp <- temp[, c("iso2c", "year", "growth")]

  # Adding previous growth rate and renaming in the meantime
  temp3 <- temp
  temp3$year <- temp3$year + 1
  names(temp)[names(temp) == "growth"] <- as.character(df.WB.gdp.names[i,1])
  names(temp3)[names(temp3) == "growth"] <- as.character(paste(df.WB.gdp.names[i,1], "previous", sep = "."))
  temp <- merge(temp, temp3, by = c("iso2c", "year"), all.x = TRUE, all.y = FALSE)
  rm(temp3)

  # Saving the data correctly
  if (i == 1) {
    df.WB.gdp <- temp
  }
  else{
    df.WB.gdp <- merge(df.WB.gdp, temp,
                       by = c("iso2c", "year"),
                       all = TRUE)
  }
}
rm(i,temp)

# Change coding of percentage 
for (i in 3:length(df.WB.gdp)){
  df.WB.gdp[,i] <- as.numeric(as.character(df.WB.gdp[,i]))
  df.WB.gdp[,i] <- df.WB.gdp[,i]*100
}
rm(i)

# Save intermediary 
write.csv(df.WB.gdp, "MasterFileGrowth.csv")

# Load intermediary object 
# df.WB.gdp <- read.csv(file = "MasterFileGrowth.csv")
# df.WB.gdp <- df.WB.gdp[,-1]
# # Problem Namibia
# df.WB.gdp$iso2c <- as.character(df.WB.gdp$iso2c) 
# df.WB.gdp$iso2c[is.na(df.WB.gdp$iso2c)] <- "NA"

# Merge with Hellwig Samuels 2007
df.WB.gdp <- merge(df.raw.hellwig, df.WB.gdp,
                   by = c("iso2c", "year"),
                   all.x = TRUE,
                   all.y = FALSE)

# Get info from WBI for election timing 
temp <- df.full[,c("iso2c", "year", "exelec", "dateexec", "legelec", "dateleg")]
df.WB.gdp <- merge(temp, df.WB.gdp,
                   by = c("iso2c", "year"),
                   all.x = FALSE,
                   all.y = TRUE)
rm(temp)

# Adjusting the year
# Problem: not easy to discern the cases where an election took place in the first or second half
# But if there are two elections a year, they are usually highly correlated with regard the month 
# in wich they take place.
# Hence if there was an election in the first half of the year, then it is time adjusted

# Create a measure whether there was an election in the first half of the year
df.WB.gdp$date.elec <- 0
for (j in 1:nrow(df.WB.gdp)) {
  if(df.WB.gdp[j, "exelec"] == 1 & is.na(df.WB.gdp[j, "exelec"]) == FALSE & 
     df.WB.gdp[j, "dateexec"] <= 6 & is.na(df.WB.gdp[j, "dateexec"]) == FALSE){
    df.WB.gdp[j, "date.elec"] <- 1
  }
  if(df.WB.gdp[j, "legelec"] == 1 & is.na(df.WB.gdp[j, "legelec"]) == FALSE & 
     df.WB.gdp[j, "dateleg"] <= 6 & is.na(df.WB.gdp[j, "dateleg"]) == FALSE){
    df.WB.gdp[j, "date.elec"] <- 1
  }
}
rm(j)

# Inserting the adjusted GDP values 
for (j in 1:nrow(df.WB.gdp.names)) {
  for (i in 1:nrow(df.WB.gdp)) {
    if (df.WB.gdp[i, "date.elec"]  == 1 & is.na(df.WB.gdp[i, "date.elec"]) == FALSE) {
      df.WB.gdp[i, paste(df.WB.gdp.names[j,1], "previous", sep = ".")] <- as.numeric(as.character(df.WB.gdp[i, paste(df.WB.gdp.names[j,1], "previous", sep = ".")])) 
    }
    else {
      df.WB.gdp[i, paste(df.WB.gdp.names[j,1], "previous", sep = ".")] <- as.numeric(as.character(df.WB.gdp[i, as.character(df.WB.gdp.names[j,1])]))
    }
  }
}
rm(j,i)

######################################
# Correlations with GDP measures
######################################

df.WB.gdp.names$correlation.unadjusted <- NA
df.WB.gdp.names$correlation.adjusted <- NA

# Save correlation values (adjusted and unadjusted) in the data frame that contained the names 
for (i in 1:nrow(df.WB.gdp.names)) {
  
  # unadjusted
  temp <- df.WB.gdp[, c("GDPgrowth.hellwig", as.character(df.WB.gdp.names[i,1]))]
  temp <- na.omit(temp)
  df.WB.gdp.names[i, "correlation.unadjusted"] <- cor(temp[,1], temp[,2])
  
  # adjusted
  temp <- df.WB.gdp[, c("GDPgrowth.hellwig", paste(df.WB.gdp.names[i,1], "previous", sep = "."))]
  temp <- na.omit(temp)
  df.WB.gdp.names[i, "correlation.adjusted"] <- cor(temp[,1], temp[,2])
}
rm(i,temp)

# Rename variable names with latex code
df.WB.gdp.names <- rename(df.WB.gdp.names,
                          c("code" = "BEGINNcodeEND",
                            "description" = "BEGINNdescriptionEND",
                            "correlation.unadjusted" = "BEGINNcorrelation unadjustedEND",
                            "correlation.adjusted" = "BEGINNcorrelation adjustedEND"))

# Stargazer output, in order to manipulate latex with latex commands 
temp <- stargazer(df.WB.gdp.names, 
                  summary = FALSE, 
                  rownames = FALSE,
                  digits = 2,
                  type = "latex",
                  title = "World Bank Development Indicators and correlations with growth rates used by Hellwig and Samuels (2007)",
                  font.size = "small",
                  header = FALSE,
                  no.space = TRUE)

temp <- gsub("BEGINN", "\\\\begin{sideways}", temp)
temp <- gsub("END", "\\\\end{sideways}", temp)

######################################
# Create separate object with both growth rates, with small sample 
######################################

# Split the own data set
df.test.growth <- df[, c("iso2c", "year", "GDPgrowth", "GDPgrowth.adj")]

# Class
df.test.growth$year <- as.numeric(as.character(df.test.growth$year))
df.raw.hellwig$year <- as.numeric(as.character(df.raw.hellwig$year))

# Merge it with the reference data set
df.test.growth <- merge(df.test.growth, df.raw.hellwig, 
                        by = c("iso2c", "year"), 
                        all.x = TRUE, all.y = FALSE)

# Shrink the data set
df.test.growth <- na.omit(df.test.growth)

######################################
# Analysis
######################################

# Correlation
# cor(df.test.growth$GDPgrowth, df.test.growth$GDPgrowth.hellwig)
# cor(df.test.growth$GDPgrowth.adj, df.test.growth$GDPgrowth.hellwig)

# Difference between measures 
# df.test.growth$GDPgrowth.adj.diff <- df.test.growth$GDPgrowth.adj - df.test.growth$GDPgrowth.hellwig
# summary(df.test.growth$GDPgrowth.adj.diff)

# df.test.growth$GDPgrowth.diff <- df.test.growth$GDPgrowth - df.test.growth$GDPgrowth.hellwig
# summary(df.test.growth$GDPgrowth.diff)

# My measure is mostly above the other measure

# Is variance the same for each country?
# df.agg.var <- aggregate(df.test.growth$GDPgrowth.diff ~ df.test.growth$iso2c, data = df.test.growth, var)
# df.agg.length <- aggregate(df.test.growth$GDPgrowth.diff ~ df.test.growth$iso2c, data = df.test.growth, length)
# df.agg <- merge(df.agg.var, df.agg.length, by = names(df.agg.length)[1])
# summary(df.agg.var[,2])
# No structure 

######################################
# Graphical representation
######################################

# unadjusted
# ggplot(df.test.growth, aes(GDPgrowth, GDPgrowth.hellwig)) + 
#   geom_jitter(size = 0.2) +
#   geom_abline(slope = 1, intercept = 0) + 
#   ggtitle("Figure XX: Scatterplot between measurements of GDP growth") +
#   theme(plot.title = element_text(size=12, face="plain"))

# adjusted
ggplot.scatter.hellwig <- ggplot(df.test.growth, aes(GDPgrowth.adj, GDPgrowth.hellwig)) +
  geom_jitter(size = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  ylab("GDPgrowth.hellwig.samuels.2007") + 
  ggtitle("Figure 1: Scatterplot with GDP growth")+
  theme(plot.title = element_text(size=12, face="plain"))

# Correlated with time? 
# ggplot.time.hellwig <- ggplot(df.test.growth, aes(year, GDPgrowth.adj - GDPgrowth.hellwig)) +
#   geom_smooth(method = "loess", color = "red") +
#   geom_jitter(size = 0.2) +
#   ylab("Difference in GDP growth") + 
#   scale_y_continuous(breaks=c(seq(-2,5,1))) + 
#   ggtitle("Figure XX: ")+
#   theme(plot.title = element_text(size=12, face="plain")) + 
#   coord_cartesian(ylim = c(-2, 5)) 

# rm(df.test.growth)

######################################
# What if I replace my model with the old data? 
######################################

# Solving the issue of duplicates in the old data frame by taking its mean
# df.raw.hellwig.dupl <- df.raw.hellwig[duplicated(df.raw.hellwig[, c("iso2c", "year")]), ]
# df.raw.hellwig <- df.raw.hellwig[!duplicated(df.raw.hellwig[, c("iso2c", "year")]), ]
# row.names(df.raw.hellwig.dupl) <- NULL
# df.raw.hellwig.dupl <- df.raw.hellwig.dupl[,c("iso2c", "year", "GDPgrowth.hellwig")]
# names(df.raw.hellwig.dupl)[names(df.raw.hellwig.dupl) == "GDPgrowth.hellwig"] <- "GDPgrowth.hellwig.dupl"
# df.raw.hellwig <- merge(df.raw.hellwig, df.raw.hellwig.dupl,
#                         by = c("iso2c", "year"),
#                         all.x = TRUE,
#                         all.y = FALSE)
# rm(df.raw.hellwig.dupl)
# df.raw.hellwig$GDPgrowth.hellwig <- rowMeans(df.raw.hellwig[, c("GDPgrowth.hellwig", "GDPgrowth.hellwig.dupl")],
#                                              na.rm = TRUE)
# df.raw.hellwig <- df.raw.hellwig[, names(df.raw.hellwig) != "GDPgrowth.hellwig.dupl"]
# df.raw.hellwig <- df.raw.hellwig[, names(df.raw.hellwig) != "electype"]
# 
# Creating intermediary object to be used in regression analysis 
# df.intermediary.hellwig <- merge(df.full, df.raw.hellwig,
#                                  by = c("iso2c", "year"),
#                                  all.x = TRUE,
#                                  all.y = FALSE)
# 
# ols.hellwig <- lm(election ~ GDPgrowth.hellwig*Trade + election.previous + system + GDPperCapita.ln + parties, 
#                   data = df.intermediary.hellwig, subset = subset.1)
# interplot(m = ols.hellwig, var1 = "GDPgrowth.hellwig", var2 = "Trade")
# 
