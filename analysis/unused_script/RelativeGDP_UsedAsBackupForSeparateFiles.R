######################################
# Master thesis
# Lars Mehwald
# Data manipulations: Relative GDP (esp. benchmarking)
# 4 April 2016
######################################

######################################
# First differences of unemployment 
######################################

# To be done after adjusting? Or adjusting after DF? 
# What about GDP?

# Hellwig 2007 pp. 8-9

# Create new object for "previous" unemployment 
df.unemployment <- df[,c("iso2c", "year", "unemployment")]
df.unemployment$year <- df.unemployment$year + 1
names(df.unemployment)[names(df.unemployment) == "unemployment"] <- "unemployment.previous"

# Merge previous and current unemployment rates 
df <- merge(df, df.unemployment, by = c("iso2c", "year"), all.x = TRUE, all.y = FALSE)

# Create first-difference for unemployment
df$unemployment.fd <- df$unemployment - df$unemployment.previous

# Delete intermediary objects/ variables
rm(df.unemployment)
df <- df[, names(df) != "unemployment.previous"]

#####################################
# Economic variable close to election (adjusted)
####### ##############################

# For the first half of the year, the economic data from the previous year is used
# For the second half, the economic data from the year of the election is used 

# Did the election occur in the first half of the year? 1=first half, 2=second half
df$dateleg.half <- ifelse(df$dateleg <= 6, 1, 2)

# Creating values for economic data of the last year 
df.econ.previous <- as.data.frame(cbind(iso2c = df$iso2c, 
                                        year = df$year + 1, 
                                        GDPgrowth.previous = df$GDPgrowth, 
                                        unemployment.fd.previous = df$unemployment.fd))

# Merging the data frames 
df.econ.previous$iso2c <- as.character(df.econ.previous$iso2c)
df.econ.previous$year <- as.numeric(as.character(df.econ.previous$year))
df <- merge(df, df.econ.previous, by=c("iso2c","year"), all = TRUE)
rm(df.econ.previous)

# Creating adjusted economic variables
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) 
  }
  else {
    df[i, "GDPgrowth.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"]))
  }
}

for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "unemployment.fd.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) 
  }
  
  else {
    df[i, "unemployment.fd.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"]))
  }
  
}

# Removing intermediate variables - it is better to keep them till the end for other calculations 
# df <- df[, names(df) !="GDPgrowth.previous"]
# df <- df[, names(df) !="unemployment.fd.previous"]
rm(i)

######################################
# Trade weighted relative growth rate (adjusted)
######################################

# Creating a new growth data frame (based on df)
trade.partner.growth <- df[, names(df) == "iso2c" | names(df) == "GDPgrowth" | names(df) == "year"]

# Adding the growth rate to the 1 - 10 trading partner
names(trade.partner.growth) <- c("trade.1.name", "trade.1.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.1.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.2.name", "trade.2.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.2.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.3.name", "trade.3.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.3.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.4.name", "trade.4.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.4.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.5.name", "trade.5.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.5.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.6.name", "trade.6.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.6.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.7.name", "trade.7.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.7.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.8.name", "trade.8.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.8.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.9.name", "trade.9.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.9.name", "year"), all.x = TRUE)
names(trade.partner.growth) <- c("trade.10.name", "trade.10.growth", "year")
df <- merge(df, trade.partner.growth, by = c("trade.10.name", "year"), all.x = TRUE)

# Problem: if one NA in the trade.x.growth, then the computation does not work (shows NAs)
# This decision is problematic, because in these cases I will underestimate 
# the regional growth rate, because I would assume that the average growth rate is over 0
df$trade.1.growth[is.na(df$trade.1.growth)] <- 0
df$trade.2.growth[is.na(df$trade.2.growth)] <- 0
df$trade.3.growth[is.na(df$trade.3.growth)] <- 0
df$trade.4.growth[is.na(df$trade.4.growth)] <- 0
df$trade.5.growth[is.na(df$trade.5.growth)] <- 0
df$trade.6.growth[is.na(df$trade.6.growth)] <- 0
df$trade.7.growth[is.na(df$trade.7.growth)] <- 0
df$trade.8.growth[is.na(df$trade.8.growth)] <- 0
df$trade.9.growth[is.na(df$trade.9.growth)] <- 0
df$trade.10.growth[is.na(df$trade.10.growth)] <- 0

# Creating a regional trade weighted growth rate 
df$GDPgrowth.regional.trade <- df$trade.1.share*df$trade.1.growth + df$trade.2.share*df$trade.2.growth +
  df$trade.3.share*df$trade.3.growth + df$trade.4.share*df$trade.4.growth + 
  df$trade.5.share*df$trade.5.growth + df$trade.6.share*df$trade.6.growth + 
  df$trade.7.share*df$trade.7.growth + df$trade.8.share*df$trade.8.growth +
  df$trade.9.share*df$trade.9.growth + df$trade.10.share*df$trade.10.growth

# Creating values for economic data of the last year 
df.trade.previous <- as.data.frame(cbind(iso2c = as.character(df$iso2c), 
                                         year = as.numeric(as.character(df$year)) + 1, 
                                         GDPgrowth.regional.trade.previous = df$GDPgrowth.regional.trade))

# Merging the data frames 
df.trade.previous$iso2c <- as.character(df.trade.previous$iso2c)
df.trade.previous$year <- as.numeric(as.character(df.trade.previous$year))
df <- merge(df, df.trade.previous, by=c("iso2c","year"), all.x = TRUE, all.y = FALSE)

# Creating adjusted economic variables
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.deviation.trade.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.regional.trade.previous"]))
  }
  else {
    df[i, "GDPgrowth.deviation.trade.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.regional.trade"]))
  }
}

# Remove intermediary objects (most variables are kept until unemployment has been adjusted)
df <- df[, names(df) != "trade.1.growth" & names(df) != "trade.2.growth" & names(df) != "trade.3.growth" & 
           names(df) != "trade.4.growth" & names(df) != "trade.5.growth" & names(df) != "trade.6.growth" & 
           names(df) != "trade.7.growth" & names(df) != "trade.8.growth" & names(df) != "trade.9.growth" & 
           names(df) != "trade.10.growth"]
rm(trade.partner.growth, df.trade.previous, i)

######################################
# Trade weighted relative unemployment (adjusted)
######################################

# Creating a new unemployment data frame
trade.partner.unemployment <- df[, names(df) == "iso2c" | names(df) == "unemployment.fd" | names(df) == "year"]

# Adding the unemployment rate to the 1 - 10 trading partner
names(trade.partner.unemployment) <- c("trade.1.name", "trade.1.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.1.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.2.name", "trade.2.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.2.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.3.name", "trade.3.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.3.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.4.name", "trade.4.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.4.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.5.name", "trade.5.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.5.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.6.name", "trade.6.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.6.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.7.name", "trade.7.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.7.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.8.name", "trade.8.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.8.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.9.name", "trade.9.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.9.name", "year"), all.x = TRUE)
names(trade.partner.unemployment) <- c("trade.10.name", "trade.10.unemployment", "year")
df <- merge(df, trade.partner.unemployment, by = c("trade.10.name", "year"), all.x = TRUE)

# Problem with NAs
df$trade.1.unemployment[is.na(df$trade.1.unemployment)] <- 0
df$trade.2.unemployment[is.na(df$trade.2.unemployment)] <- 0
df$trade.3.unemployment[is.na(df$trade.3.unemployment)] <- 0
df$trade.4.unemployment[is.na(df$trade.4.unemployment)] <- 0
df$trade.5.unemployment[is.na(df$trade.5.unemployment)] <- 0
df$trade.6.unemployment[is.na(df$trade.6.unemployment)] <- 0
df$trade.7.unemployment[is.na(df$trade.7.unemployment)] <- 0
df$trade.8.unemployment[is.na(df$trade.8.unemployment)] <- 0
df$trade.9.unemployment[is.na(df$trade.9.unemployment)] <- 0
df$trade.10.unemployment[is.na(df$trade.10.unemployment)] <- 0

# Creating a regional trade weighted unemployment rate 
df$unemployment.regional.trade <- df$trade.1.share*df$trade.1.unemployment + df$trade.2.share*df$trade.2.unemployment +
  df$trade.3.share*df$trade.3.unemployment + df$trade.4.share*df$trade.4.unemployment + 
  df$trade.5.share*df$trade.5.unemployment + df$trade.6.share*df$trade.6.unemployment + 
  df$trade.7.share*df$trade.7.unemployment + df$trade.8.share*df$trade.8.unemployment +
  df$trade.9.share*df$trade.9.unemployment + df$trade.10.share*df$trade.10.unemployment

# Creating values for economic data of the last year 
df.trade.previous <- as.data.frame(cbind(iso2c = as.character(df$iso2c), 
                                         year = as.numeric(as.character(df$year)) + 1, 
                                         unemployment.regional.trade.previous = df$unemployment.regional.trade))

# Merging the data frames 
df.trade.previous$iso2c <- as.character(df.trade.previous$iso2c)
df.trade.previous$year <- as.numeric(as.character(df.trade.previous$year))
df <- merge(df, df.trade.previous, by=c("iso2c","year"), all.x = TRUE, all.y = FALSE)

row.names(df) <- NULL

# Creating adjusted economic variables
# The regional variables are first-differences, even though it is not in the name 
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "unemployment.fd.deviation.trade.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.regional.trade.previous"]))
  }
  else {
    df[i, "unemployment.fd.deviation.trade.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.regional.trade"]))
  }
}

# Albania 1991 causes problems
# abc<-as.data.frame(t(df[260,]))

# Remove intermediary objects and variables
df <- df[, names(df) != "trade.1.name" & names(df) != "trade.2.name" & names(df) != "trade.3.name" &
           names(df) != "trade.4.name" & names(df) != "trade.5.name" & names(df) != "trade.6.name" &
           names(df) != "trade.7.name" & names(df) != "trade.8.name" & names(df) != "trade.9.name" &
           names(df) != "trade.10.name" & names(df) != "trade.1.share" & names(df) != "trade.2.share" & 
           names(df) != "trade.3.share" & names(df) != "trade.4.share" & names(df) != "trade.5.share" &
           names(df) != "trade.6.share" & names(df) != "trade.7.share" & names(df) != "trade.8.share" &
           names(df) != "trade.9.share" & names(df) != "trade.10.share" & names(df) != "trade.1.unemployment" &
           names(df) != "trade.2.unemployment" & names(df) != "trade.3.unemployment" & names(df) != "trade.4.unemployment" &
           names(df) != "trade.5.unemployment" & names(df) != "trade.6.unemployment" & names(df) != "trade.7.unemployment" &
           names(df) != "trade.8.unemployment" & names(df) != "trade.9.unemployment" & names(df) != "trade.10.unemployment" &
           names(df) != "trade.1.value" & names(df) != "trade.2.value" & names(df) != "trade.3.value" &
           names(df) != "trade.4.value" & names(df) != "trade.5.value" & names(df) != "trade.6.value" &
           names(df) != "trade.7.value" & names(df) != "trade.8.value" & names(df) != "trade.9.value" &
           names(df) != "trade.10.value" & names(df) != "trade.total.top10" & names(df) != "unemployment.regional.trade" &
           names(df) != "unemployment.regional.trade.previous"]
rm(trade.partner.unemployment, df.trade.previous, i)

######################################
# Comparison against international median: GDPgrowth
######################################

# Creating international medians for each year
df.intern.median <- aggregate(df$GDPgrowth, 
                              by = list(df$year), 
                              FUN = median, 
                              na.rm = TRUE)

# Renaming the variables
names(df.intern.median) <- c("year", "GDPgrowth.intern.median")

# Creating another object for the previous median 
df.intern.median.previous <- data.frame(year = as.numeric(as.character(df.intern.median$year + 1)),
                                        GDPgrowth.intern.median.previous = df.intern.median$GDPgrowth.intern.median)

# Merging it with original data frame 
df <- merge(df, df.intern.median, by="year", all.x = TRUE, all.y = FALSE)
df <- merge(df, df.intern.median.previous, by="year", all.x = TRUE, all.y = FALSE)

# Create adjusted variable 
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.deviation.intern.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.intern.median.previous"]))
  }
  else {
    df[i, "GDPgrowth.deviation.intern.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.intern.median"]))
  }
}

# Remove intermediary variables and objects 
rm(df.intern.median, df.intern.median.previous, i)
df <- df[, names(df) != "GDPgrowth.intern.median"]
df <- df[, names(df) != "GDPgrowth.intern.median.previous"]

######################################
# Comparison against international median: Unemployment.fd
######################################

# Creating international medians for each year
df.intern.median <- aggregate(df$unemployment.fd,
                              by = list(df$year), 
                              FUN = median, 
                              na.rm = TRUE)

# Renaming the variables
names(df.intern.median) <- c("year", "unemployment.fd.intern.median")

# Creating another object for the previous median 
df.intern.median.previous <- data.frame(year = as.numeric(as.character(df.intern.median$year + 1)),
                                        unemployment.fd.intern.median.previous = df.intern.median$unemployment.fd.intern.median)

# Merging it with original data frame 
df <- merge(df, df.intern.median, by="year", all.x = TRUE, all.y = FALSE)
df <- merge(df, df.intern.median.previous, by="year", all.x = TRUE, all.y = FALSE)

# Create adjusted variable 
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "unemployment.fd.deviation.intern.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.fd.intern.median.previous"]))
  }
  else {
    df[i, "unemployment.fd.deviation.intern.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.fd.intern.median"]))
  }
}

# Remove intermediary variables and objects 
rm(df.intern.median, df.intern.median.previous, i)
df <- df[, names(df) != "unemployment.fd.intern.median"]
df <- df[, names(df) != "unemployment.fd.intern.median.previous"]

######################################
# Deviation from regional variables: growth (adjusted)
######################################

# Creation of continent and regional variable 
# df$continent <- countrycode(df$iso2c, 
#                             origin = "iso2c",
#                             destination = "continent", 
#                             warn = TRUE)
df$region <- countrycode(df$iso2c, 
                         origin = "iso2c",
                         destination = "region", 
                         warn = TRUE)

# Create mean regional growth into new data frame
df.regional <- aggregate(df$GDPgrowth, 
                         by=list(df$region, df$year), 
                         FUN=mean, 
                         na.rm=TRUE)
names(df.regional) <- c("region", "year", "GDPgrowth.regional")

# Getting rid of NaN coded NAs 
df.regional$GDPgrowth.regional <- gsub("NaN", NA, df.regional$GDPgrowth.regional)

# Create new data frame for previous regional growth rate 
df.regional.previous <- data.frame(region = as.character(df.regional$region),
                                   year = as.numeric(as.character(df.regional$year)) + 1,
                                   GDPgrowth.regional.previous = df.regional$GDPgrowth.regional)

# Merge it with the original data frame 
df <- merge(df, df.regional, by=c("region", "year"), all.x = TRUE, all.y = FALSE)
df <- merge(df, df.regional.previous, by=c("region", "year"), all.x = TRUE, all.y = FALSE)

# Create relative economic measurement (adjusted)
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "GDPgrowth.deviation.regional.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.regional.previous"]))
  }
  else {
    df[i, "GDPgrowth.deviation.regional.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.regional"]))
  }
}

# Remove the intermediary variables
rm(df.regional, df.regional.previous, i)
df <- df[,names(df) != "GDPgrowth.regional"]
df <- df[,names(df) != "GDPgrowth.regional.previous"]

######################################
# Deviation from regional variables: unemployment.fd (adjusted)
######################################

# Create mean regional unemployment rate into new data frame
df.regional <- aggregate(df$unemployment.fd, 
                         by=list(df$region, df$year), 
                         FUN=mean, 
                         na.rm=TRUE)
names(df.regional) <- c("region", "year", "unemployment.fd.regional")

# Getting rid of NaN coded NAs 
df.regional$unemployment.fd.regional <- gsub("NaN", NA, df.regional$unemployment.fd.regional)

# Create new data frame for previous regional growth rate 
df.regional.previous <- data.frame(region = as.character(df.regional$region),
                                   year = as.numeric(as.character(df.regional$year)) + 1,
                                   unemployment.fd.regional.previous = df.regional$unemployment.fd.regional)

# Merge it with the original data frame 
df <- merge(df, df.regional, by=c("region", "year"), all.x = TRUE, all.y = FALSE)
df <- merge(df, df.regional.previous, by=c("region", "year"), all.x = TRUE, all.y = FALSE)

# Create relative economic measurement (adjusted)
for (i in 1:nrow(df)) {
  if (df[i, "dateleg.half"]  == 1 & is.na(df[i, "dateleg.half"])  == FALSE) {
    df[i, "unemployment.fd.deviation.regional.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.fd.regional.previous"]))
  }
  else {
    df[i, "unemployment.fd.deviation.regional.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.fd.regional"]))
  }
}

# Remove the intermediary variables
rm(df.regional, df.regional.previous, i)
df <- df[,names(df) != "unemployment.fd.regional"]
df <- df[,names(df) != "unemployment.fd.regional.previous"]
