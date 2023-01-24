######################################
# Master thesis
# Lars Mehwald
# Data manipulations: Relative economic variables, international
# 4 April 2016
######################################

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
    df[i, "GDPgrowth.intern.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.previous"])) - as.numeric(as.character(df[i, "GDPgrowth.intern.median.previous"]))
    df[i, "GDPgrowth.intern.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.intern.median.previous"]))
  }
  else {
    df[i, "GDPgrowth.intern.adj"] <- as.numeric(as.character(df[i, "GDPgrowth"])) - as.numeric(as.character(df[i, "GDPgrowth.intern.median"]))
    df[i, "GDPgrowth.intern.int.adj"] <- as.numeric(as.character(df[i, "GDPgrowth.intern.median"]))
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
    df[i, "ue.fd.intern.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.previous"])) - as.numeric(as.character(df[i, "unemployment.fd.intern.median.previous"]))
    df[i, "ue.fd.intern.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.intern.median.previous"]))
  }
  else {
    df[i, "ue.fd.intern.adj"] <- as.numeric(as.character(df[i, "unemployment.fd"])) - as.numeric(as.character(df[i, "unemployment.fd.intern.median"]))
    df[i, "ue.fd.intern.int.adj"] <- as.numeric(as.character(df[i, "unemployment.fd.intern.median"]))
  }
}

# Remove intermediary variables and objects 
rm(df.intern.median, df.intern.median.previous, i)
df <- df[, names(df) != "unemployment.fd.intern.median"]
df <- df[, names(df) != "unemployment.fd.intern.median.previous"]
