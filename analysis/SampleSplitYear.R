######################################
# Master thesis
# Lars Mehwald
# Sample split: year
# 4 April 2016
######################################

# Measures of GDP used 
v.GDP <- data.frame(local = c("GDPgrowth", "GDPgrowth.adj", "GDPgrowth.bordering.adj", 
                              "GDPgrowth.regional.adj", "GDPgrowth.intern.adj", 
                              "GDPgrowth.trade.adj", "GDPgrowth.closest.adj", 
                              "GDPgrowth.language.adj"),
                    international = c(NA, NA, "GDPgrowth.bordering.int.adj", 
                                      "GDPgrowth.regional.int.adj", "GDPgrowth.intern.int.adj", 
                                      "GDPgrowth.trade.int.adj", "GDPgrowth.closest.int.adj", 
                                      "GDPgrowth.language.int.adj"))
######################################
# Split with regard to year
######################################

df.split.year <- data.frame()

for (j in 1:nrow(v.GDP)) {
  
  # Drop observations where one value is NA  
  if (j<=2){
    df.split <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), 
                      "election.previous", "system", "GDPperCapita.ln", "parties")]
  }
  if (j>2) {
    df.split <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), as.character(v.GDP[j, 2]), 
                      "election.previous", "system", "GDPperCapita.ln", "parties")]
  }
  df.split <- na.omit(df.split)
  
  # Get rid of outliers
  df.split <- df.split[df.split$iso2c != "LU",]
  df.split <- df.split[which(df.split$iso2c != "IL" | df.split$year != 1996),]
  
  # Split the sample into low and high globalized country-year observations
  df.split.1 <- data.frame()
  df.split.2 <- data.frame()
  
  for(i in 1:length(unique(df.split$iso2c))){
    temp <- df.split[df.split$iso2c == unique(df.split$iso2c)[i],]
    cutoff <- min(temp$year) + 0.5*(max(temp$year) - min(temp$year))
    df.split.1 <- rbind(df.split.1, temp[temp$year <= cutoff,])
    df.split.2 <- rbind(df.split.2, temp[temp$year > cutoff,])
  }
  rm(i, cutoff, temp, df.split)
  
  # Remove country and year for regression
  df.split.1 <- df.split.1[,names(df.split.1) != "iso2c" & names(df.split.1) != "year"]
  df.split.2 <- df.split.2[,names(df.split.2) != "iso2c" & names(df.split.2) != "year"]
  
  # sample 1 contains early country observations, sample 2 later observations 
  ols.split.1 <- lm(election ~ ., data = df.split.1)
  ols.split.2 <- lm(election ~ ., data = df.split.2)
  temp <- data.frame(economic.performance = rep(v.GDP[j, 1], 2),
                     sample = c("early", "late"),
                     coefficient = c(as.numeric(ols.split.1$coefficients[2]),
                                     as.numeric(ols.split.2$coefficients[2])),
                     standard.errors = c(as.numeric(coef(summary(ols.split.1))[2, "Std. Error"]),
                                         as.numeric(coef(summary(ols.split.2))[2, "Std. Error"])))
  df.split.year <- rbind(df.split.year, temp)
}

rm(j, ols.split.1, ols.split.2, temp, df.split.1, df.split.2)

######################################
# Visualization
######################################

# Upper and lower bound
df.split.year$lb <- df.split.year$coefficient - 1.96 * df.split.year$standard.errors
df.split.year$ub <- df.split.year$coefficient + 1.96 * df.split.year$standard.errors

pd <- position_dodge(width=0.2)
plt.year <- ggplot(df.split.year, aes(economic.performance, coefficient, color=sample)) +
  geom_point(aes(shape=sample), size=4, position=pd) + 
  scale_color_manual(name="sample", values=c("coral", "steelblue")) + 
  scale_shape_manual(name="sample", values=c(17,19)) + 
  theme_bw() + 
  scale_y_continuous("Economic vote") +
  geom_errorbar(aes(ymin = lb, ymax = ub), width=0.1, position = pd) + 
  scale_x_discrete("") +
  theme(axis.text.x=element_text(angle=90)) +
  ggtitle("Figure 16: Sample split by year for each country")

# Print the object
suppressWarnings(print(plt.year))

# Remove intermediary objects
rm(pd, v.GDP, df.split.year, plt.year)
