######################################
# Master thesis
# Lars Mehwald
# Sample split: globalization
# 4 April 2016
######################################

# http://codealamode.blogspot.de/2013/07/plotting-regression-coefficients-with.html

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
# Split with regard to globalization 
######################################

df.split.globalization <- data.frame()

for (j in 1:nrow(v.GDP)) {
  
  # Drop observations where one value is NA  
  if (j<=2){
    df.split <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), 
                      "election.previous", "Trade", "system", "GDPperCapita.ln", "parties")]
  }
  if (j>2) {
    df.split <- df[,c("iso2c", "year", "election", as.character(v.GDP[j, 1]), as.character(v.GDP[j, 2]), 
                      "election.previous", "Trade", "system", "GDPperCapita.ln", "parties")]
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
    cutoff <- min(temp$Trade) + 0.5*(max(temp$Trade) - min(temp$Trade))
    df.split.1 <- rbind(df.split.1, temp[temp$Trade <= cutoff,])
    df.split.2 <- rbind(df.split.2, temp[temp$Trade > cutoff,])
  }
  rm(i, cutoff, temp, df.split)
  
  # Remove country, year, and trade for regression
  df.split.1 <- df.split.1[,names(df.split.1) != "iso2c" & names(df.split.1) != "year" & names(df.split.1) != "Trade"]
  df.split.2 <- df.split.2[,names(df.split.2) != "iso2c" & names(df.split.2) != "year" & names(df.split.2) != "Trade"]
  
  # sample 1 contains low globalization, sample 2 high globalization 
  ols.split.1 <- lm(election ~ ., data = df.split.1)
  ols.split.2 <- lm(election ~ ., data = df.split.2)
  temp <- data.frame(economic.performance = rep(v.GDP[j,1], 2),
                     sample = c("low.globalization", "high.globalization"),
                     coefficient = c(as.numeric(ols.split.1$coefficients[2]),
                                     as.numeric(ols.split.2$coefficients[2])),
                     standard.errors = c(as.numeric(coef(summary(ols.split.1))[2, "Std. Error"]),
                                         as.numeric(coef(summary(ols.split.2))[2, "Std. Error"])))
  df.split.globalization <- rbind(df.split.globalization, temp)
}

rm(j, ols.split.1, ols.split.2, temp, df.split.1, df.split.2)

######################################
# Visualization
######################################

# Upper and lower bounds
df.split.globalization$lb <- df.split.globalization$coefficient - 1.96 * df.split.globalization$standard.errors
df.split.globalization$ub <- df.split.globalization$coefficient + 1.96 * df.split.globalization$standard.errors

pd <- position_dodge(width=0.2)
plt.globalization <- ggplot(df.split.globalization, aes(economic.performance, coefficient, color=sample)) +
  geom_point(aes(shape=sample),size=4, position=pd) + 
  scale_color_manual(name="sample",values=c("coral","steelblue")) + # colors of c.i. and points
  scale_shape_manual(name="sample",values=c(17,19)) + # shape of the coefficient points 
  theme_bw() + 
  scale_y_continuous("Economic vote") +
  geom_errorbar(aes(ymin = df.split.globalization$lb, ymax = df.split.globalization$ub), width=0.1, position = pd) + 
  scale_x_discrete("") + 
  theme(axis.text.x=element_text(angle=90)) +
  ggtitle("Figure 15: Sample split by globalization for each country")

# Print 
suppressWarnings(print(plt.globalization))
  
# Remove intermediary objects
rm(pd, v.GDP, df.split.globalization, plt.globalization)
