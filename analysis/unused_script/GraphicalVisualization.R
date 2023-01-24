# Alternative to ggplot in RMD

# Using matplot 
for (i in 1:length(unique(df$iso2c))) {
  # Subsetting
  temp <- df[df$iso2c == unique(df$iso2c)[1],]
  temp <- temp[temp$year != 1960,]
  # To Matrix
  temp <- temp[, c("GDPgrowth", "GDPgrowth.adjusted", "GDPgrowth.deviation.bordering",
                   "GDPgrowth.deviation.regional", "GDPgrowth.deviation.intern", 
                   "GDPgrowth.deviation.trade", "GDPgrowth.deviation.closest")]
  temp <- as.matrix(temp)
  # Scatterplotting
  nn <- ncol(temp)
  layout(matrix(c(1,2),nrow=1), width=c(4,3)) # Devide device in 2 sections, define the width of each 
  par(mar=c(5,4,4,0)) #No margin on the right side
  matplot(temp, type = c("l"), pch=6, col = 1:7) # type b l o # pch sets symbol of corner 
  par(mar=c(5,0,4,2)) #No margin on the left side
  plot(c(0,1),type="n", axes=F, xlab="", ylab="")
  legend("center", colnames(temp),
         col=seq_len(nn),cex=0.8,fill=seq_len(nn))
}
