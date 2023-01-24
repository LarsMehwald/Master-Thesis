######################################
# Master thesis
# Lars Mehwald
# Times series of variables 
# 4 April 2016
######################################

# one function is implemented further up in the RMD file 

######################################
# Functions for plots
######################################

ggplot.econ <- function(yy, zz){
  # Subsetting
  temp <- df[df$iso2c == yy,]
  temp <- temp[temp$year != 1960,]
  # Reduced data frame
  temp <- temp[, c("year", "GDPgrowth", "GDPgrowth.adj", "GDPgrowth.bordering.adj",
                   "GDPgrowth.regional.adj", "GDPgrowth.intern.adj",
                   "GDPgrowth.trade.adj", "GDPgrowth.closest.adj", "GDPgrowth.language.adj", "legelec")]
  # From wide to long format (as required by ggplot)
  temp <- reshape(temp,
                  varying = c("GDPgrowth", "GDPgrowth.adj", "GDPgrowth.bordering.adj",
                              "GDPgrowth.regional.adj", "GDPgrowth.intern.adj",
                              "GDPgrowth.trade.adj", "GDPgrowth.closest.adj", "GDPgrowth.language.adj"),
                  v.names = "econ.performance",
                  timevar = "Index",
                  times = c("GDPgrowth", "GDPgrowth.adj", "GDPgrowth.bordering.adj",
                            "GDPgrowth.regional.adj", "GDPgrowth.intern.adj",
                            "GDPgrowth.trade.adj", "GDPgrowth.closest.adj", "GDPgrowth.language.adj"),
                  new.row.names = 1:1000,
                  direction = "long")
  temp <- temp[,names(temp) != "id"]
  # Identifying legisliative elections
  temp.election.years <- unique(temp[temp$legelec == 1 & is.na(temp$legelec) == FALSE,"year"])
  # Plotting
  plt <- ggplot(data = temp, aes(x=year, y=econ.performance), size = 1) +
    geom_line(aes(colour=Index)) +
    geom_vline(xintercept = temp.election.years, linetype = "longdash") +
    xlab("years, vertical lines indicate legislative elections") +
    ylab("economic performance") +
    ggtitle(paste(zz, " - GDP growth", sep = "")) +
    theme(plot.title = element_text(size=12, face="plain"))
  # Printing the object and subpressing message, has been moved down
  return(plt)
}

# ggplot.global has been moved up to show similarity of Luxembourg and Singapore

ggplot.unemployment <- function(yy, zz){
  # Subsetting
  temp <- df[df$iso2c == yy,]
  temp <- temp[temp$year != 1960,]
  # Reduced data frame
  temp <- temp[, c("year", "ue", "ue.fd", "ue.fd.adj", "ue.fd.bordering.adj",
                   "ue.fd.regional.adj", "ue.fd.intern.adj",
                   "ue.fd.trade.adj", "ue.fd.closest.adj", "ue.fd.language.adj", "legelec")]
  # From wide to long format (as required by ggplot)
  temp <- reshape(temp,
                  varying = c("ue", "ue.fd", "ue.fd.adj", "ue.fd.bordering.adj",
                              "ue.fd.regional.adj", "ue.fd.intern.adj",
                              "ue.fd.trade.adj", "ue.fd.closest.adj", "ue.fd.language.adj"),
                  v.names = "econ.performance",
                  timevar = "Index",
                  times = c("ue", "ue.fd", "ue.fd.adj", "ue.fd.bordering.adj",
                            "ue.fd.regional.adj", "ue.fd.intern.adj",
                            "ue.fd.trade.adj", "ue.fd.closest.adj", "ue.fd.language.adj"),
                  new.row.names = 1:1000,
                  direction = "long")
  temp <- temp[,names(temp) != "id"]
  # Identifying legisliative elections
  temp.election.years <- unique(temp[temp$legelec == 1 & is.na(temp$legelec) == FALSE,"year"])
  # Plotting
  plt <- ggplot(data = temp, aes(x=year, y=econ.performance), size = 1) +
    geom_line(aes(colour=Index)) +
    geom_vline(xintercept = temp.election.years, linetype = "longdash") +
    xlab("years, vertical lines indicate legislative elections") +
    ylab("unemployment (change)") +
    ggtitle(paste(zz, " - unemployment", sep = "")) +
    theme(plot.title = element_text(size=12, face="plain"))
  # Printing the object and subpressing message, has been moved down
  return(plt)
}

# blank grob between grobs
blank <- rectGrob(gp = gpar(col="white"))

# Create plots 
suppressWarnings(grid.arrange(ggplot.global("AU", "Australia"),
                              blank,
                              ggplot.econ("AU", "Australia"),
                              blank,
                              ggplot.unemployment("AU", "Australia"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 17: Indicators of Australia", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("AT", "Austria"),
                              blank,
                              ggplot.econ("AT", "Austria"),
                              blank,
                              ggplot.unemployment("AT", "Austria"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 18: Indicators of Austria", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("BE", "Belgium"),
                              blank,
                              ggplot.econ("BE", "Belgium"),
                              blank,
                              ggplot.unemployment("BE", "Belgium"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 19: Indicators of Belgium", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("CA", "Canada"),
                              blank,
                              ggplot.econ("CA", "Canada"),
                              blank,
                              ggplot.unemployment("CA", "Canada"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 20: Indicators of Canada", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("DK", "Denmark"),
                              blank,
                              ggplot.econ("DK", "Denmark"),
                              blank,
                              ggplot.unemployment("DK", "Denmark"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 21: Indicators of Denmark", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("FI", "Finland"),
                              blank,
                              ggplot.econ("FI", "Finland"),
                              blank,
                              ggplot.unemployment("FI", "Finland"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 22: Indicators of Finland", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("FR", "France"),
                              blank,
                              ggplot.econ("FR", "France"),
                              blank,
                              ggplot.unemployment("FR", "France"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 23: Indicators of France", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("DE", "Germany"),
                              blank,
                              ggplot.econ("DE", "Germany"),
                              blank,
                              ggplot.unemployment("DE", "Germany"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 24: Indicators of Germany", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("GB", "Great Britain"),
                              blank,
                              ggplot.econ("GB", "Great Britain"),
                              blank,
                              ggplot.unemployment("GB", "Great Britain"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 25: Indicators of Great Britain", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("GR", "Greece"),
                              blank,
                              ggplot.econ("GR", "Greece"),
                              blank,
                              ggplot.unemployment("GR", "Greece"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 26: Indicators of Greece", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("IE", "Ireland"),
                              blank,
                              ggplot.econ("IE", "Ireland"),
                              blank,
                              ggplot.unemployment("IE", "Ireland"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 27: Indicators of Ireland", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("IS", "Island"),
                              blank,
                              ggplot.econ("IS", "Island"),
                              blank,
                              ggplot.unemployment("IS", "Island"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 28: Indicators of Island", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("IL", "Israel"),
                              blank,
                              ggplot.econ("IL", "Israel"),
                              blank,
                              ggplot.unemployment("IL", "Israel"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 29: Indicators of Israel", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("IT", "Italy"),
                              blank,
                              ggplot.econ("IT", "Italy"),
                              blank,
                              ggplot.unemployment("IT", "Italy"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 30: Indicators of Italy", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("JP", "Japan"),
                              blank,
                              ggplot.econ("JP", "Japan"),
                              blank,
                              ggplot.unemployment("JP", "Japan"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 31: Indicators of Japan", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("LU", "Luxembourg"),
                              blank,
                              ggplot.econ("LU", "Luxembourg"),
                              blank,
                              ggplot.unemployment("LU", "Luxembourg"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 32: Indicators of Luxembourg", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("NL", "Netherlands"),
                              blank,
                              ggplot.econ("NL", "Netherlands"),
                              blank,
                              ggplot.unemployment("NL", "Netherlands"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 33: Indicators of the Netherlands", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("NZ", "New Zealand"),
                              blank,
                              ggplot.econ("NZ", "New Zealand"),
                              blank,
                              ggplot.unemployment("NZ", "New Zealand"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 34: Indicators of New Zealand", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("NO", "Norway"),
                              blank,
                              ggplot.econ("NO", "Norway"),
                              blank,
                              ggplot.unemployment("NO", "Norway"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 35: Indicators of Norway", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("PT", "Portugal"),
                              blank,
                              ggplot.econ("PT", "Portugal"),
                              blank,
                              ggplot.unemployment("PT", "Portugal"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 36: Indicators of Portugal", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("ES", "Spain"),
                              blank,
                              ggplot.econ("ES", "Spain"),
                              blank,
                              ggplot.unemployment("ES", "Spain"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 37: Indicators of Spain", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("SE", "Sweden"),
                              blank,
                              ggplot.econ("SE", "Sweden"),
                              blank,
                              ggplot.unemployment("SE", "Sweden"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 38: Indicators of Sweden", 
                                             gp = gpar(fontsize=12, font=1))))

cat("\\newpage")

suppressWarnings(grid.arrange(ggplot.global("US", "United States"),
                              blank,
                              ggplot.econ("US", "United States"),
                              blank,
                              ggplot.unemployment("US", "United States"),
                              nrow = 5,
                              ncol = 1,
                              top = textGrob("Figure 39: Indicators of the United States", 
                                             gp = gpar(fontsize=12, font=1))))
