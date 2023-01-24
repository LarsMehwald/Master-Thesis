######################################
# Master thesis
# Lars Mehwald
# Appendix Deviation (local component) over Globalization
# 4 April 2016
######################################

options(scipen = 999) # disable scientific notation 

# fig.height = 10, fig.width = 9

######################################
# KOF.index
######################################

# by using abs() error message indicates that NAs have been created - this corresponds exactly to the number of NAs in the variable, e.g. GDPgrowth.intern.adj

# Function
ggplot.deviation.local <- function(yy) { 
  ggplot(df, aes(x = KOF.index, y = abs(df[,names(df) == yy]))) +
    geom_point(position = "jitter", na.rm = TRUE, size = 0.2) + 
    geom_smooth(method = "loess", se = TRUE, na.rm = TRUE, color="red") + 
    ylab("") + 
    coord_cartesian(ylim = c(0, 10)) 
}

# Bivariate regression
ols.dev.1 <- lm(abs(GDPgrowth) ~ KOF.index, data = df)
ols.dev.2 <- lm(abs(GDPgrowth.adj) ~ KOF.index, data = df)
ols.dev.3 <- lm(abs(GDPgrowth.bordering.adj) ~ KOF.index, data = df)
ols.dev.4 <- lm(abs(GDPgrowth.regional.adj) ~ KOF.index, data = df)
ols.dev.5 <- lm(abs(GDPgrowth.intern.adj) ~ KOF.index, data = df)
ols.dev.6 <- lm(abs(GDPgrowth.trade.adj) ~ KOF.index, data = df)
ols.dev.7 <- lm(abs(GDPgrowth.closest.adj) ~ KOF.index, data = df)
ols.dev.8 <- lm(abs(GDPgrowth.language.adj) ~ KOF.index, data = df)


suppressWarnings(grid.arrange(ggplot.deviation.local("GDPgrowth") + 
                                ggtitle(paste("coef. GDPgrowth:", round(as.numeric(coef(ols.dev.1)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.adj") + 
                                ggtitle(paste("coef. GDPgrowth.adj:", round(as.numeric(coef(ols.dev.2)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.bordering.adj") + 
                                ggtitle(paste("coef. GDPgrowth.bordering.adj:", round(as.numeric(coef(ols.dev.3)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.regional.adj") + 
                                ggtitle(paste("coef. GDPgrowth.regional.adj:", round(as.numeric(coef(ols.dev.4)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.intern.adj") + 
                                ggtitle(paste("coef. GDPgrowth.intern.adj:", round(as.numeric(coef(ols.dev.5)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.trade.adj") + 
                                ggtitle(paste("coef. GDPgrowth.trade.adj:", round(as.numeric(coef(ols.dev.6)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.closest.adj") + 
                                ggtitle(paste("coef. GDPgrowth.closest.adj:", round(as.numeric(coef(ols.dev.7)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.language.adj") + 
                                ggtitle(paste("coef. GDPgrowth.language.adj:", round(as.numeric(coef(ols.dev.8)[2]), 3))),
                              nrow = 4,
                              top = textGrob("Figure 44: Scatterplot of (absolute) GDP growth and KOF.index", 
                                             gp = gpar(fontsize=16, font=1))))

cat("\\newpage")

######################################
# KOF. econ.global
######################################

# Function
ggplot.deviation.local <- function(yy) { 
  ggplot(df, aes(x = KOF.econ.global, y = abs(df[,names(df) == yy]))) +
    geom_point(position = "jitter", na.rm = TRUE, size = 0.2) + 
    geom_smooth(method = "loess", se = TRUE, na.rm = TRUE, color="red") + 
    ylab("") + 
    coord_cartesian(ylim = c(0, 10)) 
}

# Bivariate regression
ols.dev.1 <- lm(abs(GDPgrowth) ~ KOF.econ.global, data = df)
ols.dev.2 <- lm(abs(GDPgrowth.adj) ~ KOF.econ.global, data = df)
ols.dev.3 <- lm(abs(GDPgrowth.bordering.adj) ~ KOF.econ.global, data = df)
ols.dev.4 <- lm(abs(GDPgrowth.regional.adj) ~ KOF.econ.global, data = df)
ols.dev.5 <- lm(abs(GDPgrowth.intern.adj) ~ KOF.econ.global, data = df)
ols.dev.6 <- lm(abs(GDPgrowth.trade.adj) ~ KOF.econ.global, data = df)
ols.dev.7 <- lm(abs(GDPgrowth.closest.adj) ~ KOF.econ.global, data = df)
ols.dev.8 <- lm(abs(GDPgrowth.language.adj) ~ KOF.econ.global, data = df)


suppressWarnings(grid.arrange(ggplot.deviation.local("GDPgrowth") + 
                                ggtitle(paste("coef. GDPgrowth:", round(as.numeric(coef(ols.dev.1)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.adj") + 
                                ggtitle(paste("coef. GDPgrowth.adj:", round(as.numeric(coef(ols.dev.2)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.bordering.adj") + 
                                ggtitle(paste("coef. GDPgrowth.bordering.adj:", round(as.numeric(coef(ols.dev.3)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.regional.adj") + 
                                ggtitle(paste("coef. GDPgrowth.regional.adj:", round(as.numeric(coef(ols.dev.4)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.intern.adj") + 
                                ggtitle(paste("coef. GDPgrowth.intern.adj:", round(as.numeric(coef(ols.dev.5)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.trade.adj") + 
                                ggtitle(paste("coef. GDPgrowth.trade.adj:", round(as.numeric(coef(ols.dev.6)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.closest.adj") + 
                                ggtitle(paste("coef. GDPgrowth.closest.adj:", round(as.numeric(coef(ols.dev.7)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.language.adj") + 
                                ggtitle(paste("coef. GDPgrowth.language.adj:", round(as.numeric(coef(ols.dev.8)[2]), 3))),
                              nrow = 4,
                              top = textGrob("Figure 45: Scatterplot of (absolute) GDP growth and KOF.econ.global", 
                                             gp = gpar(fontsize=16, font=1))))

cat("\\newpage")

######################################
# KOF.econ.global.flows
######################################

# Function
ggplot.deviation.local <- function(yy) { 
  ggplot(df, aes(x = KOF.econ.global.flows, y = abs(df[,names(df) == yy]))) +
    geom_point(position = "jitter", na.rm = TRUE, size = 0.2) + 
    geom_smooth(method = "loess", se = TRUE, na.rm = TRUE, color="red") + 
    ylab("") + 
    coord_cartesian(ylim = c(0, 10)) 
}

# Bivariate regression
ols.dev.1 <- lm(abs(GDPgrowth) ~ KOF.econ.global.flows, data = df)
ols.dev.2 <- lm(abs(GDPgrowth.adj) ~ KOF.econ.global.flows, data = df)
ols.dev.3 <- lm(abs(GDPgrowth.bordering.adj) ~ KOF.econ.global.flows, data = df)
ols.dev.4 <- lm(abs(GDPgrowth.regional.adj) ~ KOF.econ.global.flows, data = df)
ols.dev.5 <- lm(abs(GDPgrowth.intern.adj) ~ KOF.econ.global.flows, data = df)
ols.dev.6 <- lm(abs(GDPgrowth.trade.adj) ~ KOF.econ.global.flows, data = df)
ols.dev.7 <- lm(abs(GDPgrowth.closest.adj) ~ KOF.econ.global.flows, data = df)
ols.dev.8 <- lm(abs(GDPgrowth.language.adj) ~ KOF.econ.global.flows, data = df)


suppressWarnings(grid.arrange(ggplot.deviation.local("GDPgrowth") + 
                                ggtitle(paste("coef. GDPgrowth:", round(as.numeric(coef(ols.dev.1)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.adj") + 
                                ggtitle(paste("coef. GDPgrowth.adj:", round(as.numeric(coef(ols.dev.2)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.bordering.adj") + 
                                ggtitle(paste("coef. GDPgrowth.bordering.adj:", round(as.numeric(coef(ols.dev.3)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.regional.adj") + 
                                ggtitle(paste("coef. GDPgrowth.regional.adj:", round(as.numeric(coef(ols.dev.4)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.intern.adj") + 
                                ggtitle(paste("coef. GDPgrowth.intern.adj:", round(as.numeric(coef(ols.dev.5)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.trade.adj") + 
                                ggtitle(paste("coef. GDPgrowth.trade.adj:", round(as.numeric(coef(ols.dev.6)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.closest.adj") + 
                                ggtitle(paste("coef. GDPgrowth.closest.adj:", round(as.numeric(coef(ols.dev.7)[2]), 3))),
                              ggplot.deviation.local("GDPgrowth.language.adj") + 
                                ggtitle(paste("coef. GDPgrowth.language.adj:", round(as.numeric(coef(ols.dev.8)[2]), 3))),
                              nrow = 4,
                              top = textGrob("Figure 46: Scatterplot of (absolute) GDP growth and KOF.econ.global.flows", 
                                             gp = gpar(fontsize=16, font=1))))
