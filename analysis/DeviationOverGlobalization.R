######################################
# Master thesis
# Lars Mehwald
# Deviation (local component) over Globalization
# 4 April 2016
######################################

options(scipen = 999) # disable scientific notation 

# fig.height = 10, fig.width = 9

######################################
# Trade
######################################

# This picture does not change when limiting trade to max 200

# Function
ggplot.deviation.local <- function(yy) { 
  ggplot(df, aes(x = Trade, y = abs(df[,names(df) == yy]))) +
    geom_point(position = "jitter", na.rm = TRUE, size = 0.2) + 
    geom_smooth(method = "loess", se = TRUE, na.rm = TRUE, color="red") + 
    ylab("") + 
    coord_cartesian(ylim = c(0, 10)) 
}

# scale_y_continuous(limits = c(,)) would remove points outside
# Otherwise I could do it like this (also in regressions): df[df$Trade < 200,]

# Bivariate regression
ols.dev.1 <- lm(abs(GDPgrowth) ~ Trade, data = df)
ols.dev.2 <- lm(abs(GDPgrowth.adj) ~ Trade, data = df)
ols.dev.3 <- lm(abs(GDPgrowth.bordering.adj) ~ Trade, data = df)
ols.dev.4 <- lm(abs(GDPgrowth.regional.adj) ~ Trade, data = df)
ols.dev.5 <- lm(abs(GDPgrowth.intern.adj) ~ Trade, data = df)
ols.dev.6 <- lm(abs(GDPgrowth.trade.adj) ~ Trade, data = df)
ols.dev.7 <- lm(abs(GDPgrowth.closest.adj) ~ Trade, data = df)
ols.dev.8 <- lm(abs(GDPgrowth.language.adj) ~ Trade, data = df)

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
                              top = textGrob("Figure 2: Scatterplot of (absolute) GDP growth and Trade", 
                                             gp = gpar(fontsize=16, font=1))))
