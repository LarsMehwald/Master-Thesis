ols.1.m <- lm(election ~ GDPgrowth + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df)
ols.2.m <- lm(election ~ GDPgrowth.adjusted + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df)
ols.3.m <- lm(election ~ GDPgrowth.deviation.bordering + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df)
# ols.4.m <- lm(election ~ GDPgrowth.deviation.regional + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df)
# ols.5.m <- lm(election ~ GDPgrowth.deviation.intern + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df)
ols.6.m <- lm(election ~ GDPgrowth.deviation.trade + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df)
ols.7.m <- lm(election ~ GDPgrowth.deviation.closest + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df)


stargazer(ols.1.m, ols.2.m, ols.3.m, 
          #          ols.4.m, ols.5.m, 
          ols.6.m, ols.7.m,
          type = "latex",
          digits = 1,
          title = "Linear regressions, including media system",
          font.size = "scriptsize", # tiny scriptsize footnotesize small normalsize large Large LARGE huge Huge
          header = FALSE,
          omit.stat = c("f", "ser"))

ols.i.2.1.m <- lm(election ~ GDPgrowth*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df, subset = df$iso2c != "LU")
ols.i.2.2.m <- lm(election ~ GDPgrowth.adjusted*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df, subset = df$iso2c != "LU")
ols.i.2.3.m <- lm(election ~ GDPgrowth.deviation.bordering*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df, subset = df$iso2c != "LU")
# ols.i.2.4.m <- lm(election ~ GDPgrowth.deviation.regional*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df, subset = df$iso2c != "LU")
# ols.i.2.5.m <- lm(election ~ GDPgrowth.deviation.intern*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df, subset = df$iso2c != "LU")
ols.i.2.6.m <- lm(election ~ GDPgrowth.deviation.trade*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df, subset = df$iso2c != "LU")
ols.i.2.7.m <- lm(election ~ GDPgrowth.deviation.closest*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system, data = df, subset = df$iso2c != "LU")

ols.i.2.1.m.iso2c <- lm(election ~ GDPgrowth*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system + iso2c, data = df, subset = df$iso2c != "LU")
ols.i.2.2.m.iso2c <- lm(election ~ GDPgrowth.adjusted*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system + iso2c, data = df, subset = df$iso2c != "LU")
ols.i.2.3.m.iso2c <- lm(election ~ GDPgrowth.deviation.bordering*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system + iso2c, data = df, subset = df$iso2c != "LU")
# ols.i.2.4.m.iso2c <- lm(election ~ GDPgrowth.deviation.regional*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system + iso2c, data = df, subset = df$iso2c != "LU")
# ols.i.2.5.m.iso2c <- lm(election ~ GDPgrowth.deviation.intern*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system + iso2c, data = df, subset = df$iso2c != "LU")
ols.i.2.6.m.iso2c <- lm(election ~ GDPgrowth.deviation.trade*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system + iso2c, data = df, subset = df$iso2c != "LU")
ols.i.2.7.m.iso2c <- lm(election ~ GDPgrowth.deviation.closest*TradeOpenness + GDPperCapita.ln + NumberParties.Ordinal + media.system + iso2c, data = df, subset = df$iso2c != "LU")

print("media system")

interplot.linear(ols.i.2.1.m, "GDPgrowth", "TradeOpenness")
interplot.linear(ols.i.2.2.m, "GDPgrowth.adjusted", "TradeOpenness")
interplot.linear(ols.i.2.3.m, "GDPgrowth.deviation.bordering", "TradeOpenness")
# interplot.linear(ols.i.2.4.m, "GDPgrowth.deviation.regional", "TradeOpenness")
# interplot.linear(ols.i.2.5.m, "GDPgrowth.deviation.intern", "TradeOpenness")
interplot.linear(ols.i.2.6.m, "GDPgrowth.deviation.trade", "TradeOpenness")
interplot.linear(ols.i.2.7.m, "GDPgrowth.deviation.closest", "TradeOpenness")

print("media system and country dummies")

interplot.linear(ols.i.2.1.m.iso2c, "GDPgrowth", "TradeOpenness")
interplot.linear(ols.i.2.2.m.iso2c, "GDPgrowth.adjusted", "TradeOpenness")
interplot.linear(ols.i.2.3.m.iso2c, "GDPgrowth.deviation.bordering", "TradeOpenness")
# interplot.linear(ols.i.2.4.m.iso2c, "GDPgrowth.deviation.regional", "TradeOpenness")
# interplot.linear(ols.i.2.5.m.iso2c, "GDPgrowth.deviation.intern", "TradeOpenness")
interplot.linear(ols.i.2.6.m.iso2c, "GDPgrowth.deviation.trade", "TradeOpenness")
interplot.linear(ols.i.2.7.m.iso2c, "GDPgrowth.deviation.closest", "TradeOpenness")

stargazer(ols.i.2.1.m, ols.i.2.2.m, ols.i.2.3.m, 
          #          ols.i.2.4.m, ols.i.2.5.m, 
          ols.i.2.6.m, ols.i.2.7.m,
          type = "latex",
          digits = 1,
          title = "Linear regressions: economic performance with TradeOpenness interacted, media system",
          font.size = "scriptsize",
          header = FALSE,
          omit.stat = c("f", "ser"))

stargazer(ols.i.2.1.m.iso2c, ols.i.2.2.m.iso2c, ols.i.2.3.m.iso2c, 
          #          ols.i.2.4.m.iso2c, ols.i.2.5.m.iso2c, 
          ols.i.2.6.m.iso2c, ols.i.2.7.m.iso2c,
          type = "latex",
          digits = 1,
          title = "Linear regressions: economic performance with TradeOpenness interacted, media system and country dummy",
          font.size = "scriptsize",
          header = FALSE,
          keep = c(1:8, 15:16, 33:38), # 8 starts number of parties, 15 starts media 33 start interaction 
          omit.stat = c("f", "ser"))

