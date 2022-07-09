## effect of temperature on recruitment estimates from the stock assessment model

library(tidyverse)
library(mgcv)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## Read in data 
dat <- read.csv("./data/cod_pollock_assessment_2020_SAFEs.csv",
                row.names = 1)

dat <- dat %>%
  mutate(log_recruits = log(pollR1.2020),
         year = 1969:2020) %>%
  select(year, log_recruits)
  

ggplot(dat, aes(year, log_recruits)) +
  geom_line() +
  geom_point()


# load GOA sst anomalies

d1 <- read.csv("./data/regional_north_pacific_ersst_anomaly_time_series.csv")

d1 <- d1 %>%
  dplyr::filter(region == "Gulf_of_Alaska") %>%
  dplyr::select(year, annual.anomaly.unsmoothed, winter.anomaly.unsmoothed) %>%
  rename(sst_annual = annual.anomaly.unsmoothed,
         sst_winter = winter.anomaly.unsmoothed) %>%
  mutate(sst_annual_lag1 = lag(sst_annual),
         sst_annual_lag2 = lag(sst_annual, n = 2),
         sst_winter_lag1 = lag(sst_winter)) 

d1$sst_annual_2yr_mean = rowMeans(d1[c(4,5)]) # so 2-yr annual mean is year before recruitment and two years before
d1$sst_winter_2yr_mean = rowMeans(d1[c(3,6)]) # 2-yr winter mean is winter of recruitment year and winter before


# and join
dat <- left_join(dat, d1) 

# now model
mod1 <- gamm(log_recruits ~  s(sst_annual_lag1,  k=5),
             correlation = corAR1(), data=dat)

gam.check(mod1$gam)
plot(mod1$gam)
summary(mod1$gam)
anova(mod1$gam)

mod2 <- gamm(log_recruits ~  s(sst_annual_lag2,  k=5),
             correlation = corAR1(), data=dat)

gam.check(mod2$gam)
plot(mod2$gam)
summary(mod2$gam)
anova(mod2$gam)

mod3 <- gamm(log_recruits ~  s(sst_annual_2yr_mean,  k=5),
             correlation = corAR1(), data=dat)

gam.check(mod3$gam)
plot(mod3$gam)
summary(mod3$gam)
anova(mod3$gam)


mod4 <- gamm(log_recruits ~  s(sst_winter,  k=5),
             correlation = corAR1(), data=dat)

gam.check(mod4$gam)
plot(mod4$gam)
summary(mod4$gam)
anova(mod4$gam)


mod5 <- gamm(log_recruits ~  s(sst_winter_lag1,  k=5),
             correlation = corAR1(), data=dat)

gam.check(mod5$gam)
plot(mod5$gam)
summary(mod5$gam)
anova(mod5$gam)


mod6 <- gamm(log_recruits ~  s(sst_winter_2yr_mean,  k=5),
             correlation = corAR1(), data=dat)

gam.check(mod6$gam)
plot(mod6$gam)
summary(mod6$gam)
anova(mod6$gam)

AIC <- AIC (mod1, mod2, mod3, mod4, mod5, mod6)
AIC
