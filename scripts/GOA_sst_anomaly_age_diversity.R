## effect of temperature on age diversity

library(tidyverse)
library(mgcv)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## Read in data 
dat <- read.csv("./data/shannon.age.diversity.acoustic.trawl.data.csv",
                 row.names = 1)

ggplot(dat, aes(year, shannon)) +
  geom_line() +
  geom_point()


# load GOA sst anomalies

d1 <- read.csv("./data/regional_north_pacific_ersst_anomaly_time_series.csv")

d1 <- d1 %>%
  dplyr::filter(region == "Gulf_of_Alaska") %>%
  dplyr::select(year, annual.anomaly.unsmoothed) %>%
  rename(sst = annual.anomaly.unsmoothed) %>%
  mutate(sst2 = zoo::rollmean(sst, 2, align = "right", fill = NA),
         sst3 = zoo::rollmean(sst, 3, align = "right", fill = NA))


# lag shannon one year to match sst
dat_lag <- dat %>%
  mutate(year = year - 1)

# and join
dat_lag <- left_join(dat_lag, d1) %>%
  na.omit() # dropping missing years for ar() model fitting! 


# now model
mod1 <- gamm(shannon ~  s(sst,  k=4),
              correlation = corAR1(), data=dat_lag)

gam.check(mod1$gam)
plot(mod1$gam)
summary(mod1$gam)
anova(mod1$gam)


mod2 <- gamm(shannon ~  s(sst2,  k=4),
             correlation = corAR1(), data=dat_lag)

gam.check(mod2$gam)
plot(mod2$gam)
summary(mod2$gam)
anova(mod2$gam)

mod3 <- gamm(shannon ~  s(sst3,  k=4),
             correlation = corAR1(), data=dat_lag)

gam.check(mod3$gam)
plot(mod3$gam)
summary(mod3$gam)
anova(mod3$gam)

AIC <- AIC(mod1, mod2, mod3)

AIC

AIC_table <- data.frame(response = "Shannon age diversity",
                        model = c("s(sst_unsmoothed,  k=4), correlation = corAR1()",
                                  "s(sst_2yr_mean,  k=4), correlation = corAR1()",
                                  "s(sst_3yr_mean,  k=4), correlation = corAR1()"),
                        AIC = AIC$AIC,
                        delta_AIC = (AIC$AIC - min(AIC$AIC)))

AIC_table <- AIC_table %>%
  arrange(delta_AIC)

# save
write.csv(AIC_table, "output/Shannon_age_diversity_AIC_table.csv", row.names = F)

# predict and plot

new_dat <- data.frame(sst3 = seq(min(dat_lag$sst3), max(dat_lag$sst3), length.out = 100)) 

pred <- predict(mod3$gam, newdata = new_dat, se.fit = T)

plot <- data.frame(sst3 = new_dat$sst3,
                   estimate = pred$fit, 
                   LCI = pred$fit - 1.96*pred$se.fit,
                   UCI = pred$fit + 1.96*pred$se.fit)


ggplot(plot) +
  aes(x = sst3, y = estimate) +
  geom_point(data = dat_lag, aes(x = sst3, y = shannon), size = 2.5) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  labs(x = "Three year running mean SST anomaly", y = "Shannon age diversity") 

ggsave("./figs/Shannon_age_diversity_goa_SST_anomaly.png", width = 6, height = 4, units = 'in')

# refit best model to scaled data

dat_scaled <- dat_lag %>%
  mutate(scaled_diversity = scale(shannon))

mod4 <- gamm(scaled_diversity ~  s(sst3,  k=4),
             correlation = corAR1(), data=dat_scaled)

pred <- predict(mod4$gam, newdata = new_dat, se.fit = T)

plot <- data.frame(sst3 = new_dat$sst3,
                   estimate = pred$fit, 
                   LCI = pred$fit - 1.96*pred$se.fit,
                   UCI = pred$fit + 1.96*pred$se.fit)


ggplot(plot) +
  aes(x = sst3, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  labs(x = "Three year running mean SST anomaly", y = "Shannon age diversity") 


