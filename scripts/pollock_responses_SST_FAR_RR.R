# fit regressions to three covariates for each response variable - 
# SST anomaly, and corresponding FAR and RR values
# window (winter/annual, degree of smoothing) already selected via model comparison

library(tidyverse)
library(mgcv)
library(gamm4)


theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load data

d1 <- read.csv("./data/attribution_and_adaptation_data.csv")

d2 <- read.csv("./data/GOA_annual_FAR_Risk_Ratio_with_uncertainty.csv")

d3 <- read.csv("./data/GOA_3yr_annual_FAR_Risk_Ratio_with_uncertainty.csv")

d4 <- read.csv("./data/GOA_winter_2yr_FAR_Risk_Ratio_with_uncertainty.csv")

d5 <- read.csv("./data/regional_north_pacific_ersst_anomaly_time_series.csv")

d5 <- d5 %>%
  filter(region == "Gulf_of_Alaska")

d6 <- read.csv("./data/cohort_weight_age.csv")

# shannon age diversity first

shann_dat <- d1 %>%
  filter(variable == "Spawning stock age diversity") %>%
  select(-LCI, -UCI) %>%
  na.omit() # dropping missing years to allow AR(1) fit

# best covariate is sst for 1-3 years before sampling
# for 2020 Shannon this is 3yr mean sst value of 2.168424

# check sst anomalies
tail(d5) # this is equal to the 2018 value in this data frame; use 2018 value for FAR and RR as well!

# select the correct lag for comparison 

temp <- d5 %>%
  select(year, annual.anomaly.three.yr.running.mean) %>%
  mutate(year = year +2) %>%
  rename(Year = year,
         sst3 = annual.anomaly.three.yr.running.mean)

shann_dat <- left_join(shann_dat, temp)

# now FAR and RR

temp <- d3 %>%
  select(-LCI, -UCI) %>%
  mutate(Year = Year + 2)  %>%
  pivot_wider(values_from = Estimate, names_from = variable) %>%
  rename(FAR = 'Fraction of Attributable Risk',
         RR = 'Risk ratio')

shann_dat <- left_join(shann_dat, temp)

# now fit models

shann_mod1 <- gamm(Estimate ~  s(sst3,  k=4), # 2020 diversity compared with 2017-2019 sst
             correlation = corAR1(), data=shann_dat)

gam.check(shann_mod1$gam)
plot(shann_mod1$gam, residuals = T, pch = 19)
summary(shann_mod1$gam)
anova(shann_mod1$gam)

shann_mod2 <- gamm(Estimate ~  s(FAR,  k=4), # 2020 diversity compared with 2017-2019 sst
                   correlation = corAR1(), data=shann_dat)

gam.check(shann_mod2$gam)
plot(shann_mod2$gam, residuals = T, pch = 19)
summary(shann_mod2$gam)
anova(shann_mod2$gam)

shann_dat$log_RR <- log(shann_dat$RR, 10)
shann_mod3 <- gamm(Estimate ~  s(log_RR,  k=4), # 2020 diversity compared with 2017-2019 sst
                   correlation = corAR1(), data=shann_dat)

gam.check(shann_mod3$gam)
plot(shann_mod3$gam, residuals = T, pch = 19)
plot.summary(shann_mod3$gam)
anova(shann_mod3$gam)

########
# recruits

recr <- d1 %>%
  filter(variable == "Log age1 recruits") %>%
  select(Year, Estimate)

# best SST covariate is winter SST year of and year before sampling

# double check in d5

check <- data.frame(Year = d5$year,
                    winter.2yr = zoo::rollmean(d5$winter.anomaly.unsmoothed, 2, align = "right", fill = NA))

tail(check); tail(d5) # d5 is left aligned, needs to be lagged 1 yr

temp <- d5 %>%
  select(year, winter.anomaly.two.yr.running.mean) %>%
  mutate(year = lead(year)) %>%
  rename(Year = year,
         winter.sst2 = winter.anomaly.two.yr.running.mean) %>%
  na.omit()

recr <- left_join(recr, temp)

temp <- d4 %>%
  select(-LCI, -UCI) %>%
  mutate(Year = Year + 1)  %>%
  pivot_wider(values_from = Estimate, names_from = variable) %>%
  rename(FAR = 'Fraction of Attributable Risk',
         RR = 'Risk ratio')

recr <- left_join(recr, temp)

# now fit models

recr_mod1 <- gamm(Estimate ~  s(winter.sst2,  k=5), 
                   correlation = corAR1(), data=recr)

gam.check(recr_mod1$gam)
plot(recr_mod1$gam, residuals = T, pch = 19)
summary(recr_mod1$gam)
anova(recr_mod1$gam)

recr_mod2 <- gamm(Estimate ~  s(FAR,  k=5), 
                   correlation = corAR1(), data=recr)

gam.check(recr_mod2$gam)
plot(recr_mod2$gam, residuals = T, pch = 19)
summary(recr_mod2$gam)
anova(recr_mod2$gam)

recr$log_RR = log(recr$RR, 10)
recr_mod3 <- gamm(Estimate ~  s(log_RR,  k=5), 
                   correlation = corAR1(), data=recr)

gam.check(recr_mod3$gam)
plot(recr_mod3$gam, residuals = T, pch = 19)
summary(recr_mod3$gam)
anova(recr_mod3$gam)

# now weight

weight_dat <- read.csv("./data/cohort_weight_age.csv")

# make sure we're coding factors
weight_dat <- weight_dat %>%
  mutate(maturity_fac = as.factor(maturity_table_3),
         sex_fac = as.factor(sex.code),
         age_fac = as.factor(Age),
         cohort_fac = as.factor(cohort))

# replace western GOA with GOA-wide sst anomalies
weight_dat <- weight_dat %>%
  select(-annual.wSST, -prevyr_annual.wSST)

temp <- d5 %>%
  filter(region == "Gulf_of_Alaska") %>%
  select(year, annual.anomaly.unsmoothed) %>%
  rename(sst = annual.anomaly.unsmoothed) %>%
  mutate(sst = lag(sst))

weight_dat <- left_join(weight_dat, temp)

temp <- d2 %>%
  select(-LCI, -UCI) %>%
  pivot_wider(values_from = Estimate, names_from = variable) %>%
  rename(FAR = 'Fraction of Attributable Risk',
         RR = 'Risk ratio',
         year = Year) %>%
  mutate(FAR = lag(FAR),
         RR = lag(RR))

weight_dat <- left_join(weight_dat, temp)

# and model 

weight_mod1 <- gamm4(sc.weight ~  s(sst,  k=4) + maturity_fac,
              random=~(1|year/Haul) + (1|cohort_fac), data=weight_dat)

# save the model object
saveRDS(weight_mod1, "./output/weight_mod1.rds")
weight_mod1 <- readRDS("./output/weight_mod1.rds")

gam.check(weight_mod1$gam)
plot(weight_mod1$gam)
summary(weight_mod1$gam)
anova(weight_mod1$gam)

# weight_mod2 <- gamm4(sc.weight ~  s(FAR,  k=4) + maturity_fac,
#                      random=~(1|year/Haul) + (1|cohort_fac), data=weight_dat)
# gam.check(weight_mod2$gam)
# plot(weight_mod2$gam)
# summary(weight_mod2$gam)
# anova(weight_mod2$gam)

weight_mod3 <- gamm4(sc.weight ~  s(log(RR, 10),  k=4) + maturity_fac,
                     random=~(1|year/Haul) + (1|cohort_fac), data=weight_dat)

# save the model object
saveRDS(weight_mod3, "./output/weight_mod3.rds")

gam.check(weight_mod3$gam)
plot(weight_mod3$gam)
summary(weight_mod3$gam)
anova(weight_mod3$gam)

# now plot sst and RR effects for all three

# first sst - recruitment

# predict and plot

# recruitment
new_dat <- data.frame(winter.sst2 = seq(min(recr$winter.sst2), max(recr$winter.sst2), length.out = 100)) 

pred <- predict(recr_mod1$gam, newdata = new_dat, se.fit = T)

plot1 <- data.frame(SST = new_dat$winter.sst2,
                   Value = pred$fit, 
                   LCI = pred$fit - 1.96*pred$se.fit,
                   UCI = pred$fit + 1.96*pred$se.fit,
                   variable = "Log age 1 recruits")


# age diversity
new_dat <- data.frame(sst3 = seq(min(shann_dat$sst3), max(shann_dat$sst3), length.out = 100)) 

pred <- predict(shann_mod1$gam, newdata = new_dat, se.fit = T)

plot2 <- data.frame(SST = new_dat$sst3,
                   Value = pred$fit, 
                   LCI = pred$fit - 1.96*pred$se.fit,
                   UCI = pred$fit + 1.96*pred$se.fit,
                   variable = "Shannon age diversity")


# weight

temp <- plot(weight_mod1$gam, resid = T, se = T, pch = 19)

head(temp)


plot3 <- data.frame(SST = temp[[1]]$x,
                    Value = temp[[1]]$fit, 
                    LCI = temp[[1]]$fit - 1.96*temp[[1]]$se,
                    UCI = temp[[1]]$fit + 1.96*temp[[1]]$se,
                    variable = "Spawning weight")

plot_sst <- rbind(plot1, plot2, plot3)

# data frame for points

points1 <- recr %>%
  select(Estimate, winter.sst2) %>%
  rename(Value = Estimate,
         SST = winter.sst2) %>%
  mutate(variable = "Log age 1 recruits",
         size = 0.15,
         alpha = 0.4)

points2 <- shann_dat %>%
  select(Estimate, sst3) %>%
  rename(Value = Estimate,
         SST = sst3) %>%
  mutate(variable = "Shannon age diversity",
         size = 0.15,
         alpha = 0.4)  

# for weight

# using "temp" from plot.gam above!
points3 <- data.frame(Value = temp[[1]]$p.resid,
                      SST = jitter(temp[[1]]$raw, amount = 0.02),
                      variable = "Spawning weight",
                      size = 0.1,
                      alpha = 0.1)

points <- rbind(points1, points2, points3)

sst_plot <- ggplot(plot_sst) +
  aes(x = SST, y = Value) +
  geom_point(data = points, aes(x = SST, y = Value)) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  facet_wrap(~variable, scales = "free", ncol = 1) +
  theme(legend.position = "none")


# now RR plots
# recruitment
new_dat <- data.frame(log_RR = seq(min(recr$log_RR), max(recr$log_RR), length.out = 100)) 

pred <- predict(recr_mod3$gam, newdata = new_dat, se.fit = T)

plot1 <- data.frame(log_RR = new_dat$log_RR,
                    Value = pred$fit, 
                    LCI = pred$fit - 1.96*pred$se.fit,
                    UCI = pred$fit + 1.96*pred$se.fit,
                    variable = "Log age 1 recruits")


# age diversity
new_dat <- data.frame(log_RR = seq(min(shann_dat$log_RR), max(shann_dat$log_RR), length.out = 100)) 

pred <- predict(shann_mod3$gam, newdata = new_dat, se.fit = T)

plot2 <- data.frame(log_RR = new_dat$log_RR,
                    Value = pred$fit, 
                    LCI = pred$fit - 1.96*pred$se.fit,
                    UCI = pred$fit + 1.96*pred$se.fit,
                    variable = "Shannon age diversity")


# weight

temp <- plot(weight_mod3$gam, resid = T, se = T, pch = 19)

head(temp)


plot3 <- data.frame(log_RR = temp[[1]]$x,
                    Value = temp[[1]]$fit, 
                    LCI = temp[[1]]$fit - 1.96*temp[[1]]$se,
                    UCI = temp[[1]]$fit + 1.96*temp[[1]]$se,
                    variable = "Spawning weight")

plot_RR <- rbind(plot1, plot2, plot3)

# data frame for points

points1 <- recr %>%
  select(Estimate, log_RR) %>%
  rename(Value = Estimate) %>%
  mutate(variable = "Log age 1 recruits",
         size = 0.6,
         alpha = 0.4)

points2 <- shann_dat %>%
  select(Estimate, log_RR) %>%
  rename(Value = Estimate) %>%
  mutate(variable = "Shannon age diversity",
         size = 0.6,
         alpha = 0.4)  

# for weight

# using "temp" from plot.gam above!
points3 <- data.frame(Value = temp[[1]]$p.resid,
                      log_RR = jitter(temp[[1]]$raw, amount = 0.02),
                      variable = "Spawning weight",
                      size = 0.5,
                      alpha = 0.1)

points <- rbind(points1, points2, points3)

RR_plot <- ggplot(plot_RR) +
  aes(x = log_RR, y = Value) +
  geom_point(data = points, aes(x = log_RR, y = Value)) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  facet_wrap(~variable, scales = "free", ncol = 1) +
  theme(legend.position = "none") +
  xlab("Log10 Risk Ratio")

# now join

png("./figs/SST_RR_pollock.png", width = 8, height = 8, units = 'in', res = 300)

ggpubr::ggarrange(sst_plot, RR_plot, ncol = 2)

dev.off()

################################
## now projected incidence of critical sst values for each

# recruitment
recr_scaled <- recr %>%
  mutate(scaled_estimate = scale(Estimate))

recr_mod1_sc <- gamm(scaled_estimate ~  s(winter.sst2,  k=5),
             correlation = corAR1(), data=recr_scaled)

new_dat <- data.frame(winter.sst2 = seq(min(recr_scaled$winter.sst2), max(recr_scaled$winter.sst2), length.out = 1000))

pred <- predict(recr_mod1_sc$gam, newdata = new_dat, se.fit = T)

plot <- data.frame(winter.sst2 = new_dat$winter.sst2,
                   estimate = pred$fit, 
                   LCI = pred$fit - 1.96*pred$se.fit,
                   UCI = pred$fit + 1.96*pred$se.fit)


ggplot(plot) +
  aes(x = winter.sst2, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  labs(x = "Winter SST anomaly - 2 yr mean", y = "Scaled log recruitment") 

# get sst values corresponding to -1 and -2 SD

index.1 <- which.min(abs(pred$fit - -1))
index.2 <- which.min(abs(pred$fit - -2))

recr_crit_sst1 <- new_dat$winter.sst2[index.1]
recr_crit_sst2 <- new_dat$winter.sst2[index.2]

###
# now age diversity

shann_scaled <- shann_dat %>%
  mutate(scaled_estimate = scale(Estimate))

shann_mod1_sc <- gamm(scaled_estimate ~  s(sst3,  k=5),
                     correlation = corAR1(), data=shann_scaled)

new_dat <- data.frame(sst3 = seq(min(shann_scaled$sst3), max(shann_scaled$sst3), length.out = 1000))

pred <- predict(shann_mod1_sc$gam, newdata = new_dat, se.fit = T)

plot <- data.frame(sst3 = new_dat$sst3,
                   estimate = pred$fit, 
                   LCI = pred$fit - 1.96*pred$se.fit,
                   UCI = pred$fit + 1.96*pred$se.fit)


ggplot(plot) +
  aes(x = sst3, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  labs(x = "Annual SST anomaly - 3 yr mean", y = "Scaled Shannon diversity") 

# get sst value corresponding to -1 SD

index.1 <- which.min(abs(pred$fit - -1))

shann_crit_sst1 <- new_dat$sst3[index.1]

## finally, weight (# already scaled in original model!)

new_dat <- data.frame(sst = seq(min(weight_dat$sst), max(weight_dat$sst), length.out = 1000),
                      maturity_fac = 4)

pred <- predict(weight_mod1$gam, newdata = new_dat, se.fit = T)

plot <- data.frame(sst = new_dat$sst,
                   estimate = pred$fit, 
                   LCI = pred$fit - 1.96*pred$se.fit,
                   UCI = pred$fit + 1.96*pred$se.fit)


ggplot(plot) +
  aes(x = sst, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  labs(x = "Annual SST anomaly", y = "Scaled weight at age") 

index.1 <- which.min(abs(pred$fit - -1))

weight_crit_sst1 <- new_dat$sst[index.1]
