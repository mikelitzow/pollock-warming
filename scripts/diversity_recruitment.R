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


# shannon age diversity first

acoustic_shann_dat <- d1 %>%
  filter(variable == "Spawning stock age diversity") %>%
  select(-LCI, -UCI, -variable) %>%
  rename(diversity = Estimate,
         year = Year) %>%
  na.omit() # dropping missing years to allow AR(1) fit

# best covariate is sst for 1-3 years before sampling
# for 2020 Shannon this is 3yr mean sst value of 2.168424

# load sst
sst <- read.csv("./data/western.goa.sst.1950.2022.csv")

# process
sst <- sst %>%
  filter(name == "Annual") %>%
  rename(sst = value) %>%
  select(-name) %>%
  mutate(sst.3 = zoo::rollmean(sst, 3, align = "right", fill = NA),
         lag.sst3 = lag(sst.3))

# join to acoustic diversity data
acoustic_shann_dat <- left_join(acoustic_shann_dat, sst)

# and load comparison data set with 2012 cohort adjusted to mean size
adj_acoustic_shann_dat <- read.csv("./output/shannon_diversity_adj_2012.csv")

adj_acoustic_shann_dat <- left_join(adj_acoustic_shann_dat, sst)

# now fit models

shann_mod1 <- gamm(diversity ~  s(lag.sst3,  k=4), # 2020 diversity compared with 2017-2019 sst
             correlation = corAR1(), data = acoustic_shann_dat)

gam.check(shann_mod1$gam)
plot(shann_mod1$gam, residuals = T, pch = 19)
summary(shann_mod1$gam)
anova(shann_mod1$gam)

# and adjusted version (2012 cohort held at mean size)
adj_shann_mod1 <- gamm(shannon ~  s(lag.sst3,  k=4), # 2020 diversity compared with 2017-2019 sst
                   correlation = corAR1(), data = adj_acoustic_shann_dat)

gam.check(adj_shann_mod1$gam)
plot(adj_shann_mod1$gam, residuals = T, pch = 19)
summary(adj_shann_mod1$gam)
anova(adj_shann_mod1$gam)

## observer data

# load data
observer_shannon_dat <- read.csv("./data/observer.age.diversity.csv") %>%
  rename(year = YEAR,
         diversity = shannon) %>%
  left_join(., sst)


shann_mod2 <- gamm(diversity ~  s(lag.sst3,  k=4), # 2020 diversity compared with 2017-2019 sst
                   correlation = corAR1(), data = observer_shannon_dat)

gam.check(shann_mod2$gam)
plot(shann_mod2$gam, residuals = T, pch = 19)
summary(shann_mod2$gam)
anova(shann_mod2$gam)

# refit as linear model
library(nlme)
library(lmerTest)
linear_mod1 <- gls(diversity ~ lag.sst3, correlation = corAR1(), 
                    data = observer_shannon_dat)

summary(linear_mod1)

rcompanion::nagelkerke(linear_mod1)
# combine and plot

acoust_ts <- acoustic_shann_dat %>%
  select(year, diversity) %>%
  rename("Acoustic trawl" = diversity)

obs_ts <- observer_shannon_dat %>%
  select(year, diversity) %>%
  rename("Commercial catch" = diversity)

plot_ts <- data.frame(year = 1986:2021) %>%
  left_join(., acoust_ts) %>%
  left_join(., obs_ts) %>%
  pivot_longer(cols = -year)

diversity_ts <- ggplot(plot_ts, aes(year, value, color = name)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = cb[c(2,6)]) +
  theme(legend.title = element_blank(),
        legend.position = c(0.25, 0.2),
        axis.title.x = element_blank()) +
  labs(y = "Shannon diversity",
       title = "Age stucture diversity") +
  scale_x_continuous(breaks = seq(1990, 2020, 5))

# now predicted
new.dat.acoustic <- data.frame(lag.sst3 = seq(min(acoustic_shann_dat$lag.sst3),
                                              max(acoustic_shann_dat$lag.sst3),
                                              length.out = 100))

pred.acoustic <- predict(shann_mod1$gam, se.fit = T, newdata = new.dat.acoustic)

pred_acoustic_dat <- data.frame(sst = new.dat.acoustic$lag.sst3,
                                effect = pred.acoustic$fit,
                                LCI = pred.acoustic$fit - 1.96*pred.acoustic$se.fit,
                                UCI = pred.acoustic$fit + 1.96*pred.acoustic$se.fit)

acoustic_plot <- ggplot(pred_acoustic_dat, aes(sst, effect)) +
  geom_line(color = cb[2]) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = cb[2], alpha = 0.2) +
  geom_point(data = acoustic_shann_dat, aes(lag.sst3, diversity), color = cb[2]) + 
  labs(y = "Shannon diversity",
       x = "SST (°C)",
       title = "Acoustic trawl") 

acoustic_plot

######
new.dat.obs <- data.frame(lag.sst3 = seq(min(observer_shannon_dat$lag.sst3),
                                              max(observer_shannon_dat$lag.sst3),
                                              length.out = 100))

pred.observer <- predict(shann_mod2$gam, se.fit = T, newdata = new.dat.obs)

pred_obs_dat <- data.frame(sst = new.dat.obs$lag.sst3,
                           effect = pred.observer$fit,
                           LCI = pred.observer$fit - 1.96*pred.observer$se.fit,
                           UCI = pred.observer$fit + 1.96*pred.observer$se.fit)

obs_shann_plot <- ggplot(pred_obs_dat, aes(sst, effect)) +
  geom_line(color = cb[6]) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = cb[6], alpha = 0.2) +
  geom_point(data = observer_shannon_dat, aes(lag.sst3, diversity), color = cb[6]) + 
  labs(y = "Shannon diversity",
       x = "SST (°C)",
       title = "Commercial catch") 

obs_shann_plot


##
png("./figs/combined_shannon_sst_plot.png", width = 4, height = 8, units = 'in', res = 300)

ggpubr::ggarrange(diversity_ts,
                  acoustic_plot,
                  obs_shann_plot,
                  ncol = 1,
                  labels = "auto"
)

dev.off()

## recruits----------------------------------

# load data

d1 <- read.csv("./data/pollock_recruitment_2022_assessment.csv")

recr <- d1 %>%
  rename(recr = R0) %>%
  mutate(recr = log(recr)) %>%
  filter(year <= 2020) # dropping most recent year b/c of poor data support

# best SST covariate is winter SST year of and year before sampling
# load sst
# sst <- read.csv("./data/western.goa.sst.1950.2022.csv")
# sst <- read.csv("./data/annual.wSST.jan.jun.csv", row.names = 1)
sst <- read.csv("./data/annual.wSST.winter.csv", row.names = 1)
# # process
# sst <- sst %>%
#   filter(name == "Annual") %>%
#   rename(sst = value) %>%
#   select(-name) %>%
#   mutate(sst.2 = zoo::rollmean(sst, 2, align = "right", fill = NA),
#          sst.3 = zoo::rollmean(sst, 3, align = "right", fill = NA))

sst <- sst %>%
  mutate(sst.2 = zoo::rollmean(sst, 2, align = "right", fill = NA),
         sst.3 = zoo::rollmean(sst, 3, align = "right", fill = NA))


recr <- left_join(recr, sst)

ggplot(recr, aes(year, recr)) +
  geom_point() +
  geom_line()

ggplot(recr, aes(sst, recr)) +
  geom_text(aes(label = year))

# now fit models

recr_mod1 <- gamm(recr ~  s(sst), 
                   correlation = corAR1(), data=recr)

gam.check(recr_mod1$gam)
plot(recr_mod1$gam, residuals = T, pch = 19)
summary(recr_mod1$gam)
anova(recr_mod1$gam)

# plot

recr_ts <- ggplot(recr, aes(year, recr)) +
  geom_line() +
  geom_point() + 
  theme(axis.title.x = element_blank()) +
  ylab("Log(recruits)") 
  

new.dat <- data.frame(sst = seq(min(recr$sst),
                                  max(recr$sst), 
                                  length.out = 100))

pred.recr <- predict(recr_mod1, newdata = new.dat, se.fit = T)

plot_pred <- data.frame(sst = new.dat$sst,
                        estimate = pred.recr$fit,
                        LCI = pred.recr$fit - 1.96*pred.recr$se.fit,
                        UCI = pred.recr$fit + 1.96*pred.recr$se.fit)


recr_sst <- ggplot(plot_pred, aes(sst, estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2) +
  geom_text(data = recr, aes(sst, recr, label = year), size = 3) +
  labs(y = "Log(recruits)",
       x = "SST (°C)") +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(2, 10, 1), minor_breaks = NULL)

# and plot

png("./figs/recr_sst.png", width = 5, height = 7, units = "in", res = 300) 

ggpubr::ggarrange(recr_ts, recr_sst, ncol = 1, labels = "auto")

dev.off()

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
