# refit temperature-driven models
# with anomalies wrt 1854-1949 for the entire GOA
# for use with climate model projections


library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(gamm4)
library(MuMIn)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dat_lag <- read.csv("./data/cohort_weight_age.csv")

# make sure we're coding factors
dat_lag <- dat_lag %>%
  mutate(maturity_fac = as.factor(maturity_table_3),
         sex_fac = as.factor(sex.code),
         age_fac = as.factor(Age),
         cohort_fac = as.factor(cohort))

# replace western GOA with GOA-wide sst anomalies
dat_lag <- dat_lag %>%
  select(-annual.wSST, -prevyr_annual.wSST)

d1 <- read.csv("./data/regional_north_pacific_ersst_anomaly_time_series.csv")

sst_lagged <- d1 %>%
  dplyr::filter(region == "Gulf_of_Alaska") %>%
  dplyr::select(year, annual.anomaly.unsmoothed)

sst_lagged$prevyr_annual.SST <- NA

for(k in 1:length(sst_lagged$year)){
  if(k>1){
    sst_lagged$prevyr_annual.SST[k] <- sst_lagged$annual.anomaly.unsmoothed[k-1] 
  }
} #works now join

sstjoin <- sst_lagged[,c(1,3)]

dat_lag <- left_join(dat_lag, sstjoin)

# fit models

# first cohort models with age effect for males and females

coM <- gamm4(sc.weight ~  s(prevyr_annual.SST, by=age_fac, k=4) + maturity_fac,
             random=~(1|year/Haul) + (1|cohort_fac), data=dat_lag[which(dat_lag$sex_fac==1),])
gam.check(coM$gam)
plot(coM$gam)
summary(coM$gam)
anova(coM$gam)

coF <- gamm4(sc.weight ~  s(prevyr_annual.SST, by=age_fac, k=4) + maturity_fac,
             random=~(1|year/Haul) + (1|cohort_fac), data=dat_lag[which(dat_lag$sex_fac==2),])
gam.check(coF$gam)
plot(coF$gam)
summary(coF$gam)
anova(coF$gam)

# models without age effect

coMnoa <- gamm4(sc.weight ~  s(prevyr_annual.SST,  k=4) + maturity_fac,
                random=~(1|year/Haul) + (1|cohort_fac), data=dat_lag[which(dat_lag$sex_fac==1),])
gam.check(coMnoa$gam)
plot(coMnoa$gam)
summary(coMnoa$gam)
anova(coMnoa$gam)

coFnoa <- gamm4(sc.weight ~  s(prevyr_annual.SST, k=4) + maturity_fac,
                random=~(1|year/Haul) + (1|cohort_fac), data=dat_lag[which(dat_lag$sex_fac==2),])
gam.check(coFnoa$gam)
plot(coFnoa$gam)
summary(coFnoa$gam)
anova(coFnoa$gam)

AIC1 <- AIC(coM$mer, coMnoa$mer)
AIC2 <- AIC(coF$mer, coFnoa$mer)


AIC1_table <- data.frame(response = c("male_scaled_weight",
                                     "male_scaled_weight"),
                        model = c("s(prevyr_annual.SST, by=age_fac, k=4) + maturity_fac, random=~(1|year/Haul) + (1|cohort_fac)",
                                  "s(prevyr_annual.SST,  k=4) + maturity_fac, random=~(1|year/Haul) + (1|cohort_fac)"),
                        AIC = AIC1$AIC,
                        delta_AIC = (AIC1$AIC - min(AIC1$AIC)))


AIC2_table <- data.frame(response = c("female_scaled_weight",
                                      "female_scaled_weight"),
                         model = c("s(prevyr_annual.SST, by=age_fac, k=4) + maturity_fac, random=~(1|year/Haul) + (1|cohort_fac)",
                                   "s(prevyr_annual.SST,  k=4) + maturity_fac, random=~(1|year/Haul) + (1|cohort_fac)"),
                         AIC = AIC2$AIC,
                         delta_AIC = (AIC2$AIC - min(AIC2$AIC)))


AIC_table <- rbind(AIC1_table, AIC2_table) # model *without* age is best in both cases


# plot predictions for age-specific male model
new_dat <- data.frame(prevyr_annual.SST = seq(min(dat_lag$prevyr_annual.SST), max(dat_lag$prevyr_annual.SST), length.out = 100),
                      `age_fac` = rep(4:10, each = 100),
                      maturity_fac = 4) # predicting for spawning weight

pred <- predict(coM$gam, newdata = new_dat, se.fit = T)

plot_male <- data.frame(SST = new_dat$prevyr_annual.SST,
                        estimate = pred$fit, 
                        LCI = pred$fit - 1.96*pred$se.fit,
                        UCI = pred$fit + 1.96*pred$se.fit,
                        age = new_dat$age_fac, 
                        sex = "male")


ggplot(plot_male) +
  aes(x = SST, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~age) +
  labs(x = "Previous year SST anomaly", y = "Log weight anomaly") 

pred <- predict(coF$gam, newdata = new_dat, se.fit = T)

plot_female <- data.frame(SST = new_dat$prevyr_annual.SST,
                        estimate = pred$fit, 
                        LCI = pred$fit - 1.96*pred$se.fit,
                        UCI = pred$fit + 1.96*pred$se.fit,
                        age = new_dat$age_fac, 
                        sex = "female")


ggplot(plot_female) +
  aes(x = SST, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~age) +
  labs(x = "Previous year SST anomaly", y = "Log weight anomaly") 

# plot both sex / age models together
plot_both_age <- rbind(plot_male, plot_female)

ggplot(plot_both_age) +
  aes(x = SST, y = estimate, fill = sex, color = sex) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~age) +
  scale_color_manual(values = cb[c(2,6)]) +
  scale_fill_manual(values = cb[c(2,6)]) +
  labs(x = "Previous year SST anomaly", y = "Log weight anomaly") 

ggsave("./figs/weight_age_goa_SST_anomaly_by_sex.png", width = 6, height = 4, units = 'in')

# little support for separate models for each sex
# refit with sexes combined

mod1 <- gamm4(sc.weight ~  s(prevyr_annual.SST, by=age_fac, k=4) + maturity_fac,
             random=~(1|year/Haul) + (1|cohort_fac), data=dat_lag)
gam.check(mod1$gam)
plot(mod1$gam)
summary(mod1$gam)
anova(mod1$gam)

# model without age effect

mod2 <- gamm4(sc.weight ~  s(prevyr_annual.SST,  k=4) + maturity_fac,
                random=~(1|year/Haul) + (1|cohort_fac), data=dat_lag)
gam.check(mod2$gam)
plot(mod2$gam)
summary(mod2$gam)
anova(mod2$gam)

AIC <- AIC(mod1$mer, mod2$mer)

AIC_table <- data.frame(response = "scaled_weight",
                         model = c("s(prevyr_annual.SST, by=age_fac, k=4) + maturity_fac, random=~(1|year/Haul) + (1|cohort_fac)",
                                   "s(prevyr_annual.SST,  k=4) + maturity_fac, random=~(1|year/Haul) + (1|cohort_fac)"),
                         AIC = AIC$AIC,
                         delta_AIC = (AIC$AIC - min(AIC$AIC)))
write.csv(AIC_table, "./output/acoustic_trawl_combined_sexes_AIC_table.csv", row.names = F)

anova(mod1$gam, mod2$gam)

# and plot best model

new_dat <- data.frame(prevyr_annual.SST = seq(min(dat_lag$prevyr_annual.SST), max(dat_lag$prevyr_annual.SST), length.out = 100),
                      `age_fac` = rep(4:10, each = 100),
                      maturity_fac = 4) # predicting for spawning weight

pred <- predict(mod1$gam, newdata = new_dat, se.fit = T)

plot_age <- data.frame(SST = new_dat$prevyr_annual.SST,
                        estimate = pred$fit, 
                        LCI = pred$fit - 1.96*pred$se.fit,
                        UCI = pred$fit + 1.96*pred$se.fit,
                        age = as.factor(new_dat$age_fac))


pred <- predict(mod2$gam, newdata = new_dat, se.fit = T)

plot_all <- data.frame(SST = new_dat$prevyr_annual.SST,
                       estimate = pred$fit, 
                       LCI = pred$fit - 1.96*pred$se.fit,
                       UCI = pred$fit + 1.96*pred$se.fit,
                       age = "All ages")


plot_both <- rbind(plot_age, plot_all)

# and order
plot_order <- data.frame(age = c(4:10, "All ages"),
                         order = 1:8)

plot_both <- left_join(plot_both, plot_order)

plot_both$age <- reorder(plot_both$age, plot_both$order)


ggplot(plot_both) +
  aes(x = SST, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~age) +
  labs(x = "Previous year SST anomaly", y = "Log weight anomaly") 

ggsave("./figs/weight_age_goa_SST_anomaly_combined_sexes.png", width = 8, height = 6, units = 'in')

##
##
# plot predictions from models without age effect on the same panel
new_dat <- data.frame(prevyr_annual.SST = seq(min(dat_lag$prevyr_annual.SST), max(dat_lag$prevyr_annual.SST), length.out = 100),
                      maturity_fac = 4) # predicting for spawning weight

pred_female <- predict(coFnoa$gam, newdata = new_dat, se.fit = T)
plot_female <- data.frame(SST = new_dat$prevyr_annual.SST,
                          estimate = pred_female$fit, 
                          LCI = pred_female$fit - 1.96*pred_female$se.fit,
                          UCI = pred_female$fit + 1.96*pred_female$se.fit,
                          sex = "Female")

pred_male <- predict(coMnoa$gam, newdata = new_dat, se.fit = T)
plot_male <- data.frame(SST = new_dat$prevyr_annual.SST,
                        estimate = pred_male$fit, 
                        LCI = pred_male$fit - 1.96*pred_male$se.fit,
                        UCI = pred_male$fit + 1.96*pred_male$se.fit,
                        sex = "Male")


plot_both <- rbind(plot_female, plot_male)


ggplot(plot_both) +
  aes(x = SST, y = estimate, fill = sex, color = sex) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = cb[c(2,6)]) +
  scale_fill_manual(values = cb[c(2,6)]) +
  labs(x = "Previous year SST anomaly", y = "Log weight anomaly") 



