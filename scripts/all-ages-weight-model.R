#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#All ages weight models

#Krista, Jan 11, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: does age matter?
# going to use new SST from Mike
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#data is loaded in load_explor_size-at-age.R
library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(gamm4)
library(MuMIn)

library(rstan)
library(brms)
library(bayesplot)
source("./scripts/stan_utils.R")

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#look at weight by age and sst

ggplot(all.dat, aes(annual.wSST, sc.weight, colour=as.factor(sex.code))) + geom_point() +geom_smooth()

ggplot(all.dat, aes(annual.wSST, sc.weight, colour=as.factor(Age))) + geom_point() +geom_smooth(method="lm")

all.dat$sex.code <- as.factor(all.dat$sex.code)
all.dat$age.factor <- as.factor(all.dat$Age)
all.dat$maturity_table_3 <- as.factor(all.dat$maturity_table_3)

#big overall model----

#BELOW doesn't make a lot of sense because of timing of survey, skip to the lagged models farther down

# bigmod1 <- gamm4(sc.weight ~ s(annual.wSST, k=4) + s(annual.wSST, by=age.factor, k=4) + #s(annual.wSST, by=sex.code, k=4) +
#                     maturity_table_3,
#                   random=~(1|year/Haul), data=all.dat)
# gam.check(bigmod1$gam)
# plot(bigmod1$gam)
# summary(bigmod1$gam)
# 
# bigmod2 <- gamm4(sc.weight ~  s(annual.wSST, by=age.factor, k=4), #+ #s(annual.wSST, by=sex.code, k=4),
#                  random=~(1|year/Haul), data=all.dat)
# gam.check(bigmod2$gam)
# plot(bigmod2$gam)
# summary(bigmod2$gam)
# anova(bigmod2$gam)
# 
# bigmod3 <- gamm4(sc.weight ~  s(annual.wSST, by=age.factor, k=4) + maturity_table_3,
#                  random=~(1|year/Haul), data=all.dat[which(all.dat$sex.code==1),])
# gam.check(bigmod3$gam)
# plot(bigmod3$gam)
# summary(bigmod3$gam)
# anova(bigmod3$gam)
# 
# bigmod4 <- gamm4(sc.weight ~  s(annual.wSST, by=age.factor, k=4) + maturity_table_3,
#                  random=~(1|year/Haul), data=all.dat[which(all.dat$sex.code==2),])
# gam.check(bigmod4$gam)
# plot(bigmod4$gam)
# summary(bigmod4$gam)
# 
# anova(bigmod4$gam)
# 
# bigmodS1 <- gamm4(sc.weight ~ s(annual.wSST, k=4) + age.factor +
#                    maturity_table_3,
#                  random=~(1|year/Haul), data=all.dat[which(all.dat$sex.code==1),])
# gam.check(bigmodS1$gam)
# plot(bigmodS1$gam)
# summary(bigmodS1$gam)
# 
# bigmodS2 <- gamm4(sc.weight ~ s(annual.wSST, k=4) + age.factor +
#                     maturity_table_3,
#                   random=~(1|year/Haul), data=all.dat[which(all.dat$sex.code==2),])
# gam.check(bigmodS2$gam)
# plot(bigmodS2$gam)
# summary(bigmodS2$gam)
# 
# AIC(bigmodS1$mer, bigmod3$mer)
# AIC(bigmodS2$mer, bigmod4$mer)


#lag annual sst------
#reload sst data
d1 <- read.csv("./data/western.goa.sst.csv")
sst_lagged <- d1

sst_lagged$prevyr_annual.wSST <- NA
k<-1
for(k in 1:length(sst_lagged$year)){
  if(k>1){
    sst_lagged$prevyr_annual.wSST[k] <- sst_lagged$annual.wSST[k-1] 
  }
} #works now join

sstjoin <- sst_lagged[,c(1,5)]

dat_lag <- left_join(all.dat, sstjoin)

#big models lagged 1 yr-------

bigmod5 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=dat_lag[which(dat_lag$sex.code==1),])
gam.check(bigmod5$gam)
plot(bigmod5$gam)
summary(bigmod5$gam)
anova(bigmod5$gam)

bigmod6 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=dat_lag[which(dat_lag$sex.code==2),])
gam.check(bigmod6$gam)
plot(bigmod6$gam)
summary(bigmod6$gam)
anova(bigmod6$gam)


#what about maturity?-------

bigmod7 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=maturity_table_3, k=4),
                 random=~(1|year/Haul), data=dat_lag[which(dat_lag$sex.code==2),])
gam.check(bigmod7$gam)
plot(bigmod7$gam)
summary(bigmod7$gam)
anova(bigmod7$gam)


bigmod8 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=maturity_table_3, k=4) + maturity_table_3*age.factor,
                 random=~(1|year/Haul), data=dat_lag[which(dat_lag$sex.code==2),])
gam.check(bigmod8$gam)
plot(bigmod8$gam)
summary(bigmod8$gam)
anova(bigmod8$gam)


bigmod9 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=maturity_table_3, k=4) + maturity_table_3*age.factor,
                 random=~(1|year/Haul), data=dat_lag[which(dat_lag$sex.code==1),])
gam.check(bigmod9$gam)
plot(bigmod9$gam)
summary(bigmod9$gam)
anova(bigmod9$gam)

ggplot(dat_lag[which(dat_lag$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + geom_smooth(method="lm") +
  facet_wrap(~Age, scales="free")

ggplot(dat_lag[which(dat_lag$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + geom_smooth(method="lm") +
  facet_wrap(~Age, scales="free")

#add cohort effects=================================================================

dat_lag$cohort <- dat_lag$year - dat_lag$Age
dat_lag$cohort <- as.factor(dat_lag$cohort)

coM <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul) + (1|cohort), data=dat_lag[which(dat_lag$sex.code==1),])
gam.check(coM$gam)
plot(coM$gam)
summary(coM$gam)
anova(coM$gam)

coF <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul) + (1|cohort), data=dat_lag[which(dat_lag$sex.code==2),])
gam.check(coF$gam)
plot(coF$gam)
summary(coF$gam)
anova(coF$gam)


#without age interaction

coMnoa <- gamm4(sc.weight ~  s(prevyr_annual.wSST,  k=4) + maturity_table_3,
             random=~(1|year/Haul) + (1|cohort), data=dat_lag[which(dat_lag$sex.code==1),])
gam.check(coMnoa$gam)
plot(coMnoa$gam)
summary(coMnoa$gam)
anova(coMnoa$gam)

coFnoa <- gamm4(sc.weight ~  s(prevyr_annual.wSST, k=4) + maturity_table_3,
             random=~(1|year/Haul) + (1|cohort), data=dat_lag[which(dat_lag$sex.code==2),])
gam.check(coFnoa$gam)
plot(coFnoa$gam)
summary(coFnoa$gam)
anova(coFnoa$gam)

AIC(coMnoa$mer, coM$mer)
AIC(coFnoa$mer, coF$mer)

# models without age effect are best

# models suggest very different shapes to temperature effects for each sex

# plot predictions from each model on the same panel
new_dat <- data.frame(prevyr_annual.wSST = seq(min(dat_lag$prevyr_annual.wSST), max(dat_lag$prevyr_annual.wSST), length.out = 100),
                         maturity_table_3 = 4) # predicting for spawning weight

pred_female <- predict(coFnoa$gam, newdata = new_dat, se.fit = T)
plot_female <- data.frame(SST = new_dat$prevyr_annual.wSST,
                          estimate = pred_female$fit, 
                          LCI = pred_female$fit - 1.96*pred_female$se.fit,
                          UCI = pred_female$fit + 1.96*pred_female$se.fit,
                          sex = "Female")

pred_male <- predict(coMnoa$gam, newdata = new_dat, se.fit = T)
plot_male <- data.frame(SST = new_dat$prevyr_annual.wSST,
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
  labs(x = "Previous year SST (°C)", y = "Log weight anomaly") 

ggsave("./figs/weight_age_sst_by_sex_gamm.png", width = 6, height = 4, units = 'in')

# try a combined model - separate smooths to each sex
coBnoa <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by = sex.code, k=4) + maturity_table_3:sex.code,
                random=~(1|year/Haul) + (1|cohort), data=dat_lag) # model does not fit

# try with random effect maturity:sex.code
coBnoa <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by = sex.code, k=4),
                random=~(1|year/Haul) + (1|cohort) + (1|sex.code/maturity_table_3), data=dat_lag) # no dice

# save the cohort version of the data
write.csv(dat_lag, "./data/cohort_weight_age.csv", row.names = F)

## brms models ---------------
# reload for brms 
dat_lag <- read.csv("./data/cohort_weight_age.csv")

weight1_formula <-  bf(sc.weight ~  s(prevyr_annual.wSST, k=4) + maturity_table_3 +
                         (1|year/Haul) + (1|cohort))


## Show default priors
get_prior(weight1_formula, dat_lag)


## fit models --------------------------------------
weight_sst_female <- brm(weight1_formula,
                         data = dplyr::filter(dat_lag, sex.code==2),
                         cores = 4, chains = 4, iter = 4000,
                         save_pars = save_pars(all = TRUE),
                         control = list(adapt_delta = 0.999, max_treedepth = 16))

saveRDS(weight_sst_female, file = "./output/brm_weight_sst_female.rds")

weight_sst_female <- readRDS("./output/brm_weight_sst_female.rds")

weight_sst_male <- brm(weight1_formula,
                       data = dplyr::filter(dat_lag, sex.code==1),
                       seed = 99,
                       cores = 4, chains = 4, iter = 12000,
                       save_pars = save_pars(all = TRUE),
                       control = list(adapt_delta = 0.9999999, max_treedepth = 16))

saveRDS(weight_sst_male, file = "./output/brm_weight_sst_male.rds")

weight_sst_male <- readRDS("./output/brm_weight_sst_male.rds")

neff_lowest(weight_sst_male$fit)
check_hmc_diagnostics(weight_sst_male$fit)
#######################-------------
# plot both
## 95% CI
ce1s_1 <- conditional_effects(weight_sst_female, effect = "prevyr_annual.wSST", re_formula = NA,
                              probs = c(0.025, 0.975))

dat_ce <- ce1s_1$prevyr_annual.wSST
dat_ce[["upper_95"]] <- dat_ce[["upper__"]]
dat_ce[["lower_95"]] <- dat_ce[["lower__"]]

dat_ce$sex <- "female"

# now male
ce1s_1 <- conditional_effects(weight_sst_male, effect = "prevyr_annual.wSST", re_formula = NA,
                              probs = c(0.025, 0.975))

dat_ce2 <- ce1s_1$prevyr_annual.wSST
dat_ce2[["upper_95"]] <- dat_ce[["upper__"]]
dat_ce2[["lower_95"]] <- dat_ce[["lower__"]]

dat_ce2$sex <- "male"

plot_both <- rbind(dat_ce, dat_ce2)



ggplot(plot_both) +
  aes(x = effect1__, y = estimate__, fill = sex, color = sex) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, lty = 0) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = cb[c(2,6)]) +
  scale_fill_manual(values = cb[c(2,6)]) +
  labs(x = "Previous year SST (°C)", y = "Log weight anomaly") 

ggsave("./figs/weight_age_sst_by_sex_brms.png", width = 6, height = 4, units = 'in')
