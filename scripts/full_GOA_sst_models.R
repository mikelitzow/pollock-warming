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

# refit best models using western GOA SST

coMnoa <- gamm4(sc.weight ~  s(prevyr_annual.SST,  k=4) + maturity_table_3,
                random=~(1|year/Haul) + (1|cohort), data=dat_lag[which(dat_lag$sex.code==1),])
gam.check(coMnoa$gam)
plot(coMnoa$gam)
summary(coMnoa$gam)
anova(coMnoa$gam)

coFnoa <- gamm4(sc.weight ~  s(prevyr_annual.SST, k=4) + maturity_table_3,
                random=~(1|year/Haul) + (1|cohort), data=dat_lag[which(dat_lag$sex.code==2),])
gam.check(coFnoa$gam)
plot(coFnoa$gam)
summary(coFnoa$gam)
anova(coFnoa$gam)

# plot predictions from each model on the same panel
new_dat <- data.frame(prevyr_annual.SST = seq(min(dat_lag$prevyr_annual.SST), max(dat_lag$prevyr_annual.SST), length.out = 100),
                      maturity_table_3 = 4) # predicting for spawning weight

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

ggsave("./figs/weight_age_goa_SST_anomaly_by_sex.png", width = 6, height = 4, units = 'in')

