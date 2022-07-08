# load and process age-0 pollock data from beach seines

library(tidyverse)
theme_set(theme_bw())
library(mgcv)
library(gamm4)

dat <- read.csv("./data/cod.pollock.lengths.2006.2021.csv", row.names = 1)

dat <- dat %>% 
  filter(species == "poll")

head(dat)

# add sst anomalies
sst <- read.csv("./data/regional_north_pacific_ersst_anomaly_time_series.csv")

sst <- sst %>%
  filter(region == "Gulf_of_Alaska") %>%
  select(year, winter.anomaly.unsmoothed) %>%
  rename(sst = winter.anomaly.unsmoothed)


dat <- left_join(dat, sst)

str(dat)

# set up some factors for analysis
dat$bay_fac <- as.factor(dat$bay)
dat$day_site <- as.factor(paste(dat$julian, dat$site, sep = "_"))
dat$year_fac <- as.factor(dat$year)

# examine length histograms to separate age-0 and age-1
ggplot(dat, aes(length)) +
  geom_histogram(bins = 80, fill = "dark grey", color = "black")

# looks like a break of ~125 mm or so would separate age-0 from age-1

# plot by julian day
ggplot(dat, aes(julian, length)) +
  geom_point() +
  geom_hline(yintercept = 120, lty = 2)

# yes, < 120 mm will segregate age-0

# remove age-1

dat <- dat %>%
  filter(length < 120)

# check
ggplot(dat, aes(length)) +
  geom_histogram(bins = 80, fill = "dark grey", color = "black")

# check bay-year combinations
f <- function(x) sum(!is.na(x))
check <- tapply(dat$length, list(dat$bay, dat$year), f)

check

# to aid model fitting, drop years with 1-5 lengths
dat <- dat %>%
  filter(! year %in% c(2009, 2011, 2015))

# a couple more exploratory plots

ggplot(dat, aes(julian, length)) +
  geom_point()


ggplot(dat, aes(sst, length)) +
  geom_point()

ggplot(dat) +
  aes(x = year, y = length, color = site) +
  geom_point() +
  facet_wrap( ~ bay) +
  theme(legend.position = "none")

# ready to analyze!!
mod1 <- gamm4(length ~  s(sst,  k=4) + s(julian, k=4),
                random=~(1|year/day_site), data=dat)

gam.check(mod1$gam)
plot(mod1$gam)
summary(mod1$gam)
anova(mod1$gam)


mod2 <- gamm4(length ~  s(sst,  k=4) + s(julian, k=4) + bay_fac,
              random=~(1|year/day_site), data=dat)

gam.check(mod2$gam)
plot(mod2$gam)
summary(mod2$gam)
anova(mod2$gam)


AIC <- AIC(mod1$mer, mod2$mer) # mod 5 appears best
AIC

AIC_table <- data.frame(response = "age 0 length",
                        model = c("s(sst,  k=4) + s(julian, k=4), random=~(1|year/day_site)",
                                  "s(sst,  k=4) + s(julian, k=4) + bay_fac, random=~(1|year/day_site)"),
                        AIC = AIC$AIC,
                        delta_AIC = (AIC$AIC - min(AIC$AIC)))

AIC_table <- AIC_table %>%
  arrange(delta_AIC)

# save
write.csv(AIC_table, "age_length_AIC_table.csv", row.names = F)

# predict and plot

new_dat <- data.frame(sst = seq(min(dat$sst), max(dat$sst), length.out = 100),
                      julian = mean(dat$julian),
                      bay_fac = "Balboa") # predicting for spawning weight

pred <- predict(mod2$gam, newdata = new_dat, se.fit = T)

plot <- data.frame(sst = new_dat$sst,
                          estimate = pred$fit, 
                          LCI = pred$fit - 1.96*pred$se.fit,
                          UCI = pred$fit + 1.96*pred$se.fit)


ggplot(plot) +
  aes(x = sst, y = estimate) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  geom_line(size = 1, color = cb[7]) +
  labs(x = "Previous winter SST anomaly", y = "Length (mm)") 

ggsave("./figs/age_0_length_goa_SST_anomaly.png", width = 6, height = 4, units = 'in')
