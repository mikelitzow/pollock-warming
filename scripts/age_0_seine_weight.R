# load and process age-0 pollock data from beach seines

library(tidyverse)
theme_set(theme_bw())
library(mgcv)

dat <- read.csv("./data/LW_juv_pollock.csv")

head(dat)

unique(dat$Bay)

dat <- dat %>% 
  filter(Species == "walleye pollock")

head(dat)

# add sst
# sst <- read.csv("./data/western.goa.jan-jul.sst.csv")
sst <- read.csv("./data/annual.wSST.jan.jun.csv")
# this is average western GOA for January-June (roughly, spawning until settlement)

dat <- left_join(dat, sst)

str(dat)

# change to all lowercase
names(dat) <- str_to_lower(names(dat))

# add julian day
dat$julian <- lubridate::yday(lubridate::parse_date_time(dat$date, orders="mdy"))


# set up some factors for analysis
dat$bay_fac <- as.factor(dat$bay)
dat$sst_fac <- as.factor(dat$sst)
dat$station_fac <- as.factor(dat$station)

# examine length histograms to separate age-0 and age-1
ggplot(dat, aes(length)) +
  geom_histogram(bins = 80, fill = "dark grey", color = "black")

# looks like a break of 100 mm would separate age-0 from age-1

# plot by julian day
ggplot(dat, aes(julian, length)) +
  geom_point() +
  geom_hline(yintercept = 100, lty = 2)

# yes, < 100 mm will segregate age-0

# remove age-1

dat <- dat %>%
  filter(length < 100)

# check
ggplot(dat, aes(length)) +
  geom_histogram(bins = 20, fill = "dark grey", color = "black")

# check bay-year combinations
f <- function(x) sum(!is.na(x))
check <- tapply(dat$length, list(dat$bay, dat$year), f)

check

dat <- dat %>%
  filter(!bay %in% c("Falmouth", "Baralof"))

# drop Falmouth (1 sample)

# a couple more exploratory plots

ggplot(dat, aes(julian, length)) +
  geom_point()

# they grow!

ggplot(dat, aes(weight)) +
  geom_histogram(bins = 20, fill = "grey", color = "black")

ggplot(dat, aes(log(weight))) +
  geom_histogram(bins = 20, fill = "grey", color = "black")

ggplot(dat, aes(log(length), log(weight))) +
  geom_point()

ggplot(dat, aes(julian, log(weight))) +
  geom_point()

ggplot(dat, aes(year, sst)) +
  geom_col()

ggplot(dat, aes(sst, length)) +
  geom_point()

ggplot(dat) +
  aes(x = year, y = length) +
  geom_point() +
  facet_wrap( ~ bay) +
  theme(legend.position = "none")

# ready to analyze!!
mod1 <- mgcv::gam(log(weight) ~ s(julian, k = 3) + sst + bay_fac, data = dat)

summary(mod1)
plot(mod1)

mod2 <- mgcv::gam(log(weight) ~ s(julian, k = 3) + sst, data = dat)

summary(mod2)
plot(mod2)

MuMIn::AICc(mod1, mod2) #mod1 (including bay_fac fixed effect) is best

mod2 <- mgcv::gam(log(weight) ~ s(julian, k = 3) + sst, data = dat)

summary(mod2)
plot(mod2)

MuMIn::AICc(mod1, mod2)


# ok, try a mixed-effects model 

# start simply with a bay random intercept
mod3 <- gamm(length ~ s(julian, k = 3) + sst, 
              random = list(station_fac=~1), data = dat)

summary(mod3$gam)
plot(mod3$gam)

# and plot
new.dat <- data.frame(julian = mean(dat$julian),
                      sst = seq(min(dat$sst), max(dat$sst), length.out = 100))

predict <- predict(mod3$gam, newdata = new.dat, se = T)

plot.dat <- data.frame(sst = new.dat$sst,
                       weight = predict$fit, 
                       LCI = predict$fit - 1.96*predict$se.fit,
                       UCI = predict$fit + 1.96*predict$se.fit)

ggplot(plot.dat, aes(sst, weight)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), fill = "grey", alpha = 0.3) +
  labs(x = "SST (°C)",
       y = "Weight (g)")
