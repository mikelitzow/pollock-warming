# three data sources: beach seines, acoustic trawl, observer data 

library(tidyverse)
theme_set(theme_bw())
library(mgcv)


## age-0 pollock data from beach seines--------------

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
dat$year_fac <- as.factor(dat$year)

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
# try a mixed-effects model 

# start simply with a station random intercept
mod1 <- gamm4(weight ~ s(julian, k = 5) + sst, 
             random=~(1|station_fac), data = dat)

summary(mod1$gam)
plot(mod1$gam)


# random station nested within bay effects
mod2 <- gamm4(weight ~ s(julian, k = 5) + sst, 
              random=~(1|bay_fac/station_fac), data = dat)

summary(mod2$gam)
plot(mod2$gam)


MuMIn::AICc(mod1$mer, mod2$mer) # mod 1 (random station effects) best

# save for AIC table
seine_AIC <- data.frame(data = "Age-0 weight",
                        fixed_effects = "s(julian, k = 5) + sst",
                        random_effects = c("station","bay/station"),
                        AICc = MuMIn::AICc(mod1$mer, mod2$mer)$AICc,
                        delta_AICc =  MuMIn::AICc(mod1$mer, mod2$mer)$AICc- min(MuMIn::AICc(mod1$mer, mod2$mer)$AICc))


# and plot
new.dat <- data.frame(julian = mean(dat$julian),
                      sst = seq(min(dat$sst), max(dat$sst), length.out = 100))

predict <- predict(mod1$gam, newdata = new.dat, se = T)

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

new.dat <- data.frame(length = mean(dat$length),
                      sst = seq(min(dat$sst), max(dat$sst), length.out = 100))


## now acoustic trawl data