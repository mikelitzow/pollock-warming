# load and process age-0 pollock data from beach seines

library(tidyverse)
theme_set(theme_bw())

dat <- read.csv("./data/cod.pollock.lengths.2006.2021.csv", row.names = 1)

dat <- dat %>% 
  filter(species == "poll")

head(dat)

# add sst
sst <- read.csv("./data/western.goa.jan-jul.sst.csv")

# this is average western GOA for January-June (roughly, spawning until settlement)

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

# a couple more exploratory plots

ggplot(dat, aes(julian, length)) +
  geom_point()
# they grow!

ggplot(dat, aes(as.factor(year), as.numeric(sst))) +
  geom_bar(stat = "identity")

ggplot(dat, aes(sst, length)) +
  geom_point()

ggplot(dat) +
  aes(x = year, y = length, color = site) +
  geom_point() +
  facet_wrap( ~ bay) +
  theme(legend.position = "none")

# ready to analyze!!
mod1 <- mgcv::gam(length ~ s(julian, k = 3) + s(sst, k = 3) + bay_fac, data = dat)

summary(mod1)
plot(mod1)
# suggests strong monotonic sst effects
# also suggests some strong bay differences
# but these are all over the map geographically 
# - i.e., strong pos and neg effects on east side of Kodiak
# I think it may be best to use bay as a random term
# and also day_site nested within year to control for pseudo-replication 

# ok, try a mixed-effects model 

# start simply with a bay random intercept
mod2 <- gamm(length ~ s(julian, k = 3) + s(sst, k = 3), 
              random = list(bay_fac=~1), data = dat)

summary(mod2$gam)
plot(mod2$gam)
# much the same result as mod1!

# now add day_site as an additional random term
# the way I read the $lme summary, this produces 
# day_site nested within bay_fac - we should confirm this!
mod3 <- gamm(length ~ s(julian, k = 3) + s(sst, k = 3), 
             random = list(bay_fac=~1, day_site=~1), data = dat)

summary(mod3$lme)
summary(mod3$gam)
plot(mod3$gam)

# finally, add year_fac
# again, if I read the $lme summary right,
# this is producing nested random terms
mod4 <- gamm(length ~ s(julian, k = 3) + s(sst, k = 3), 
             random = list(year_fac=~1, bay_fac=~1, day_site=~1), data = dat)

summary(mod4$lme)
summary(mod4$gam)
plot(mod4$gam)

MuMIn::AICc(mod1, mod2, mod3, mod4)
# mod 4 is the best - I would propose that as a suitable model

## try in gamm4 as well
library(gamm4)

# tweak data
options(lmerControl=list(check.nobs.vs.rankZ = "warning",
                         check.nobs.vs.nlev = "warning",
                         check.nobs.vs.nRE = "warning",
                         check.nlev.gtreq.5 = "warning",
                         check.nlev.gtr.1 = "warning"))


mod5 <- gamm4(length ~ s(julian, k = 3) + s(sst, k = 3), 
             random = ~(1|bay_fac), data = dat)

# Warning messages:
# 1: grouping factors must have > 1 sampled level 
# 2: grouping factors with < 5 sampled levels may give unreliable estimates 

summary(mod5$mer)
summary(mod5$gam)

mod6 <- gamm4(length ~ s(julian, k = 3) + s(sst, k = 3), 
              random = ~(1|bay_fac/day_site), data = dat)

# Warning messages:
# 1: grouping factors must have > 1 sampled level 
# 2: grouping factors with < 5 sampled levels may give unreliable estimates

summary(mod6$mer)
summary(mod6$gam)

# Warning messages:
# 1: grouping factors must have > 1 sampled level 
# 2: grouping factors with < 5 sampled levels may give unreliable estimates

mod7 <- gamm4(length ~ s(julian, k = 3) + s(sst, k = 3), 
              random = ~(1|year_fac/bay_fac/day_site), data = dat)

summary(mod7$mer)
summary(mod7$gam)

plot(mod7$gam)


######
# ok, here is my summary:

# ideally I would like to control
# for non-independence among sets (day_sites) nested within bays, nested within years

# I can't assess the validity of these different random parameterizations and don't have
# the time to read up on it

# but there is support for larger size-at-date for age-0 pollock with increasing
# temperatures...

# thoughts?
######

