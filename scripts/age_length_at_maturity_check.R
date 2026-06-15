library(tidyverse)
library(mgcv)
library(lme4)
library(lemon)
library(gamm4)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## age and length at maturity from 2022 assessment

dat <- read.csv("./data/age_and_length_50_maturity.csv")

head(dat)

ggplot(dat, aes(Year, Age)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F)


# load sst
sst <- read.csv("./data/western.goa.sst.1950.2022.csv")

# process
sst <- sst %>%
  filter(name == "Annual") %>%
  rename(sst = value) %>%
  select(-name) %>%
  mutate(sst.3 = zoo::rollmean(sst, 3, align = "right", fill = NA),
         lag.sst3 = lag(sst.3))

# join with age & length
dat <- dat %>%
  rename(year = Year,
         age = Age) %>%
  left_join(., sst)

# compare with the same models as for diversity sst, sst.3, sst.3 lag 1

mod1 <- gamm(age ~  s(sst,  k=4), 
     correlation = corAR1(), data = dat)

summary(mod1$gam)

mod2 <- gamm(age ~  s(sst.3,  k=4), 
             correlation = corAR1(), data = dat)

summary(mod2$gam)
plot(mod1$gam, se = T, resid = T, pch = 19)

mod3 <- gamm(age ~  s(lag.sst3,  k=4), 
             correlation = corAR1(), data = dat)

summary(mod3$gam)

# and evaluate year effect
age.mod <- gamm(age ~  s(year,  k=4), correlation = corAR1(),
               data = dat)

summary(age.mod$gam)
plot(age.mod$gam, se = T, resid = T, pch = 19)

