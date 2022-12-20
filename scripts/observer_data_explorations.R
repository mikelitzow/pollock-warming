library(tidyverse)
theme_set(theme_bw())

dat1 <- read.csv("./data/pollock_bio_data_vessels.csv")

dat2 <- read.csv("./data/DEBRIEFED_HAUL_vessels.csv")

unique(dat1$SPECIES)
unique(dat2$YEAR)
unique(dat2$GEAR_TYPE)
unique(dat2$NMFS_AREA)

# restrict dat2 to GOA - NMFS area 610, 620, 630
dat2 <- dat2 %>%
  filter(NMFS_AREA %in% c(610, 620, 630))

# summarize by gear
summ <- dat2 %>%
  group_by(GEAR_TYPE) %>%
  summarize(count = n())

summ

# restrict dat1 to GOA hauls and !is.na(age)

dat1 <- dat1 %>%
  filter(HAUL_JOIN %in% unique(dat2$HAUL_JOIN),
         !is.na(AGE)) %>%
  select(HAUL_JOIN, SEX, LENGTH, WEIGHT, AGE)

# examine gear type
keep <- is.na(dat2$GEAR_TYPE) # 0!

unique(dat2$GEAR_TYPE) # some blanks...

# ignore gear type in analysis for now

# combine dat1 and dat2

dat2 <- dat2 %>%
  select(HAUL_JOIN, YEAR, HAUL_DATE, NMFS_AREA)

dat <- left_join(dat1, dat2)


ggplot(dat, aes(AGE, WEIGHT)) +
  geom_point() + 
  facet_wrap(~YEAR) +
  geom_smooth(method="gam", color="red", formula = y ~ s(x, k = 5))

# look at julian day by year
dat$julian <- lubridate::yday(chron::dates(dat$HAUL_DATE))

ggplot(dat, aes(julian)) +
  geom_histogram(bins = 50, fill = "grey", color = "black") + 
  facet_wrap(~YEAR, scales = "free_y") 

ggplot(dat, aes(AGE)) +
  geom_histogram(bins = 18, fill = "grey", color = "black") + 
  facet_wrap(~YEAR, scales = "free_y") 

ggplot(dat, aes(AGE)) +
  geom_histogram(bins = 18, fill = "grey", color = "black")

age.diversity <- dat %>%
  group_by(YEAR, AGE) %>%
  summarize(count = n())

year.total <- age.diversity %>%
  group_by(YEAR) %>%
  summarise(year.total = sum(count)) 

age.diversity <- left_join(age.diversity, year.total) %>%
  mutate(proportion = count / year.total,
         ln.proportion = log(proportion),
         product = proportion*ln.proportion)

age.shannon <- age.diversity %>%
  group_by(YEAR) %>%
  summarise(shannon = -sum(product))

ggplot(age.shannon, aes(YEAR, shannon)) +
  geom_line() +
  geom_point()


# save
write.csv(age.shannon, "./data/observer.age.diversity.csv", row.names = F)

# load sst as a covariate
sst <- read.csv("./data/monthly_western_GOA_SST_anomalies_wrt_1980-2021.csv")

# now cross-correlate sst-diversity

sst$shannon <- ifelse(sst$month == 3, age.shannon$shannon[match(sst$year, age.shannon$YEAR)], NA)

sst$lag.neg.36 <- lag(sst$anom, 36)
sst$lag.neg.35 <- lag(sst$anom, 35)
sst$lag.neg.34 <- lag(sst$anom, 34)
sst$lag.neg.33 <- lag(sst$anom, 33)
sst$lag.neg.32 <- lag(sst$anom, 32)
sst$lag.neg.31 <- lag(sst$anom, 31)
sst$lag.neg.30 <- lag(sst$anom, 30)
sst$lag.neg.29 <- lag(sst$anom, 29)
sst$lag.neg.28 <- lag(sst$anom, 28)
sst$lag.neg.27 <- lag(sst$anom, 27)
sst$lag.neg.26 <- lag(sst$anom, 26)
sst$lag.neg.25 <- lag(sst$anom, 25)

sst$lag.neg.24 <- lag(sst$anom, 24)
sst$lag.neg.23 <- lag(sst$anom, 23)
sst$lag.neg.22 <- lag(sst$anom, 22)
sst$lag.neg.21 <- lag(sst$anom, 21)
sst$lag.neg.20 <- lag(sst$anom, 20)
sst$lag.neg.19 <- lag(sst$anom, 19)
sst$lag.neg.18 <- lag(sst$anom, 18)
sst$lag.neg.17 <- lag(sst$anom, 17)
sst$lag.neg.16 <- lag(sst$anom, 16)
sst$lag.neg.15 <- lag(sst$anom, 15)
sst$lag.neg.14 <- lag(sst$anom, 14)
sst$lag.neg.13 <- lag(sst$anom, 13)

sst$lag.neg.12 <- lag(sst$anom, 12)
sst$lag.neg.11 <- lag(sst$anom, 11)
sst$lag.neg.10 <- lag(sst$anom, 10)
sst$lag.neg.9 <- lag(sst$anom, 9)
sst$lag.neg.8 <- lag(sst$anom, 8)
sst$lag.neg.7 <- lag(sst$anom, 7)
sst$lag.neg.6 <- lag(sst$anom, 6)
sst$lag.neg.5 <- lag(sst$anom, 5)
sst$lag.neg.4 <- lag(sst$anom, 4)
sst$lag.neg.3 <- lag(sst$anom, 3)
sst$lag.neg.2 <- lag(sst$anom, 2)
sst$lag.neg.1 <- lag(sst$anom, 1)

sst$lag.0 <- sst$anom

sst$lag.pos.1 <- lead(sst$anom, 1)
sst$lag.pos.2 <- lead(sst$anom, 2)
sst$lag.pos.3 <- lead(sst$anom, 3)
sst$lag.pos.4 <- lead(sst$anom, 4)
sst$lag.pos.5 <- lead(sst$anom, 5)
sst$lag.pos.6 <- lead(sst$anom, 6)
sst$lag.pos.7 <- lead(sst$anom, 7)
sst$lag.pos.8 <- lead(sst$anom, 8)
sst$lag.pos.9 <- lead(sst$anom, 9)
sst$lag.pos.10 <- lead(sst$anom, 10)
sst$lag.pos.11 <- lead(sst$anom, 11)
sst$lag.pos.12 <- lead(sst$anom, 12)

sst$lag.pos.13 <- lead(sst$anom, 13)
sst$lag.pos.14 <- lead(sst$anom, 14)
sst$lag.pos.15 <- lead(sst$anom, 15)
sst$lag.pos.16 <- lead(sst$anom, 16)
sst$lag.pos.17 <- lead(sst$anom, 17)
sst$lag.pos.18 <- lead(sst$anom, 18)
sst$lag.pos.19 <- lead(sst$anom, 19)
sst$lag.pos.20 <- lead(sst$anom, 20)
sst$lag.pos.21 <- lead(sst$anom, 21)
sst$lag.pos.22 <- lead(sst$anom, 22)
sst$lag.pos.23 <- lead(sst$anom, 23)
sst$lag.pos.24 <- lead(sst$anom, 24)

cor.dat <- sst[,5:66]

cors <- cor(cor.dat, use = "p")

dim(cors)

cor.plot <- data.frame(lag = -36:24,
                       cor = cors[1,2:62])


ggplot(cor.plot, aes(lag, cor)) +
  geom_bar(stat = "identity", fill = "grey", color = "dark grey") +
  scale_x_continuous(breaks = seq(-36, 24, by = 6)) +
  labs(x = "lag (months; 0 = March of sampling year)",
       y = "SST - Shannon correlation")

## average for the two years prior as covariate for age diversity

sst.annual <- tapply(sst$anom, sst$year, mean)

sst.annual <- data.frame(year = as.numeric(names(sst.annual)),
                         sst = sst.annual,
                         sst.2 = zoo::rollmean(sst.annual, 2, align = "right", fill = NA),
                         sst.3 = zoo::rollmean(sst.annual, 3, align = "right", fill = NA))


ggplot(sst.annual, aes(year, sst)) +
  geom_line() + 
  geom_point() +
  geom_line(aes(year, sst.2), color = "red")

age.shannon <- age.shannon %>%
  rename(year = YEAR)

sst.annual <- left_join(sst.annual, age.shannon)

sst.annual <- sst.annual %>%
  pivot_longer(cols = c(-year, -shannon))

ggplot(sst.annual, aes(value, shannon)) +
  geom_point() +
  facet_wrap(~name)

# compare predictive skill / model fit
library(mgcv)

sst.annual <- sst.annual %>%
  pivot_wider(names_from = name)

mod1 <- gam(shannon ~ s(sst, k = 4), data = sst.annual)
mod2 <- gam(shannon ~ s(sst.2, k = 4), data = sst.annual)
mod3 <- gam(shannon ~ s(sst.3, k = 4), data = sst.annual)

MuMIn::AICc(mod1, mod2, mod3) # ain't no denying - mod3 best by far

summary(mod3)
plot(mod3, residuals = T, pch = 19)

## scaled weight at age as a function of SST ------------------------

# check range of weights and ages

ggplot(dat, aes(WEIGHT)) +
  geom_histogram(bins = 50, fill = "grey", color = "black")
# looks fine, all in kg

ggplot(dat, aes(AGE)) +
  geom_histogram(bins = 18, fill = "grey", color = "black")

# limit to age 2-10, log-transform weight and scale by age
weight.dat <- dat %>%
  filter(AGE %in% 2:10) %>%
  mutate(log_weight = log(WEIGHT))
  
weight.dat <- plyr::ddply(weight.dat, c("SEX", "AGE"), transform, sc.weight = scale(log_weight))
  
