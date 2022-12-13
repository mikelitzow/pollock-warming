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


## now acoustic trawl data ----------------------------------------


# first, combine data for different periods into a single df

d1 <- read.csv("./data/ShelikofHistorical_86_94_v2.csv")

d2 <- read.csv("./data/ShelikofHistorical_1995_2002.csv")

d3 <- read.csv("./data/ShelikofHistorical_2003_2020.csv")

d4 <- read.csv("./data/ShelikofHistorical_0704.csv")

# make d1/d2 names compatible
names(d1)[11] <- "Sex"

# lump into the original maturity schedule (Table 3)
d1$maturity_table_3 <- d1$Maturity

d2$maturity_table_3 <- ifelse(d2$MaturityTable == 3, d2$Maturity,
                              ifelse(d2$MaturityTable == 11 & d2$Maturity == 1, 1,
                                     ifelse(d2$MaturityTable == 11 & d2$Maturity %in% c(2,3), 2,
                                            ifelse(d2$MaturityTable == 11 & d2$Maturity %in% c(4,5), 3,
                                                   ifelse(d2$MaturityTable == 11 & d2$Maturity == 6, 4, 5)))))
d3$maturity_table_3 <- case_when(
  d3$Maturity == "Immature" ~ 1,
  d3$Maturity %in% c("Developing I", "Developing II", "Developing") ~ 2,
  d3$Maturity %in% c("Prespawning I", "Prespawning II", "Prespawning") ~ 3,
  d3$Maturity == "Spawning" ~ 4,
  d3$Maturity %in% c("Spent I", "Spent II", "Spent") ~ 5)

d4$maturity_table_3 <- case_when(
  d4$Maturity == "Immature" ~ 1,
  d4$Maturity %in% c("Developing I", "Developing II", "Developing") ~ 2,
  d4$Maturity %in% c("Prespawning I", "Prespawning II", "Prespawning") ~ 3,
  d4$Maturity == "Spawning" ~ 4,
  d4$Maturity %in% c("Spent I", "Spent II", "Spent") ~ 5)

# clean up names
names(d1)[6:7] <- names(d2)[6:7] <- names(d3)[6:7] <- names(d4)[6:7] <- c("length", "weight")

# combine
d1 <- d1 %>% 
  select(Survey, Haul, Latitude, Longitude, DateTime, length, weight, Age, Sex, maturity_table_3)

d2 <- d2 %>% 
  select(Survey, Haul, Latitude, Longitude, DateTime, length, weight, Age, Sex, maturity_table_3)

d3 <- d3 %>% 
  select(Survey, Haul, Latitude, Longitude, DateTime, length, weight, Age, Sex, maturity_table_3)

d4 <- d4 %>% 
  select(Survey, Haul, Latitude, Longitude, DateTime, length, weight, Age, Sex, maturity_table_3)

# change d3/d4 weight to g
d3$weight <- 1000*d3$weight
d4$weight <- 1000*d4$weight

dat <- rbind(d1, d2, d3, d4)

# get julian day
dat$julian <- lubridate::yday(temp.more$date)
dat$year <- lubridate::year(temp.more$date)

# add sex 
dat$sex.code <- if_else(dat$Sex == 1, 1,
                        if_else(dat$Sex == 2, 2,
                                if_else(dat$Sex == "Male", 1, 
                                        ifelse(dat$Sex == "Female", 2, NA))))


# now need to scale weights by age and maturity stage

# first, clean up - only known sex, known age, age 4-10,
# stages developing - spent

mature.weights <- dat %>%
  dplyr::filter(maturity_table_3 %in% 2:5,
         Age %in% 4:10,
         sex.code %in% 1:2)

# log transform weight
mature.weights$log.weight <- log(mature.weights$weight)

# check for NAs (age 7 below is plotting as no data)
check <- is.na(mature.weights$log.weight)
sum(check)

dplyr::filter(mature.weights, Age == 7)
mature.weights[which.min(mature.weights$log.weight),] # one value of weight = 0!!

mature.weights <- mature.weights %>%
  dplyr::filter(weight > 0)

# need to scale weight by age and sex
females <- mature.weights %>%
  dplyr::filter(sex.code == 2)

males <- mature.weights %>%
  dplyr::filter(sex.code == 1)


female.weights <- plyr::ddply(females, "Age", transform, sc.weight = scale(log.weight))

male.weights <- plyr::ddply(males, "Age", transform, sc.weight = scale(log.weight))


mature.weights <- rbind(females, males)

# check
ggplot(mature.weights, aes(log.weight, sc.weight, color = as.factor(sex.code))) +
  geom_point() +
  facet_wrap(~Age, scales = "free")

ggplot(mature.weights, aes(sc.weight)) +
  geom_histogram(fill = "grey", color = "black", bins = 40) +
  facet_wrap(~sex.code, scales = "free", ncol = 1)

# assign year class
levels(mature.weights$year)
mature.weights$year <- as.numeric(as.character(mature.weights$year))

mature.weights$year.class <- mature.weights$year - mature.weights$Age

# plot to check
ggplot(mature.weights, aes(Age)) +
  geom_histogram() +
  facet_wrap(~year.class)

# clean up
all.dat <- mature.weights %>%
  select(-DateTime, -Sex)

# set factors
all.dat$sex.code <- as.factor(all.dat$sex.code)
all.dat$age.factor <- as.factor(all.dat$Age)
all.dat$maturity_table_3 <- as.factor(all.dat$maturity_table_3)
all.dat$cohort <- as.factor(all.dat$year.class)

#lag annual sst------
#load sst data
sst_lagged <- read.csv("./data/western.goa.sst.csv")


sst_lagged$prevyr_annual.wSST <- NA
k<-1
for(k in 1:length(sst_lagged$year)){
  if(k>1){
    sst_lagged$prevyr_annual.wSST[k] <- sst_lagged$annual.wSST[k-1] 
  }
} #works now join

sstjoin <- sst_lagged[,c(1,5)]

dat_lag <- left_join(all.dat, sstjoin)

# model - separate smooths to each age
mod1 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by = age.factor, k=4) + maturity_table_3,
                random=~(1|year/Haul) + (1|cohort), data=dat_lag) # model does not fit

# save the model object
saveRDS(mod1, "./output/acoustic_trawl_weight_mod1.rds")
weight_mod1 <- readRDS("./output/acoustic_trawl_weight_mod1.rds")

gam.check(mod1$gam)
plot(mod1$gam)
summary(mod1$gam)
anova(mod1$gam)

# model - same smooth for all ages
mod2 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, k = 4) + maturity_table_3,
              random=~(1|year/Haul) + (1|cohort), data=dat_lag) # model does not fit

# save the model object
saveRDS(mod2, "./output/acoustic_trawl_weight_mod2.rds")
mod2 <- readRDS("./output/acoustic_trawl_weight_mod2.rds")

gam.check(mod2$gam)
plot(mod2$gam)
summary(mod2$gam)
anova(mod2$gam)

AIC(mod1$mer, mod2$mer) # mod1 (age-specific smooths) is best
