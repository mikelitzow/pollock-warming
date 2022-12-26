# three data sources: beach seines, acoustic trawl, observer data 

library(tidyverse)
library(mgcv)
library(lme4)
library(lemon)
library(gamm4)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
sst <- read.csv("./data/annual.wSST.winter.csv")
# this is average western GOA for January-June (roughly, spawning until settlement)

dat <- left_join(dat, sst)

str(dat)

# change to all lowercase
names(dat) <- str_to_lower(names(dat))

# add julian day
dat$julian <- lubridate::yday(lubridate::parse_date_time(dat$date, orders="mdy"))

# save julian day for combined histogram

seine_hist <- data.frame(data = "Beach seine",
                         julian = dat$julian)


# save lat/long for site fig
bay <- read.csv("./data/bay_lat_long.csv", row.names = 1) %>%
  rename(bay = Bay)

unique(bay$bay)
unique(dat$bay)

# align names
change <- bay$bay == "Port Wrangell"
bay$bay[change] <- "Pt Wrangell"

change <- bay$bay == "Ugak "
bay$bay[change] <- "Ugak"

change <- dat$bay == "Ugak "
dat$bay[change] <- "Ugak"

seine_loc <- left_join(dat, bay) %>% 
  select(bay, lat, lon) %>%
  filter(!bay %in% c("Falmouth", "Baralof")) %>%
  group_by(bay) %>%
  summarise(lat = mean(lat), long = mean(lon)) %>%
  select(-bay) %>%
  mutate(data = "Beach seine")


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

# summarize effort
bay_year <- dat %>%
  dplyr::group_by(year, bay) %>%
  dplyr::summarise(count = n()) 

sum(bay_year$count)

bay_year <- bay_year %>%
  pivot_wider(names_from = bay, values_from = count)

bay_year

length(unique(dat$station_fac))

# ready to analyze!!

# log-transform and scale to aid plotting with other data
dat$weight <- scale(log(dat$weight))


# try a mixed-effects model 

# start simply with a station random intercept
mod1_seine <- gamm4(weight ~ s(julian, k = 5) + sst, 
             random=~(1|station_fac), data = dat)

summary(mod1_seine$gam)
plot(mod1_seine$gam)


# random station nested within bay effects
mod2_seine <- gamm4(weight ~ s(julian, k = 5) + sst, 
              random=~(1|bay_fac/station_fac), data = dat)

summary(mod2_seine$gam)
plot(mod2_seine$gam)


MuMIn::AICc(mod1_seine$mer, mod2_seine$mer) # mod 1 (random station effects) best

# save for AIC table
seine_AIC <- data.frame(data = "Age-0 weight",
                        fixed_effects = "s(julian, k = 5) + sst",
                        random_effects = c("station","bay/station"),
                        AICc = MuMIn::AICc(mod1_seine$mer, mod2_seine$mer)$AICc,
                        delta_AICc =  MuMIn::AICc(mod1_seine$mer, mod2_seine$mer)$AICc- min(MuMIn::AICc(mod1_seine$mer, mod2_seine$mer)$AICc))


# and plot
new.dat <- data.frame(julian = mean(dat$julian),
                      sst = seq(min(dat$sst), max(dat$sst), length.out = 100))

predict <- predict(mod1_seine$gam, newdata = new.dat, se = T)

plot_dat_seine <- data.frame(sst = new.dat$sst,
                       effect = predict$fit, 
                       LCI = predict$fit - 1.96*predict$se.fit,
                       UCI = predict$fit + 1.96*predict$se.fit)

ggplot(plot_dat_seine, aes(sst, effect)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), fill = "grey", alpha = 0.3) +
  labs(x = "Mean winter SST (°C)",
       y = "Log weight anomaly")

ggsave("./figs/sst_age0_weight.png", width = 6, height = 4, units = "in")

# save data for rug plot
rug_seine <- data.frame(data = "Beach seine",
                        sst = unique(dat$sst), 
                        age = "Age 0")

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
temp <- dat %>%
  select(DateTime) 

temp.more <- separate(temp, DateTime, into = c("date", "time"), sep = "\\s")

temp.more$date <- chron::dates(temp.more$date)

sum(is.na(temp.more$date)) # 0!

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
# longitude 158 - 150 W (Shelikof and Kodiak)

mature.weights <- dat %>%
  dplyr::filter(maturity_table_3 %in% 2:5,
         Age %in% 4:10,
         sex.code %in% 1:2,
         Longitude < -150 & Longitude > -158)

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


mature.weights <- rbind(female.weights, male.weights)

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
all.dat$year.factor <- as.factor(all.dat$year)
all.dat$haul.factor <- as.factor(all.dat$Haul)

## lag annual sst
# load sst data
sst_lagged <- read.csv("./data/western.goa.sst.csv")


sst_lagged$prevyr_annual.wSST <- NA
k<-1
for(k in 1:length(sst_lagged$year)){
  if(k>1){
    sst_lagged$prevyr_annual.wSST[k] <- sst_lagged$annual.wSST[k-1] 
  }
} #works now join

sstjoin <- sst_lagged[,c(1,4)]

dat_lag <- left_join(all.dat, sstjoin)

check <- dat_lag %>%
  group_by(year.factor) %>%
  summarize(count = length(unique(haul.factor)))

sum(check$count)

check <- dat_lag %>%
  group_by(year.factor) %>%
  summarize(count = n())

sum(check$count)

length(check$year.factor)

# save julian dates for histogram
acoustic_hist <- data.frame(data = "Acoustic trawl",
                         julian = dat_lag$julian)

# Save locations
acoustic_loc <- dat_lag %>%
  mutate(data = "Acoustic trawl") %>%
  rename(lat = Latitude,
         long = Longitude) %>%
  select(data, lat, long)
  

# model - separate smooths to each age
mod1 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by = age.factor, k=4) + maturity_table_3,
                random=~(1|year.factor/Haul) + (1|cohort), data=dat_lag) 

# save the model object
saveRDS(mod1, "./output/acoustic_trawl_weight_mod1.rds")
mod1_acoustic <- readRDS("./output/acoustic_trawl_weight_mod1.rds")

gam.check(mod1_acoustic$gam)
plot(mod1_acoustic$gam)
summary(mod1_acoustic$gam)
anova(mod1_acoustic$gam)

# model - same smooth for all ages
mod2 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, k = 4) + maturity_table_3,
              random=~(1|year.factor/Haul) + (1|cohort), data=dat_lag) # model does not fit

# save the model object
saveRDS(mod2, "./output/acoustic_trawl_weight_mod2.rds")
mod2_acoustic <- readRDS("./output/acoustic_trawl_weight_mod2.rds")

gam.check(mod2_acoustic$gam)
plot(mod2_acoustic$gam)
summary(mod2_acoustic$gam)
anova(mod2_acoustic$gam)

AIC(mod1_acoustic$mer, mod2_acoustic$mer) # mod1 (age-specific smooths) is best

# refit best formulation as a linear mixed model
library(lmerTest)

linear_mod1 <- lmer(sc.weight ~ prevyr_annual.wSST:age.factor + maturity_table_3 +
               (1|year.factor/haul.factor) + (1|cohort), 
             data = dat_lag)

fixef(linear_mod1)
summary(linear_mod1)

MuMIn::r.squaredGLMM(linear_mod1)

# plot mod1 and mod2 predicted effects on one multi-panel plot

new.dat.mod1 <- data.frame(prevyr_annual.wSST = seq(min(dat_lag$prevyr_annual.wSST),
                                               max(dat_lag$prevyr_annual.wSST),
                                               length.out = 100),
                      age.factor = as.factor(rep(4:10, each = 100)),
                      maturity_table_3 = round(mean(as.numeric(as.character(dat_lag$maturity_table_3, 0)))))

effects_age <- effects::effect(term= "prevyr_annual.wSST:age.factor", mod= linear_mod1,
                               xlevels = 20)

# get effect
effects_temp <- as.data.frame((summary(effects_age))[3]) 

sst_temp <- row.names(effects_temp)

mean_temp <-  effects_temp %>%
  mutate(sst = as.numeric(sst_temp)) %>%
  pivot_longer(cols = starts_with("eff"), names_to = "Age", values_to = "Effect")


# get LCI
effects_temp <- as.data.frame((summary(effects_age))[5]) 

LCI_temp <-  effects_temp %>%
  mutate(sst = as.numeric(sst_temp)) %>%
  pivot_longer(cols = starts_with("lo"), names_to = "Age", values_to = "LCI")

# get UCI
effects_temp <- as.data.frame((summary(effects_age))[7]) 

UCI_temp <-  effects_temp %>%
  mutate(sst = as.numeric(sst_temp)) %>%
  pivot_longer(cols = starts_with("up"), names_to = "Age", values_to = "UCI")

head(mean_temp)
head(LCI_temp)

plot_dat_acoustic <- cbind(mean_temp,
                           LCI_temp[,3],
                           UCI_temp[,3])

plot_dat_acoustic <- plot_dat_acoustic %>%
  mutate(Age = str_replace_all(Age, "effect.", "Age "))

# pred_mod1 <- predict(mod1_acoustic$gam, se = T, newdata = new.dat.mod1)
# 
# plot_dat <- data.frame(age = rep(c("Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10"),
#                                  each = 100),
#                        sst = seq(min(dat_lag$prevyr_annual.wSST),
#                                  max(dat_lag$prevyr_annual.wSST),
#                                  length.out = 100),
#                        Effect = pred_mod1$fit,
#                        UCI = pred_mod1$fit+1.96*pred_mod1$se.fit,
#                        LCI = pred_mod1$fit-1.96*pred_mod1$se.fit)
                       

plot_order <- data.frame(Age = c("Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10"),
                         order = 1:7)


plot_dat_acoustic <- left_join(plot_dat_acoustic, plot_order)

plot_dat_acoustic <- plot_dat_acoustic %>%
  rename(age = Age)

plot_dat_acoustic$age <- reorder(plot_dat_acoustic$age, plot_dat_acoustic$order)

ggplot(plot_dat_acoustic, aes(sst, Effect)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), fill = "grey", alpha = 0.3) +
  labs(x = "Previous year SST (°C)",
       y = "Log weight anomaly") +
  facet_wrap(~age)

ggsave("./figs/sst_vs_weight-age_acoustic_trawl.png", width = 6, height = 5.3, units = 'in')

# save data for rug plot
rug_acoustic <- data.frame(data = "Acoustic trawl", 
                           sst = unique(dat_lag$prevyr_annual.wSST),
                           age = rep(c("Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10"), 
                                     each = length(unique(dat_lag$prevyr_annual.wSST))))

## load observer data---------------

dat1 <- read.csv("./data/pollock_bio_data_vessels.csv")

dat2 <- read.csv("./data/DEBRIEFED_HAUL_vessels.csv")

# restrict dat2 to GOA - NMFS area 610, 620, 630
dat2 <- dat2 %>%
  dplyr::filter(NMFS_AREA %in% c(610, 620, 630))


# restrict dat1 to GOA hauls and !is.na(age)
dat1 <- dat1 %>%
  dplyr::filter(HAUL_JOIN %in% unique(dat2$HAUL_JOIN),
         !is.na(AGE)) %>%
  dplyr::select(HAUL_JOIN, SEX, LENGTH, WEIGHT, AGE)

# combine dat1 and dat2
dat2 <- dat2 %>%
  select(HAUL_JOIN, YEAR, HAUL_DATE, NMFS_AREA, GEAR_RETRIEVAL_LATDD, GEAR_RETRIEVAL_LONDD) %>%
  rename(lat = GEAR_RETRIEVAL_LATDD,
         long = GEAR_RETRIEVAL_LONDD)

dat <- left_join(dat1, dat2)

# limit to age 2-10, known sex, known weight, log-transform weight and scale by age
weight.dat <- dat %>%
  dplyr::filter(AGE %in% 2:10,
         WEIGHT > 0, 
         SEX %in% c("M", "F")) %>%
  mutate(log_weight = log(WEIGHT))

weight.dat <- plyr::ddply(weight.dat, c("SEX", "AGE"), transform, sc.weight = scale(log_weight))

# get julian day
weight.dat$julian <- lubridate::yday(chron::dates(weight.dat$HAUL_DATE))

# check distributions - look good
ggplot(weight.dat, aes(sc.weight)) +
  geom_histogram(bins = 40, fill = "grey", color = "black") +
  facet_wrap(~SEX, ncol = 1)

# set cohort
weight.dat$cohort <- weight.dat$YEAR - weight.dat$AGE 

# set factors
weight.dat$age.factor <- as.factor(weight.dat$AGE)
weight.dat$cohort <- as.factor(weight.dat$cohort)
weight.dat$haul.factor <- as.factor(weight.dat$HAUL_JOIN)
weight.dat$year.factor <- as.factor(weight.dat$YEAR)

## lag annual sst
# load sst data
sst_lagged <- read.csv("./data/western.goa.sst.csv")


sst_lagged$prevyr_annual.wSST <- NA

k<-1
for(k in 1:length(sst_lagged$year)){
  if(k>1){
    sst_lagged$prevyr_annual.wSST[k] <- sst_lagged$annual.wSST[k-1] 
  }
} #works now join

sstjoin <- sst_lagged[,c(1,3,4)] %>%
  rename(YEAR = year)

sstjoin$sst.2yr <- zoo::rollmean(sstjoin$annual.wSST, 2, align = "right", fill = NA)

dat_lag <- left_join(weight.dat, sstjoin)

length(unique(dat_lag$YEAR))

check <- dat_lag %>%
  group_by(YEAR) %>%
  summarise(hauls = length(unique(haul.factor)))

sum(check$hauls)

unique(dat$NMFS_AREA)

# plot to examine sst effects by age
ggplot(dat_lag, aes(prevyr_annual.wSST, sc.weight)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm, se = F) +
  facet_wrap(~AGE)

# save data for histograms
observer_hist <- data.frame(data = "Commercial catch",
                         julian = dat_lag$julian)


# save lat-long for maps
observer_loc <- data.frame(data = "Commercial catch",
                           lat = dat_lag$lat,
                           long = dat_lag$long)

# fit model for plotting time series
obs_mean_mod <- lmer(log_weight ~ year.factor + (1|haul.factor), 
                     data = filter(dat_lag, age.factor == 4))

effects_mean <- effects::effect(term= "year.factor", mod = obs_mean_mod)
summary(effects_mean)

# try linear mixed-effects model
library(lmerTest)
mod1 <- lmer(sc.weight ~ prevyr_annual.wSST:age.factor + (1|year.factor/haul.factor) + (1|cohort), 
                   data = dat_lag)

fixef(mod1)
summary(mod1)

MuMIn::r.squaredGLMM(mod1)

library(sjPlot)

plot_model(mod1)

effects_age <- effects::effect(term= "prevyr_annual.wSST:age.factor", mod= mod1,
                               xlevels = 20)

# get effect
effects_temp <- as.data.frame((summary(effects_age))[3]) 

sst_temp <- row.names(effects_temp)

mean_temp <-  effects_temp %>%
  mutate(sst = as.numeric(sst_temp)) %>%
  pivot_longer(cols = starts_with("eff"), names_to = "Age", values_to = "Effect")


# get LCI
effects_temp <- as.data.frame((summary(effects_age))[5]) 

LCI_temp <-  effects_temp %>%
  mutate(sst = as.numeric(sst_temp)) %>%
  pivot_longer(cols = starts_with("lo"), names_to = "Age", values_to = "LCI")

# get UCI
effects_temp <- as.data.frame((summary(effects_age))[7]) 

UCI_temp <-  effects_temp %>%
  mutate(sst = as.numeric(sst_temp)) %>%
  pivot_longer(cols = starts_with("up"), names_to = "Age", values_to = "UCI")

head(mean_temp)
head(LCI_temp)

plot_dat_observer <- cbind(mean_temp,
                           LCI_temp[,3],
                           UCI_temp[,3])

plot_dat_observer <- plot_dat_observer %>%
  mutate(Age = str_replace_all(Age, "effect.", "Age "))


plot_order <- data.frame(Age = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10"),
                         order = 1:9)


plot_dat_observer <- left_join(plot_dat_observer, plot_order)

plot_dat_observer$Age <- reorder(plot_dat_observer$Age, plot_dat_observer$order)

ggplot(plot_dat_observer, aes(sst, Effect)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = "grey", alpha = 0.3) +
  labs(x = "Previous year SST (°C)",
       y = "Log weight anomaly") +
  facet_wrap(~Age)

# save sst data for rug plot
rug_observer <- data.frame(data = "Commercial catch",
                           sst = unique(dat_lag$prevyr_annual.wSST),
                           age = rep(c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10"),
                                     each = length(unique(dat_lag$prevyr_annual.wSST))))

## combine plot--------------

# combine plot data
head(plot_dat_seine)
head(plot_dat_acoustic)
head(plot_dat_observer)

# clean up and combine
plot_dat_seine$age = "Age 0"
plot_dat_seine$data = "Beach seine"

names(plot_dat_acoustic)[3] = "LCI"
plot_dat_acoustic$data = "Acoustic trawl"

names(plot_dat_observer)[2:3] <- c("age", "effect")
plot_dat_observer$data = "Commercial catch"

plot_dat_acoustic <- plot_dat_acoustic %>%
  select(sst, effect, LCI, UCI, age, data)

plot_dat_observer <- plot_dat_observer %>%
  select(sst, effect, LCI, UCI, age, data)

plot_all <- rbind(plot_dat_seine,
                  plot_dat_acoustic,
                  plot_dat_observer)

plot_order <- data.frame(age = c("Age 0", "Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10"),
                         order = 1:10)

plot_all <- left_join(plot_all, plot_order)

plot_all$age <- reorder(plot_all$age, plot_all$order)

data_order <- data.frame(data = c("Beach seine", "Acoustic trawl", "Commercial catch"),
                         data_order = 1:3)

plot_all <- left_join(plot_all, data_order)

plot_all$data <- reorder(plot_all$data, plot_all$data_order)

# and combine rug plots
rug_all <- rbind(rug_seine,
                 rug_acoustic,
                 rug_observer)

rug_all <- left_join(rug_all, plot_order)

rug_all$age <- reorder(rug_all$age, rug_all$order)

my.col <- cb[c(4,2,6)]

plot <- ggplot(plot_all, aes(sst, effect, fill = data, color = data)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, lty = 0) +
  labs(x = "SST (°C)",
       y = "Log weight anomaly") +
  geom_rug(data = rug_all, aes(x = sst, y = NULL))  +
  facet_wrap(~age, scales = "free_x", ncol = 4) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col)

png("./figs/combined_sst_weight_age_plot.png", width = 6, height = 5, units = 'in', res = 300)
plot2 <-  reposition_legend(plot, position = 'top right', panel='panel-4-3')
dev.off()

# plot combined histogram
all_hist <- rbind(seine_hist, acoustic_hist, observer_hist)

all_hist <- left_join(all_hist, data_order)

all_hist$data <- reorder(all_hist$data, all_hist$data_order)

# get total sample size
n_total <- all_hist %>%
  group_by(data) %>%
  summarise(n = prettyNum(n(), big.mark = ",")) 


all_hist <- left_join(all_hist, n_total) %>%
  mutate(label = paste(data, " (n = ", n, ")", sep = ""))



all_hist <- left_join(all_hist, data_order)

all_hist$data <- reorder(all_hist$data, all_hist$data_order)
all_hist$label <- reorder(all_hist$label, all_hist$data_order)

ggplot(all_hist, aes(julian, fill = data)) +
  geom_histogram(bins = 100, alpha = 0.4, color = "black") +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = my.col) +
  theme(legend.position = "none") +
  labs(x = "Day of year", 
       y = "Count") +
  scale_x_continuous(breaks = c(1, seq(30, 360, by = 30)))

ggsave("./figs/weight_seasonal_histograms.png", width = 5, height = 5, units = 'in')

## plot sample sites ------------------------------------

# flip sign for seine long
seine_loc$long <- -seine_loc$long

# reduce acoustic and observer locations to one per tow
acoustic_plot <- acoustic_loc %>%
  mutate(index = paste(lat, long, sep = "_")) %>%
  group_by(index) %>%
  summarise(lat = mean(lat),
            long = mean(long)) %>%
  mutate(data = "Acoustic trawl") %>%
  select(data, lat, long)

nrow(acoustic_loc); nrow(acoustic_plot)

observer_plot <- observer_loc %>%
  mutate(index = paste(lat, long, sep = "_")) %>%
  group_by(index) %>%
  summarise(lat = mean(lat),
            long = mean(long)) %>%
  mutate(data = "Commercial catch") %>%
  select(data, lat, long)

nrow(observer_loc); nrow(observer_plot)

plot_loc <- rbind(seine_loc,
                  acoustic_plot,
                  observer_plot)

# arrange and set up plot 
plot_loc <- left_join(plot_loc, data_order)
plot_loc$data <- reorder(plot_loc$data, plot_loc$data_order)

# set up point shapes and alpha
plot_loc <- plot_loc %>%
  mutate(shape = if_else(data == "Beach seine", 21, 3),
         alpha = if_else(data == "Beach seine", 1, 0.2),
         size = if_else(data == "Beach seine", 2,
                        if_else(data == "Acoustic trawl", 1, 0.7)),
         color = if_else(data == "Beach seine", "black",
                         if_else(data == "Acoustic trawl", my.col[2], my.col[3])),
         fill = if_else(data == "Beach seine", my.col[1], "black"))


library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")

world <- ne_countries(scale = "medium", returnclass = "sf")

#bigger map
ggplot(data = world) +
  geom_sf(fill = "gray50") +
  coord_sf(xlim = c(-171, -148), ylim = c(52, 60.5), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  geom_point(aes(long, lat), data=plot_loc,
             shape = plot_loc$shape,
             fill = plot_loc$fill,
             color = plot_loc$color,
             size = plot_loc$size,
             alpha = plot_loc$alpha) +
  facet_wrap(~data, ncol = 1) +
  theme(axis.title = element_blank())

ggsave("./figs/weight_study_site_maps.png", width = 4, height = 7, units = 'in')
