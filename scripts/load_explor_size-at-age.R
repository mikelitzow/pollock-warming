#================================================================================
#Load & explore size-at-age data
#================================================================================

#data wrangling----------
#copied from acoustic_trawl_data_explor
library(tidyverse)
theme_set(theme_bw())

# first, combine data for different periods into a single df

d1 <- read.csv("./data/ShelikofHistorical_86_94_v2.csv")

d2 <- read.csv("./data/ShelikofHistorical_1995_2002.csv")

d3 <- read.csv("./data/ShelikofHistorical_2003_2020.csv")

d4 <- read.csv("./data/ShelikofHistorical_0704.csv")

head(d1); head(d2); head(d3); head(d4)

# check dates on d4 / d3
unique(d4$DateTime) # March 13-28 2007
unique(d3$DateTime)

check <- grep("2007", d3$DateTime)
d3[check,] # no matches - that's good!


# make d1/d2 names compatible
names(d1)[11] <- "Sex"

identical(names(d1), names(d2))

identical(names(d2), names(d3))

names(d2); names(d3)

# examine maturity table
mat <- read.csv("./data/maturity.csv")
mat

# see how these are used in the different data sets
unique(d1$Maturity)
unique(d1$MaturityTable)

unique(d2$Maturity)
unique(d2$MaturityTable)

unique(d3$Maturity)
unique(d3$MaturityIndex)

unique(d4$Maturity)
unique(d4$MaturityIndex)

# for an initial look, I will lump into the original maturity schedule (Table 3)
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

# # limit to known-age
# 
# drop <- is.na(dat$Age)
# 
# dat <- dat[!drop,]

# get julian day
temp <- dat %>%
  select(DateTime) 

temp.more <- separate(temp, DateTime, into = c("date", "time"), sep = "\\s")

temp.more$date <- chron::dates(temp.more$date)

dat$julian <- lubridate::yday(temp.more$date)
dat$year <- lubridate::year(temp.more$date)
unique(dat$year)

hist(dat$julian) # looks right!

#limit to observations within range of interest by limiting longitude

dat <- dat[which(dat$Longitude> -158 & dat$Longitude< -150),]

#end data wrangling-----

library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")

world <- ne_countries(scale = "medium", returnclass = "sf")

#bigger map
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -145), ylim = c(53, 61), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(Longitude,Latitude,  col=year), data=dat) +   
  scale_color_distiller(palette = "Spectral")


#smaller map
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-160, -150), ylim = c(54, 60), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(Longitude,Latitude,  col=year), data=dat) +   
  scale_color_distiller(palette = "Spectral")


#explore size-at-age-------

ggplot(dat, aes(Age, weight)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) +
  facet_wrap(~year)

# becomes very age-truncated in recent years!
# examine just age 1-10
ggplot(dplyr::filter(dat, Age %in% 1:10), aes(Age, weight)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) +
  facet_wrap(~year)

ggplot(dplyr::filter(dat, Age %in% 4:10), aes(Age, weight, color = as.factor(as.character(year)))) +
  geom_point(stroke = 0, size=0) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) +
  coord_cartesian(ylim = c(200, 2200)) + 
  scale_color_viridis_d()

ggplot(dat, aes(Age)) +
  geom_histogram(bins = 23) +
  facet_wrap(~year, scale = "free_y")

# age structure collapsed (?!)

# examine just spawning fish < 15 years old
ggplot(dplyr::filter(dat, maturity_table_3 == 4, Age < 15), aes(Age)) +
  geom_histogram(bins = 23) +
  facet_wrap(~year, scale = "free_y")

# and weight of spawning fish
ggplot(dplyr::filter(dat, maturity_table_3 == 4), aes(weight)) +
  geom_histogram() +
  facet_wrap(~year, scale = "free_y")

# spawn.age <- dat %>%
#   filter(Age %in% 1:15 , maturity_table_3 == 4)

spawn.age <- dat[which(dat$Age >=1 & dat$Age<16 & dat$maturity_table_3==4),]

ggplot(spawn.age, aes(as.factor(Age), weight)) +
  geom_violin() +
  facet_wrap(~year)

# mean and sd of spawning age by year
spawn.summary <- spawn.age %>%
  group_by(year) %>%
  summarize(mean.age = mean(Age), sd.age = sd(Age)) %>%
  pivot_longer(cols = - year) %>%
  mutate(year = as.numeric(as.character(year)))

ggplot(spawn.summary, aes(year, value)) +
  geom_line() +
  geom_point() + 
  facet_wrap(~name, scales = "free_y", ncol=1)

# spawn date
# spawn.all <- dat %>%
#   filter(maturity_table_3 == 4)

spawn.all <- dat[which(dat$maturity_table_3==4),]

ggplot(spawn.all, aes(julian)) +
  geom_histogram(bins=20, fill = "dark grey", color = "black") + 
  facet_wrap(~year, scales = "free_y") +
  coord_cartesian(xlim = c(60, 90))

# add sex 
dat$sex.code <- if_else(dat$Sex == 1, 1,
                        if_else(dat$Sex == 2, 2,
                                if_else(dat$Sex == "Male", 1, 
                                        ifelse(dat$Sex == "Female", 2, NA))))

table(dat$Sex, dat$maturity_table_3)

table(dat$sex.code, dat$maturity_table_3)

# fit an exploratory model to weight of spawners with age, sex, and year effects
library(mgcv)

# spawn.age <- dat %>%
#   filter(Age %in% 1:20, maturity_table_3 == 4)

spawn.age <- dat[which(dat$Age >=1 & dat$Age<20 & dat$maturity_table_3==4),]

spawn.age$year <- as.factor(as.character(spawn.age$year))
spawn.age$sex.code <- as.factor(spawn.age$sex.code)

mod1 <- gam(weight ~ s(Age, k = 4) + sex.code + year, na.action = "na.exclude",
            data = spawn.age)

summary(mod1)

plot(mod1)

mod2 <- gam(weight ~ s(Age, by = year, k = 4) + sex.code, na.action = "na.exclude",
            data = spawn.age)

summary(mod2)

# save predicted values to plot
age.range <- spawn.age %>%
  group_by(year) %>%
  summarise(min.age = min(Age),
            max.age = max(Age))

plot.pred <- data.frame()

for(i in 1:nrow(age.range)){
  temp.age <- seq(age.range$min.age[i],
                  age.range$max.age[i],
                  length.out = 100)
  
  temp <- data.frame(
    Age = temp.age,
    year = age.range$year[i],
    sex.code = 2)
  plot.pred <- rbind(plot.pred, temp)
  
}

plot.pred$pred.weight <- predict(mod2, type = "response", newdata = plot.pred)

ggplot(plot.pred, aes(Age, pred.weight, color = year)) +
  geom_line() +
  scale_color_viridis_d()


## look at weight distributions by age;
## include spawning and pre-spawning for now

# spawn.prespawn  <- dat %>%
#   filter(maturity_table_3 %in% 3:4, 
#          Age %in% 1:20)

spawn.prespawn <- dat[which(dat$maturity_table_3<5 & dat$maturity_table_3>2 &
                              dat$Age>0 & dat$Age<21),]

ggplot(spawn.prespawn, aes(weight)) +
  geom_histogram(fill = "grey90", color="black") + 
  facet_wrap(~Age, scale = "free_y")

# so, few that are under 4 or older than 10
# and the distributions are definitely skewed

# I'll look at the numbers for developing, spawning, prespawning, spent separately
# develop.spawn.prespawn.spent  <- dat %>%
#   filter(maturity_table_3 %in% 2:5, 
#          Age %in% 1:20)

develop.spawn.prespawn.spent <- dat[which(dat$maturity_table_3<6 & dat$maturity_table_3>1 &
                                            dat$Age >0 & dat$Age <21),]

ggplot(dplyr::filter(develop.spawn.prespawn.spent, maturity_table_3 == 2), aes(as.factor(Age))) +
  geom_histogram(fill = "grey90", color="black", stat = "count") + 
  facet_wrap(~year, scale = "free_y")

ggplot(dplyr::filter(develop.spawn.prespawn.spent, maturity_table_3 == 3), aes(as.factor(Age))) +
  geom_histogram(fill = "grey90", color="black", stat = "count") + 
  facet_wrap(~year, scale = "free_y")

ggplot(dplyr::filter(develop.spawn.prespawn.spent, maturity_table_3 == 4), aes(as.factor(Age))) +
  geom_histogram(fill = "grey90", color="black", stat = "count") + 
  facet_wrap(~year, scale = "free_y")

ggplot(dplyr::filter(develop.spawn.prespawn.spent, maturity_table_3 == 2), aes(as.factor(Age))) +
  geom_histogram(fill = "grey90", color="black", stat = "count") + 
  facet_wrap(~year, scale = "free_y")

## so all 4 classes look reasonable to include!


# 4-8 might capture most of the population

# look at counts per year
ggplot(dplyr::filter(spawn.prespawn, maturity_table_3 == 3,
              Age %in% 4:10), aes(Age)) +
  geom_histogram(fill = "grey90", color="black") + 
  facet_wrap(~year, scale = "free_y")

ggplot(dplyr::filter(spawn.prespawn, maturity_table_3 == 4,
              Age %in% 4:10), aes(Age)) +
  geom_histogram(fill = "grey90", color="black") + 
  facet_wrap(~year, scale = "free_y")

# not many spawner samples in some years - 2000-2003, 2009, 2012

# calculate the proportion of developing / pre-spawning / spawning / spent 
# that are in the dominant year class each year (and sample size!)

mature <- dat %>%
  dplyr::filter(maturity_table_3 %in% 2:5,
         Age %in% 4:10) %>%
  group_by(year, Age) %>%
  summarize(count = n())

ggplot(mature, aes(Age, count)) +
  geom_col(fill = "grey90", color="black", position = "dodge", width = 0.5) + 
  facet_wrap(~year, scale = "free_y")+
  geom_hline(yintercept = 0)

mature <- mature %>%
  pivot_wider(names_from = Age, values_from = count)

mature$n <- apply(mature[,2:8], 1, sum, na.rm=T)

mature$max <- apply(mature[,2:8], 1, max, na.rm=T)

mature$max.prop <- mature$max/mature$n

plot.mature <- mature %>% 
  select(year, n, max.prop) %>%
  pivot_longer(cols = -year)

ggplot(plot.mature, aes(as.numeric(as.character(year)), value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name, scales = "free_y", ncol = 1)

# load sst as a covariate
sst <- read.csv("./data/monthly.western.GOA.SST.anomalies.wrt.1980-2020.csv")

# now cross-correlate sst-diversity

# add winter year to sst
sst$winter.yr <- ifelse(sst$month > 3, sst$year + 1, sst$year)




# now need to scale weights by age and maturity stage

# first, clean up - only known sex, known age, age 4-10,
# stages developing - spent

mature.weights <- dat %>%
  dplyr::filter(maturity_table_3 %in% 2:5,
         Age %in% 4:10,
         sex.code %in% 1:2)

# check weight distribution
ggplot(mature.weights, aes(weight)) +
  geom_histogram(fill = "dark grey", color = "black") +
  facet_grid(maturity_table_3~Age, scales = "free")

# few individuals at some age-maturity combinations - this may be 
# important to understanding some of the age class dynamics
# many developing age 4, few spawning
# few developing age 6+

# log transform weight
mature.weights$log.weight <- log(mature.weights$weight)

# check for NAs (age 7 below is plotting as no data)
check <- is.na(mature.weights$log.weight)
sum(check)

dplyr::filter(mature.weights, Age == 7)
mature.weights[which.min(mature.weights$log.weight),] # one value of weight = 0!!

mature.weights <- mature.weights %>%
  dplyr::filter(weight > 0)

# need to scale weight by age
mature.weights <- plyr::ddply(mature.weights, "Age", transform, sc.weight = scale(log.weight))

# check
ggplot(mature.weights, aes(log.weight, sc.weight)) +
  geom_point() +
  facet_wrap(~Age, scales = "free")

#  model the progression of weight for each year class through time

# assign year class
levels(mature.weights$year)
mature.weights$year <- as.numeric(as.character(mature.weights$year))

mature.weights$year.class <- mature.weights$year - mature.weights$Age

# plot to check
ggplot(mature.weights, aes(Age)) +
  geom_histogram() +
  facet_wrap(~year.class)

# begin with an analysis of SST and density-dependent effects on weight-at-age
# not using a cohort approach for now!

# load covariates

d1 <- read.csv("./data/western.goa.sst.csv")
d2 <- read.csv("./data/pollock_SSB_2020_SAFE.csv")

# add to mature weights (and simplify)

all.dat <- mature.weights %>%
  select(-DateTime, -Sex)

all.dat <- left_join(all.dat, d1)
all.dat <- left_join(all.dat, d2)
str(all.dat)

library(fRegression)

#plot

ggplot(all.dat, aes(apr.jul.wSST, sc.weight, colour=as.factor(maturity_table_3))) + geom_point() +
  facet_wrap(~Age) + geom_smooth(method = "lm")

#apr-jul temps
ggplot(all.dat[which(all.dat$sex.code==1),], aes(apr.jul.wSST, sc.weight, colour=as.factor(maturity_table_3))) + geom_point() +
  facet_wrap(~Age, scales="free") + geom_smooth(method = "lm")

ggplot(all.dat[which(all.dat$sex.code==2),], aes(apr.jul.wSST, sc.weight, colour=as.factor(maturity_table_3))) + geom_point() +
  facet_wrap(~Age, scales="free") + geom_smooth(method = "lm")

#nov-feb temps
ggplot(all.dat[which(all.dat$sex.code==1),], aes(nov.feb.wSST, sc.weight, colour=as.factor(maturity_table_3))) + geom_point() +
  facet_wrap(~Age, scales="free") + geom_smooth(method = "lm")

ggplot(all.dat[which(all.dat$sex.code==2),], aes(nov.feb.wSST, sc.weight, colour=as.factor(maturity_table_3))) + geom_point() +
  facet_wrap(~Age, scales="free") + geom_smooth(method = "lm")


#how many samples in each maturity code in each yr?

table(all.dat$sex.code, all.dat$maturity_table_3, all.dat$year)

#look at males
ggplot(all.dat[which(all.dat$sex.code==1),], aes(maturity_table_3)) + geom_bar() + facet_wrap(~year)

#look at females
ggplot(all.dat[which(all.dat$sex.code==2),], aes(maturity_table_3)) + geom_bar() + facet_wrap(~year)

#not too many in each year but across yrs should be ok







