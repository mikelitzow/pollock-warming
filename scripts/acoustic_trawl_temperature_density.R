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


# get julian day
temp <- dat %>%
  select(DateTime) 

temp.more <- separate(temp, DateTime, into = c("date", "time"), sep = "\\s")

temp.more$date <- chron::dates(temp.more$date)

dat$julian <- lubridate::yday(temp.more$date)
dat$year <- lubridate::year(temp.more$date)
unique(dat$year)

hist(dat$julian) # looks right!



ggplot(dat, aes(Age, weight)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) +
  facet_wrap(~year)

ggsave("./figs/acoustic_trawl_age_weight_all_fish.png", width = 8, height = 8, units = 'in')

# becomes very age-truncated in recent years!


ggplot(dplyr::filter(dat, Age %in% 4:10), aes(Age, weight, color = as.factor(as.character(year)))) +
  geom_point(stroke = 0, size=0) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) +
  coord_cartesian(ylim = c(200, 2200)) + 
  scale_color_viridis_d()

ggsave("./figs/acoustic_weight_at_age_by_year.png", width = 7, height = 4, units = 'in')

ggplot(dat, aes(Age)) +
  geom_histogram(bins = 23) +
  facet_wrap(~year, scale = "free_y")

ggsave("./figs/acoustic_age_histogram_all_fish.png", width = 8, height = 6, units = 'in')
# age structure collapsed (?!)

# add sex 
dat$sex.code <- if_else(dat$Sex == 1, 1,
                        if_else(dat$Sex == 2, 2,
                                if_else(dat$Sex == "Male", 1, 
                                        ifelse(dat$Sex == "Female", 2, NA))))

# not many spawner samples in some years - 2000-2003, 2009, 2012

# calculate the proportion of developing / pre-spawning / spawning / spent 
# that are in the dominant year class each year (and sample size!)

mature <- dat %>%
  filter(maturity_table_3 %in% 2:5,
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
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(x = "year")

ggsave("./figs/proportion_mature_developing-spent_by_year.png", width = 8, height = 6, units = 'in')

# now need to scale weights by age and maturity stage

# first, clean up - only known sex, known age, age 4-10,
# stages developing - spent

mature.weights <- dat %>%
  filter(maturity_table_3 %in% 2:5,
         Age %in% 4:10,
         sex.code %in% 1:2)

# check weight distribution
ggplot(mature.weights, aes(weight)) +
  geom_histogram(fill = "dark grey", color = "black") +
  facet_grid(maturity_table_3~Age, scales = "free")

ggsave("./figs/acoustic_age_stage_histograms.png", width = 8, height = 6, units = 'in')

# few individuals at some age-maturity combinations - this may be 
# important to understanding some of the age class dynamics
# many developing age 4, few spawning
# few developing age 6+

# log transform weight
mature.weights$log.weight <- log(mature.weights$weight)

# check for NAs (age 7 below is plotting as no data)
check <- is.na(mature.weights$log.weight)
sum(check)

filter(mature.weights, Age == 7)
mature.weights[which.min(mature.weights$log.weight),] # one value of weight = 0!!

mature.weights <- mature.weights %>%
  filter(weight > 0)

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

sst <- read.csv("./data/western.goa.sst.csv")
ssb <- read.csv("./data/pollock_SSB_2020_SAFE.csv")

# add to mature weights (and simplify)

all.dat <- mature.weights %>%
  select(-DateTime, -Sex)

all.dat <- left_join(all.dat, sst)
all.dat <- left_join(all.dat, ssb)
str(all.dat)

all.dat$maturity <- as.factor(as.character(all.dat$maturity_table_3))
all.dat$sex.code <- as.factor(all.dat$sex.code)

ggplot(all.dat, aes(nov.feb.wSST, sc.weight)) +
  geom_point() +
  facet_wrap(~Age) +
  geom_smooth(method = "gam", formula = y ~ s(x, k=4))

ggplot(all.dat, aes(year, sc.weight)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k=4))

ggplot(all.dat, aes(Age, sc.weight)) +
  geom_point() +
  facet_wrap(~year.class) +
  geom_smooth(method = "gam", formula = y ~ s(x, k=4))

ggplot(all.dat, aes(nov.feb.wSST, sc.weight)) +
  geom_point() +
  facet_wrap(~year.class) +
  geom_smooth(method = "gam", formula = y ~ s(x, k=4)) +
  geom_hline(yintercept = 0)

ggplot(dplyr::filter(all.dat, Age %in% 4:10, year.class %in% 1979:2013),
       aes(Age, weight, color = as.factor(as.character(year.class)))) +
  geom_point(stroke = 0, size=0) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) +
  coord_cartesian(ylim = c(200, 2200)) + 
  scale_color_viridis_d()

ggsave("./figs/acoustic_weight_at_age_by_year_class.png", width = 7, height = 4, units = 'in')

# question - do density-dependence and SST explain changing weight at age?
# begin model selection
# using year class as a random term in this first iteration;
# should also consider adding haul nested in year as a random term
library(mgcv)

# add lagged spring/summer temps
sst <- read.csv("./data/western.goa.sst.csv")
head(sst)

lagged.temp <- sst %>%
  select(-nov.feb.wSST) %>%
  mutate(year=year+1)

names(lagged.temp)[2] <- "apr.jul.wSST.lag1"

all.dat <- left_join(all.dat, lagged.temp)

m1 <- gamm(log.weight ~  maturity + sex.code + Age + s(nov.feb.wSST, k = 4) + s(poll.SSB.2020, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m1$gam)

# plot(m1$gam)

# do sst and density effects differ over time? I'll test with some ad-hoc era divisions
all.dat$era <- as.factor(if_else(all.dat$year < 2005, 1,
                       if_else(all.dat$year %in% 2006:2015, 2, 3)))

m2 <- gamm(log.weight ~  maturity + sex.code + Age + te(nov.feb.wSST, year, k = 4) + s(poll.SSB.2020, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m2$gam)
vis.gam(m2$gam, view=c("year", "nov.feb.wSST"),
        plot.type='contour', color='topo', type = "response")
# plot(m2$gam)

m3 <- gamm(log.weight ~  maturity + sex.code + Age + s(nov.feb.wSST, k = 4) + te(poll.SSB.2020, year, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m3$gam)

vis.gam(m3$gam, view=c("year", "poll.SSB.2020"),
        plot.type='contour', color='topo', type = "response")

m4 <- gamm(log.weight ~  maturity + sex.code + Age + te(nov.feb.wSST, year, k = 4) + te(poll.SSB.2020, year, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m4$gam)

png("./figs/acoustic_m4_vis.gam.png", 8, 4, units="in", res=300)
par(mfrow=c(1,2))

vis.gam(m4$gam, view=c("year", "poll.SSB.2020"),
        plot.type='contour', color='topo', type = "response")

vis.gam(m4$gam, view=c("year", "nov.feb.wSST"),
        plot.type='contour', color='topo', type = "response")

dev.off()

m5 <- gamm(log.weight ~  maturity + sex.code + Age + te(nov.feb.wSST, poll.SSB.2020, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m5$gam)

vis.gam(m5$gam, view=c("nov.feb.wSST", "poll.SSB.2020"),
        plot.type='contour', color='topo', type = "response")

# compare with april-july sst

m6 <- gamm(log.weight ~  maturity + sex.code + Age + s(apr.jul.wSST.lag1, k = 4) + s(poll.SSB.2020, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m6$gam)

# plot(m6$gam)

m7 <- gamm(log.weight ~  maturity + sex.code + Age + te(apr.jul.wSST.lag1, year, k = 4) + s(poll.SSB.2020, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m7$gam)

vis.gam(m7$gam, view=c("year", "apr.jul.wSST.lag1"),
        plot.type='contour', color='topo', type = "response")

# plot(m7$gam)

m8 <- gamm(log.weight ~  maturity + sex.code + Age + s(apr.jul.wSST.lag1, k = 4) + te(poll.SSB.2020, year, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m8$gam)

vis.gam(m8$gam, view=c("year", "poll.SSB.2020"),
        plot.type='contour', color='topo', type = "response")

m9 <- gamm(log.weight ~  maturity + sex.code + Age + te(apr.jul.wSST.lag1, year, k = 4) + te(poll.SSB.2020, year, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m9$gam)

png("./figs/acoustic_m9_vis.gam.png", 8, 4, units="in", res=300)
par(mfrow=c(1,2))

vis.gam(m9$gam, view=c("year", "poll.SSB.2020"),
        plot.type='contour', color='topo', type = "response")

vis.gam(m9$gam, view=c("year", "apr.jul.wSST.lag1"),
        plot.type='contour', color='topo', type = "response")

dev.off()

m10 <- gamm(log.weight ~  maturity + sex.code + Age + te(apr.jul.wSST.lag1, poll.SSB.2020, k = 4),
           random = list(year.class=~1), data=all.dat) 

summary(m10$gam)

vis.gam(m10$gam, view=c("apr.jul.wSST.lag1", "poll.SSB.2020"),
        plot.type='contour', color='topo', type = "response")

mod.out <- MuMIn::AICc(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)

mod.out$delta.AICc <- mod.out$AICc - min(mod.out$AICc)

mod.out$formula <- c(as.character(m1$gam$formula)[3],
                     as.character(m2$gam$formula)[3],
                     as.character(m3$gam$formula)[3],
                     as.character(m4$gam$formula)[3],
                     as.character(m5$gam$formula)[3],
                     as.character(m6$gam$formula)[3],
                     as.character(m7$gam$formula)[3],
                     as.character(m8$gam$formula)[3],
                     as.character(m9$gam$formula)[3],
                     as.character(m10$gam$formula)[3])

mod.out <- mod.out %>%
  select(formula, df, AICc, delta.AICc) %>%
  arrange(delta.AICc)

mod.out

write.csv(mod.out, "./output/acoustic_trawl_first_model_comparison.csv")


## I'm not sure that any of the above makes sense!
## the tensor products on year impose a pure time effect, not accounted for by SST or SSB,
## that we don't want at all!

# try a model-fitting approach with different possible changepoints

range(all.dat$year)
.2*length(1986:2020)

mod.out <- NA

for(i in 2000:2005){
  
  all.dat$era <- as.factor(if_else(all.dat$year <= i, 1, 2))
  
  mod <- gamm(log.weight ~  maturity + sex.code + Age +
                s(nov.feb.wSST, k = 4, by = era) + s(poll.SSB.2020, k = 4, by = era),
              random = list(year.class=~1), data=all.dat) 
  
  mod.out <- rbind(mod.out, data.frame(threshold = i,
                                       SST = "winter",
                                       AICc = MuMIn::AICc(mod)))
  
}

# and spring/summer sst

for(i in 2000:2005){
  
  all.dat$era <- as.factor(if_else(all.dat$year <= i, 1, 2))
  
  mod <- gamm(log.weight ~  maturity + sex.code + Age +
                s(apr.jul.wSST.lag1, k = 4, by = era) + s(poll.SSB.2020, k = 4, by = era),
              random = list(year.class=~1), data=all.dat) 
  
  mod.out <- rbind(mod.out, data.frame(threshold = i,
                                       SST = "spring/summer",
                                       AICc = MuMIn::AICc(mod)))
  
}


# and stationary models for comparison

st.spring <- gamm(log.weight ~  maturity + sex.code + Age +
                    s(apr.jul.wSST.lag1, k = 4) + s(poll.SSB.2020, k = 4),
                  random = list(year.class=~1), data=all.dat) 

st.winter <- gamm(log.weight ~  maturity + sex.code + Age +
                    s(nov.feb.wSST, k = 4) + s(poll.SSB.2020, k = 4),
                  random = list(year.class=~1), data=all.dat) 

# add palette 

cb <- cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(mod.out, aes(threshold, AICc, color = SST)) +
  geom_line() +
  scale_color_manual(values = cb[c(2,4)]) +
  geom_hline(yintercept = c(MuMIn::AICc(st.spring), MuMIn::AICc(st.winter)),
             lty = 2, color = cb[c(2,4)])

ggsave("./figs/acoustic_threshold_AICc.png", width = 6, height = 4, units = "in")

# plot best model!

mod.out[which.min(mod.out$AICc),]

all.dat$era <- as.factor(if_else(all.dat$year <= 2000, 1, 2))

mod <- gamm(log.weight ~  maturity + sex.code + Age +
              s(nov.feb.wSST, k = 4, by = era) + s(poll.SSB.2020, k = 4, by = era),
            random = list(year.class=~1), data=all.dat) 

summary(mod$gam)

png("./figs/acoustic_winter.sst_2000_nonstationary_gam.png", 6, 6, 'in', res=300)
plot(mod$gam, pages=1)
dev.off()

plot.dat <- all.dat %>%
  group_by(year) %>%
  summarise(winter.sst = mean(nov.feb.wSST),
            ssb = mean(poll.SSB.2020)) %>%
  mutate(era = if_else(year <= 2000, "1986-2000", "2001-2020")) 

ggplot(plot.dat, aes(winter.sst)) +
  geom_histogram(bins=5, fill= "grey", color="black") +
  facet_grid(~era)

ggsave("./figs/acoustic_winter_sst_era_histograms.png", width = 4, height = 4, units = 'in')

ggplot(plot.dat, aes(ssb)) +
  geom_histogram(bins=5, fill= "grey", color="black") +
  facet_grid(~era)

ggsave("./figs/acoustic_ssb_era_histograms.png", width = 4, height = 4, units = 'in')

ggplot(plot.dat, aes(ssb, winter.sst)) +
  geom_point() + 
  facet_wrap(~era)

ggsave("./figs/acoustic_ssb_sst_scatter_era.png", width = 4, height = 3, units = 'in')
