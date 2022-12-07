library(tidyverse)

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
