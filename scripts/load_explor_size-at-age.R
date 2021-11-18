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

#end data wrangling-----
