# load and process age-0 pollock data from beach seines

library(tidyverse)
theme_set(theme_bw())

dat <- read.csv("./data/cod.pollock.lengths.2006.2021.csv")

dat <- dat %>% 
  filter(species == "poll")

head(dat)

# examine length histograms to separate age-0 and age-1
ggplot(dat, aes(length)) +
  geom_histogram(bins = 80, fill = "dark grey", color = "black")

# looks like a break of ~100 mm would separate age-0 from age-1

# plot by julian day
ggplot(dat, aes(julian, length)) +
  geom_point() +
  geom_hline(yintercept = 112, lty = 2)

# yes, < 112 mm will segregate age-0'

# remove age-1

dat <- dat %>%
  filter(length < 112)

# check
ggplot(dat, aes(length)) +
  geom_histogram(bins = 80, fill = "dark grey", color = "black")


# ready to analyze!!
