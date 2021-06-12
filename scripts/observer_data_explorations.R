library(tidyverse)

dat <- read.csv("./data/observer_data.csv")

unique(dat$SPECIES)
head(dat)

aged <- dat %>%
  select(YEAR, AGE, SEX, WEIGHT) %>%
  filter(!is.na(AGE))

nrow(aged)

ggplot(aged, aes(AGE, WEIGHT)) +
  geom_point() + 
  facet_wrap(~YEAR) +
  geom_smooth(method="gam", color="red", formula = y ~ s(x, k = 5))

tail(aged)
