# create plot of all time series for attribution / adaptation paper:
# recrtuitment, weight at age, spawning age structure diversity

library(tidyverse)
library(gamm4)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


d1 <- read.csv("./data/shannon.age.diversity.acoustic.trawl.data.csv",
               row.names = 1)

# clean up
d1 <- d1 %>%
  rename(Year = year,
         Estimate = shannon) %>%
  mutate(LCI = NA,
         UCI = NA,
         variable = "Spawning stock age diversity")

d2 <- read.csv("./data/cod_pollock_assessment_2020_SAFEs.csv")

# clean up
d2 <- d2 %>%
  select(year, pollR0.2020, pollR0.2020.LCI, pollR0.2020.UCI) %>%
  mutate(year = lead(year), # leading one year to align with age1 recruitment
         pollR0.2020 = log(pollR0.2020),
         pollR0.2020.LCI = log(pollR0.2020.LCI),
         pollR0.2020.UCI = log(pollR0.2020.UCI),
         variable = "Log age1 recruits") %>% 
  rename(Year = year,
         Estimate = pollR0.2020,
         LCI = pollR0.2020.LCI,
         UCI = pollR0.2020.UCI,
         Year = year) %>%
  na.omit() # dropping last row of NAs

d3a <- read.csv("./data/cohort_weight_age.csv")

# fit model to predict values for each year
# make sure we're coding factors
d3a <- d3a %>%
  mutate(maturity_fac = as.factor(maturity_table_3),
         cohort_fac = as.factor(cohort),
         year_fac = as.factor(year))

mod <- gamm4(sc.weight ~  year_fac + maturity_fac,
             random=~(1|year/Haul) + (1|cohort_fac), data=d3a)

summary(mod$gam)

# predict values for plot
new_dat <- data.frame(year_fac = unique(d3$year_fac),
                      maturity_fac = 4) # predicting for spawning weight

pred <- predict(mod$gam, newdata = new_dat, se.fit = T)

d3 <- data.frame(Year = new_dat$year_fac,
                       Estimate = pred$fit, 
                       LCI = pred$fit - 1.96*pred$se.fit,
                       UCI = pred$fit + 1.96*pred$se.fit,
                       variable = "Log spawning weight anomaly") 

d3 <- d3 %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  arrange(Year)

# add NAs to make plot work
xtra <- data.frame(Year = c(1999, 2001, 2011),
                   Estimate = NA,
                   LCI = NA,
                   UCI = NA,
                   variable = "Log spawning weight anomaly")
  
d3 <- rbind(d3, xtra)


d4 <- read.csv("./data/regional_north_pacific_ersst_time_series.csv")

# clean up
d4 <- d4 %>%
  filter(region == "Gulf_of_Alaska") %>%
  select(year, annual.unsmoothed, winter.unsmoothed) %>%
  rename(Year = year,
         'Annual mean SST (°C)' = annual.unsmoothed,
         'Winter mean (°C)' = winter.unsmoothed) %>%
  pivot_longer(cols = -Year, 
               values_to = "Estimate",
               names_to = "variable") %>%
  mutate(LCI = NA,
         UCI = NA) %>%
  select(Year, Estimate, LCI, UCI, variable)

d5 <- read.csv("./data/GOA_annual_FAR_Risk_Ratio_with_uncertainty.csv")

# combine and save
all_dat <- rbind(d1, d2, d3, d4, d5)
all_dat$Year <- as.integer(all_dat$Year)
write.csv(all_dat, "./data/attribution_and_adaptation_data.csv", 
          row.names = F)

# and plot
plot_order <- data.frame(variable = unique(all_dat$variable),
                         order = c(6,4,5,1,NA,2,3)) %>%
  filter(variable != "Winter mean (°C)")

all_dat <- left_join(all_dat, plot_order)

all_dat$variable <- reorder(all_dat$variable, all_dat$order)
temp.color = cb[6]

ggplot(filter(all_dat, 
                      variable != "Winter mean (°C)", 
                      Year >= 1970)) +
  aes(Year, Estimate) +
  geom_line(color = temp.color, size = 0.3) +
  geom_point(size = 0.5, color = temp.color) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), alpha = 0.5, color = temp.color) +
  facet_wrap(~variable, scales = "free_y", ncol = 2, dir = "v") +
  theme(axis.title.x = element_blank()) +
  ylab("Value")

ggsave("./figs/attribution_adaptation_time_series_plot.png",
       width = 6, height = 6, units = 'in')

