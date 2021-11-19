#================================================================================
#Size-at-age models
#================================================================================

#data is loaded in load_explor_size-at-age.R
library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(gamm4)
theme_set(theme_bw())

#let's split the data out into age classes

#starts at age 4
age4dat <- all.dat[which(all.dat$Age==4),]
age5dat <- all.dat[which(all.dat$Age==5),]
age6dat <- all.dat[which(all.dat$Age==6),]
age7dat <- all.dat[which(all.dat$Age==7),]
age8dat <- all.dat[which(all.dat$Age==8),]
age9dat <- all.dat[which(all.dat$Age==9),]
age10dat <- all.dat[which(all.dat$Age==10),]

#models=====================================================================
#start by trying same models as for EBS

#age 4 models-------

mod4 <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + t2(Longitude, Latitude) +
                s(julian, k=4) + sex.code:maturity_table_3,
              random=~(1|year/Haul), data=age4dat)

#age 5 models-------

#age 6 models-------

#age 7 models-------

#age 8 models-------

#age 9 models-------

#age 10 models-------





