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
gam.check(mod4$gam)
summary(mod4$gam)
summary(mod4$mer)

plot(mod4$gam)

#age 5 models-------

mod5 <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + t2(Longitude, Latitude) +
                s(julian, k=4) + sex.code:maturity_table_3,
              random=~(1|year/Haul), data=age5dat)
gam.check(mod5$gam)
summary(mod5$gam)
summary(mod5$mer)

plot(mod5$gam)


#age 6 models-------

mod6 <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + t2(Longitude, Latitude) +
                s(julian, k=4) + sex.code:maturity_table_3,
              random=~(1|year/Haul), data=age6dat)
gam.check(mod6$gam)
summary(mod6$gam)
summary(mod6$mer)

plot(mod6$gam)


#age 7 models-------

mod7 <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + t2(Longitude, Latitude) +
                s(julian, k=4) + sex.code:maturity_table_3,
              random=~(1|year/Haul), data=age7dat)
gam.check(mod7$gam)
summary(mod7$gam)
summary(mod7$mer)

plot(mod7$gam)


#age 8 models-------

mod8 <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + t2(Longitude, Latitude) +
                s(julian, k=4) + sex.code:maturity_table_3,
              random=~(1|year/Haul), data=age8dat)
gam.check(mod8$gam)
summary(mod8$gam)
summary(mod8$mer)

plot(mod8$gam)


#age 9 models-------

mod9 <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + t2(Longitude, Latitude) +
                s(julian, k=4) + sex.code:maturity_table_3,
              random=~(1|year/Haul), data=age9dat)
gam.check(mod9$gam)
summary(mod9$gam)
summary(mod9$mer)

plot(mod9$gam)


#age 10 models-------

mod10 <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + t2(Longitude, Latitude) +
                s(julian, k=4) + sex.code:maturity_table_3,
              random=~(1|year/Haul), data=age10dat)
gam.check(mod10$gam)
summary(mod10$gam)
summary(mod10$mer)

plot(mod10$gam)




