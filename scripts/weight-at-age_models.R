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

all.dat$sex.code <- as.factor(all.dat$sex.code)
all.dat$maturity_table_3 <- as.factor(all.dat$maturity_table_3)

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
#these models are using nov-feb SST, will re-do with spring SST too

#NO LAT x LONG in these models because these are sampled during spawning, so fish
#from various places came together into one big spawning aggregation

#models are rank deficient (seems like too few DF) when both sexes modeled together with a sex : maturity interaction
#SO we will do age-specific sex-specific models

#age 4 models-------

table(age4dat$sex.code, age4dat$maturity_table_3)

#males
mod4male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                s(julian, k=4) + maturity_table_3,
              random=~(1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
gam.check(mod4male$gam)
summary(mod4male$gam)
summary(mod4male$mer)

plot(mod4male$gam)

#females
mod4fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    s(julian, k=4) + maturity_table_3,
                  random=~(1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
gam.check(mod4fem$gam)
summary(mod4fem$gam)
summary(mod4fem$mer)

plot(mod4fem$gam)


ggplot(age4dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 5 models-------

table(age5dat$sex.code, age5dat$maturity_table_3)

#males
mod5male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    s(julian, k=4) + maturity_table_3,
                  random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
gam.check(mod5male$gam)
summary(mod5male$gam)
summary(mod5male$mer)

plot(mod5male$gam)

#females
mod5fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   s(julian, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
gam.check(mod5fem$gam)
summary(mod5fem$gam)
summary(mod5fem$mer)

plot(mod5fem$gam)

ggplot(age5dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 6 models-------

table(age6dat$sex.code, age6dat$maturity_table_3)

#males
mod6male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    s(julian, k=4) + maturity_table_3,
                  random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
gam.check(mod6male$gam)
summary(mod6male$gam)
summary(mod6male$mer)

plot(mod6male$gam)

#females
mod6fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   s(julian, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
gam.check(mod6fem$gam)
summary(mod6fem$gam)
summary(mod6fem$mer)

plot(mod6fem$gam)

ggplot(age6dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 7 models-------

table(age7dat$sex.code, age7dat$maturity_table_3)

#males
mod7male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    s(julian, k=4) + maturity_table_3,
                  random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
gam.check(mod7male$gam)
summary(mod7male$gam)
summary(mod7male$mer)

plot(mod7male$gam)

#females
mod7fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   s(julian, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
gam.check(mod7fem$gam)
summary(mod7fem$gam)
summary(mod7fem$mer)

plot(mod7fem$gam)

ggplot(age7dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()


#age 8 models-------

table(age8dat$sex.code, age8dat$maturity_table_3)

#males
mod8male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    s(julian, k=4) + maturity_table_3,
                  random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
gam.check(mod8male$gam)
summary(mod8male$gam)
summary(mod8male$mer)

plot(mod8male$gam)

#females
mod8fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   s(julian, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
gam.check(mod8fem$gam)
summary(mod8fem$gam)
summary(mod8fem$mer)

plot(mod8fem$gam)

ggplot(age8dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 9 models-------

table(age9dat$sex.code, age9dat$maturity_table_3)

#males
mod9male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    s(julian, k=4) + maturity_table_3,
                  random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])
gam.check(mod9male$gam)
summary(mod9male$gam)
summary(mod9male$mer)

plot(mod9male$gam)

#females
mod9fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   s(julian, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
gam.check(mod9fem$gam)
summary(mod9fem$gam)
summary(mod9fem$mer)

plot(mod9fem$gam)

ggplot(age9dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 10 models-------

table(age10dat$sex.code, age10dat$maturity_table_3)

#males
mod10male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    s(julian, k=4) + maturity_table_3,
                  random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==1),])
gam.check(mod10male$gam)
summary(mod10male$gam)
summary(mod10male$mer)

plot(mod10male$gam)

#females
mod10fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   s(julian, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
gam.check(mod10fem$gam)
summary(mod10fem$gam)
summary(mod10fem$mer)

plot(mod10fem$gam)

ggplot(age10dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()



