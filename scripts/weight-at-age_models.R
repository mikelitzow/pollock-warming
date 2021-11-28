#================================================================================
#Size-at-age models
#================================================================================

#data is loaded in load_explor_size-at-age.R
library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(gamm4)
library(MuMIn)
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

#NO JULIAN DAY - has potential to be collinear with maturity AND not much variation in julian day
#vast majority of samples between 70-90th day (unlike bottom trawl survey)
ggplot(all.dat, aes(julian)) + geom_histogram() + facet_wrap(~maturity_table_3)

#age 4 models-------

table(age4dat$sex.code, age4dat$maturity_table_3)

#males
mod4male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                maturity_table_3,
              random=~(1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
gam.check(mod4male$gam)
summary(mod4male$gam)
summary(mod4male$mer)

plot(mod4male$gam)

mod4maleAICc <- AICc(mod4male$mer)

#females
mod4fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   maturity_table_3,
                  random=~(1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
gam.check(mod4fem$gam)
summary(mod4fem$gam)
summary(mod4fem$mer)

plot(mod4fem$gam)

mod4femAICc <- AICc(mod4fem$mer)

ggplot(age4dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 5 models-------

table(age5dat$sex.code, age5dat$maturity_table_3)

#males
mod5male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
gam.check(mod5male$gam)
summary(mod5male$gam)
summary(mod5male$mer)

plot(mod5male$gam)

mod5maleAICc <- AICc(mod5male$mer)

#females
mod5fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
gam.check(mod5fem$gam)
summary(mod5fem$gam)
summary(mod5fem$mer)

plot(mod5fem$gam)

mod5femAICc <- AICc(mod5fem$mer)

ggplot(age5dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 6 models-------

table(age6dat$sex.code, age6dat$maturity_table_3)

#males
mod6male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                     maturity_table_3,
                  random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
gam.check(mod6male$gam)
summary(mod6male$gam)
summary(mod6male$mer)

plot(mod6male$gam)

mod6maleAICc <- AICc(mod6male$mer)

#females
mod6fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    maturity_table_3,
                 random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
gam.check(mod6fem$gam)
summary(mod6fem$gam)
summary(mod6fem$mer)

plot(mod6fem$gam)

mod6femAICc <- AICc(mod6fem$mer)

ggplot(age6dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 7 models-------

table(age7dat$sex.code, age7dat$maturity_table_3)

#males
mod7male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                     maturity_table_3,
                  random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
gam.check(mod7male$gam)
summary(mod7male$gam)
summary(mod7male$mer)

plot(mod7male$gam)

mod7maleAICc <- AICc(mod7male$mer)

#females
mod7fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
gam.check(mod7fem$gam)
summary(mod7fem$gam)
summary(mod7fem$mer)

plot(mod7fem$gam)

mod7femAICc <- AICc(mod7fem$mer)

ggplot(age7dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()


#age 8 models-------

table(age8dat$sex.code, age8dat$maturity_table_3)

#males
mod8male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                     maturity_table_3,
                  random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
gam.check(mod8male$gam)
summary(mod8male$gam)
summary(mod8male$mer)

plot(mod8male$gam)

mod8maleAICc <- AICc(mod8male$mer)

#females
mod8fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
gam.check(mod8fem$gam)
summary(mod8fem$gam)
summary(mod8fem$mer)

plot(mod8fem$gam)

mod8femAICc <- AICc(mod8fem$mer)

ggplot(age8dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 9 models-------

table(age9dat$sex.code, age9dat$maturity_table_3)

#males
mod9male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])
gam.check(mod9male$gam)
summary(mod9male$gam)
summary(mod9male$mer)

plot(mod9male$gam)
mod9maleAICc <- AICc(mod9male$mer)

#females
mod9fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    maturity_table_3,
                 random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
gam.check(mod9fem$gam)
summary(mod9fem$gam)
summary(mod9fem$mer)

plot(mod9fem$gam)

mod9femAICc <- AICc(mod9fem$mer)

ggplot(age9dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()

#age 10 models-------

table(age10dat$sex.code, age10dat$maturity_table_3)

#males
mod10male <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                     maturity_table_3,
                  random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==1),])
gam.check(mod10male$gam)
summary(mod10male$gam)
summary(mod10male$mer)

plot(mod10male$gam)

mod10maleAICc <- AICc(mod10male$mer)

#females
mod10fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    maturity_table_3,
                 random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
gam.check(mod10fem$gam)
summary(mod10fem$gam)
summary(mod10fem$mer)

plot(mod10fem$gam)

mod10femAICc <- AICc(mod10fem$mer)

ggplot(age10dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()




#repeat models w april-jul sst==========================================================


#reload sst data
d1 <- read.csv("./data/western.goa.sst.csv")
sst_lagged <- d1

sst_lagged$prevyr_apr.jul.wSST <- NA
k<-1
for(k in 1:length(sst_lagged$year)){
  if(k>1){
    sst_lagged$prevyr_apr.jul.wSST[k] <- sst_lagged$apr.jul.wSST[k-1] 
  }
} #works now join

sstjoin <- sst_lagged[,c(1,4)]

age4dat <- left_join(age4dat, sstjoin, by="year")
age5dat <- left_join(age5dat, sstjoin, by="year")
age6dat <- left_join(age6dat, sstjoin, by="year")
age7dat <- left_join(age7dat, sstjoin, by="year")
age8dat <- left_join(age8dat, sstjoin, by="year")
age9dat <- left_join(age9dat, sstjoin, by="year")
age10dat <- left_join(age10dat, sstjoin, by="year")




#age 4 models-------

#males
spr4male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
gam.check(spr4male$gam)
summary(spr4male$gam)
summary(spr4male$mer)

plot(spr4male$gam)

spr4maleAICc <- AICc(spr4male$mer)

#females
spr4fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
gam.check(spr4fem$gam)
summary(spr4fem$gam)
summary(spr4fem$mer)

plot(spr4fem$gam)

spr4femAICc <- AICc(spr4fem$mer)

#age 5 models-------

#males
spr5male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
gam.check(spr5male$gam)
summary(spr5male$gam)
summary(spr5male$mer)

plot(spr5male$gam)

spr5maleAICc <- AICc(spr5male$mer)

#females
spr5fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
gam.check(spr5fem$gam)
summary(spr5fem$gam)
summary(spr5fem$mer)

plot(spr5fem$gam)

spr5femAICc <- AICc(spr5fem$mer)

#age 6 models-------

#males
spr6male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
gam.check(spr6male$gam)
summary(spr6male$gam)
summary(spr6male$mer)

plot(spr6male$gam)

spr6maleAICc <- AICc(spr6male$mer)

#females
spr6fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
gam.check(spr6fem$gam)
summary(spr6fem$gam)
summary(spr6fem$mer)

plot(spr6fem$gam)

spr6femAICc <- AICc(spr6fem$mer)

#age 7 models-------

#males
spr7male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
gam.check(spr7male$gam)
summary(spr7male$gam)
summary(spr7male$mer)

plot(spr7male$gam)

spr7maleAICc <- AICc(spr7male$mer)

#females
spr7fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
gam.check(spr7fem$gam)
summary(spr7fem$gam)
summary(spr7fem$mer)

plot(spr7fem$gam)

spr7femAICc <- AICc(spr7fem$mer)


#age 8 models-------


#males
spr8male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
gam.check(spr8male$gam)
summary(spr8male$gam)
summary(spr8male$mer)

plot(spr8male$gam)

spr8maleAICc <- AICc(spr8male$mer)

#females
spr8fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
gam.check(spr8fem$gam)
summary(spr8fem$gam)
summary(spr8fem$mer)

plot(spr8fem$gam)

spr8femAICc <- AICc(spr8fem$mer)


#age 9 models-------

#males
spr9male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])
gam.check(spr9male$gam)
summary(spr9male$gam)
summary(spr9male$mer)

plot(spr9male$gam)

spr9maleAICc <- AICc(spr9male$mer)

#females
spr9fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
gam.check(spr9fem$gam)
summary(spr9fem$gam)
summary(spr9fem$mer)

plot(spr9fem$gam)

spr9femAICc <- AICc(spr9fem$mer)

#age 10 models-------

#males
spr10male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                     maturity_table_3,
                   random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==1),])
gam.check(spr10male$gam)
summary(spr10male$gam)
summary(spr10male$mer)

plot(spr10male$gam)

spr10maleAICc <- AICc(spr10male$mer)

#females
spr10fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
gam.check(spr10fem$gam)
summary(spr10fem$gam)
summary(spr10fem$mer)

spr10femAICc <- AICc(spr10fem$mer)

plot(spr10fem$gam)

spr4maleAICc
mod4maleAICc

spr4femAICc
mod4femAICc

spr5maleAICc
mod5maleAICc

spr5femAICc
mod5femAICc

spr6maleAICc
mod6maleAICc

spr6femAICc
mod6femAICc

spr7maleAICc
mod7maleAICc

spr7femAICc
mod7femAICc

spr8maleAICc
mod8maleAICc

spr8femAICc
mod8femAICc

spr9maleAICc
mod9maleAICc

spr9femAICc
mod9femAICc

spr10maleAICc
mod10maleAICc

spr10femAICc
mod10femAICc
