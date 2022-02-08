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

plot(mod10male$gam) #looks linear

mod10maleAICc <- AICc(mod10male$mer)

#females
mod10fem <- gamm4(sc.weight ~ s(nov.feb.wSST, k=4) + 
                    maturity_table_3,
                 random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
gam.check(mod10fem$gam)
summary(mod10fem$gam)
summary(mod10fem$mer)

plot(mod10fem$gam) #looks linear

mod10femAICc <- AICc(mod10fem$mer)

ggplot(age10dat, aes(as.factor(maturity_table_3), sc.weight, colour=as.factor(sex.code))) + geom_boxplot()


#plot winter models=========

wmp4 <- visreg(mod4male$gam, "nov.feb.wSST", scale="response", rug=1, data=age4dat[which(age4dat$sex.code==1),])
wfp4 <- visreg(mod4fem$gam, "nov.feb.wSST", scale="response", rug=1, data=age4dat[which(age4dat$sex.code==2),])

wmp5 <- visreg(mod5male$gam, "nov.feb.wSST", scale="response", rug=1, data=age5dat[which(age5dat$sex.code==1),])
wfp5 <- visreg(mod5fem$gam, "nov.feb.wSST", scale="response", rug=1, data=age5dat[which(age5dat$sex.code==2),])

wmp6 <- visreg(mod6male$gam, "nov.feb.wSST", scale="response", rug=1, data=age6dat[which(age6dat$sex.code==1),])
wfp6 <- visreg(mod6fem$gam, "nov.feb.wSST", scale="response", rug=1, data=age6dat[which(age6dat$sex.code==2),])

wmp7 <- visreg(mod7male$gam, "nov.feb.wSST", scale="response", rug=1, data=age7dat[which(age7dat$sex.code==1),])
wfp7 <- visreg(mod7fem$gam, "nov.feb.wSST", scale="response", rug=1, data=age7dat[which(age7dat$sex.code==2),])

wmp8 <- visreg(mod8male$gam, "nov.feb.wSST", scale="response", rug=1, data=age8dat[which(age8dat$sex.code==1),])
wfp8 <- visreg(mod8fem$gam, "nov.feb.wSST", scale="response", rug=1, data=age8dat[which(age8dat$sex.code==2),])

wmp9 <- visreg(mod9male$gam, "nov.feb.wSST", scale="response", rug=1, data=age9dat[which(age9dat$sex.code==1),])
wfp9 <- visreg(mod9fem$gam, "nov.feb.wSST", scale="response", rug=1, data=age9dat[which(age9dat$sex.code==2),])

wmp10 <- visreg(mod10male$gam, "nov.feb.wSST", scale="response", rug=1, data=age10dat[which(age10dat$sex.code==1),])
wfp10 <- visreg(mod10fem$gam, "nov.feb.wSST", scale="response", rug=1, data=age10dat[which(age10dat$sex.code==2),])

library(visreg)
winplot <- visregList(wmp4, wfp4, 
                   wmp5, wfp5, 
                  wmp6, wfp6, 
                   wmp7, wfp7, 
                   wmp8, wfp8, 
                   wmp9, wfp9, 
                    wmp10, wfp10,  collapse=TRUE) #,  
                   # 
                   # labels=c("Age 8", "Age 9",  "Age 10", "Age 11", 
                   #          "Age 4", "Age 5", "Age 6", "Age 7",  
                   #          "Age 2", "Age 3"))
plot(winplot, rug=1) 

#op <- par(mfrow=c(7,2), mar=c(0.5,0.5,0.5,0.5))
op <- par(mfrow=c(7,2), mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
plot(wmp4)
plot(wfp4)
plot(wmp5)
plot(wfp5)
plot(wmp6)
plot(wfp6)
plot(wmp7)
plot(wfp7)
plot(wmp8)
plot(wfp8)
plot(wmp9)
plot(wfp9)
plot(wmp10)
plot(wfp10)

# mtext("Nov-Feb SST",side=1,line=0,outer=TRUE,cex=1.3)
# mtext("Scaled weight",side=2,line=0,outer=TRUE,cex=1.3,las=0)

mtext("Nov-Feb SST", side = 1, outer = TRUE, cex = 0.7, line = 2.2,  col = "grey20")
mtext("Scaled weight", side = 2, outer = TRUE, cex = 0.7, line = 2.2,
             col = "grey20")


#linear winter models-----------------------------
library(car)
#age 4 models-------

#males
lin4male <- lmer(sc.weight ~ nov.feb.wSST * 
                    maturity_table_3 + (1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])

summary(lin4male)
Anova(lin4male, type="III")



linwin4maleAICc <- AICc(lin4male)

#females
lin4fem <- lmer(sc.weight ~ nov.feb.wSST * 
                   maturity_table_3 + (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])

summary(lin4fem)
Anova(lin4fem, type="III")


lin4femnoint <- lmer(sc.weight ~ nov.feb.wSST + 
                  maturity_table_3 + (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])

summary(lin4femnoint)
Anova(lin4femnoint, type="II")


linwin4femAICc <- AICc(lin4femnoint)


#age 5 models-------

#males
lin5male <- lmer(sc.weight ~ nov.feb.wSST * 
                   maturity_table_3 + (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])

summary(lin5male)
Anova(lin5male, type="III") #interaction not sig so drop


lin5malenoint <- lmer(sc.weight ~ nov.feb.wSST + 
                   maturity_table_3 + (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])

summary(lin5malenoint)
Anova(lin5malenoint, type="II")


linwin5maleAICc <- AICc(lin5malenoint)

#females
lin5fem <- lmer(sc.weight ~ nov.feb.wSST * 
                  maturity_table_3 + (1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])

summary(lin5fem)
Anova(lin5fem, type="III")


linwin5femAICc <- AICc(lin5fem)


#age 6 models-------

#males
lin6male <- lmer(sc.weight ~ nov.feb.wSST * 
                   maturity_table_3 + (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])

summary(lin6male)
Anova(lin6male, type="III") #interaction not sig so drop


lin6malenoint <- lmer(sc.weight ~ nov.feb.wSST + 
                        maturity_table_3 + (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])

summary(lin6malenoint)
Anova(lin6malenoint, type="II")


linwin6maleAICc <- AICc(lin6malenoint)

#females
lin6fem <- lmer(sc.weight ~ nov.feb.wSST * 
                  maturity_table_3 + (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])

summary(lin6fem)
Anova(lin6fem, type="III") #interaction not sig so drop

lin6femnoint <- lmer(sc.weight ~ nov.feb.wSST + 
                  maturity_table_3 + (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])

summary(lin6femnoint)
Anova(lin6femnoint, type="II")

linwin6femAICc <- AICc(lin6femnoint)



#age 7 models-------

#males
lin7male <- lmer(sc.weight ~ nov.feb.wSST * 
                   maturity_table_3 + (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])

summary(lin7male)
Anova(lin7male, type="III") #interaction not sig so drop


lin7malenoint <- lmer(sc.weight ~ nov.feb.wSST + 
                        maturity_table_3 + (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])

summary(lin7malenoint)
Anova(lin7malenoint, type="II")


linwin7maleAICc <- AICc(lin7malenoint)

#females
lin7fem <- lmer(sc.weight ~ nov.feb.wSST * 
                  maturity_table_3 + (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])

summary(lin7fem)
Anova(lin7fem, type="III") #did not converge, will model without interaction?

lin7femnoint <- lmer(sc.weight ~ nov.feb.wSST + 
                       maturity_table_3 + (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])

summary(lin7femnoint)
Anova(lin7femnoint, type="II") #does converge

linwin7femAICc <- AICc(lin7femnoint)




#age 8 models-------

#males
lin8male <- lmer(sc.weight ~ nov.feb.wSST * 
                   maturity_table_3 + (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])

summary(lin8male)
Anova(lin8male, type="III") #interaction not sig so drop


lin8malenoint <- lmer(sc.weight ~ nov.feb.wSST + 
                        maturity_table_3 + (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])

summary(lin8malenoint)
Anova(lin8malenoint, type="II")


linwin8maleAICc <- AICc(lin8malenoint)

#females
lin8fem <- lmer(sc.weight ~ nov.feb.wSST * 
                  maturity_table_3 + (1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])

summary(lin8fem)
Anova(lin8fem, type="III") #

linwin8femAICc <- AICc(lin8fem)




#age 9 models-------

#males
lin9male <- lmer(sc.weight ~ nov.feb.wSST * 
                   maturity_table_3 + (1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])

summary(lin9male)
Anova(lin9male, type="III") #interaction not sig so drop


lin9malenoint <- lmer(sc.weight ~ nov.feb.wSST + 
                        maturity_table_3 + (1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])

summary(lin9malenoint)
Anova(lin9malenoint, type="II")


linwin9maleAICc <- AICc(lin9malenoint)

#females
lin9fem <- lmer(sc.weight ~ nov.feb.wSST * 
                  maturity_table_3 + (1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])

summary(lin9fem)
Anova(lin9fem, type="III") #interaction not sig so drop

lin9femnoint <- lmer(sc.weight ~ nov.feb.wSST + 
                       maturity_table_3 + (1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])

summary(lin9femnoint)
Anova(lin9femnoint, type="II")

linwin9femAICc <- AICc(lin9femnoint)





#age 10 models-------

#males
lin10male <- lmer(sc.weight ~ nov.feb.wSST * 
                   maturity_table_3 + (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),])

summary(lin10male)
Anova(lin10male, type="III") #singular fit


lin10malenoint <- lmer(sc.weight ~ nov.feb.wSST + 
                        maturity_table_3 + (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),])

summary(lin10malenoint)
Anova(lin10malenoint, type="II") #singular fit


linwin10maleAICc <- AICc(lin10malenoint)

#females
lin10fem <- lmer(sc.weight ~ nov.feb.wSST * 
                  maturity_table_3 + (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])

summary(lin10fem)
Anova(lin10fem, type="III") #interaction not sig so drop

lin10femnoint <- lmer(sc.weight ~ nov.feb.wSST + 
                       maturity_table_3 + (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])

summary(lin10femnoint)
Anova(lin10femnoint, type="II")

linwin10femAICc <- AICc(lin10femnoint)








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
summary(spr4male$gam) #sst not sig
summary(spr4male$mer)

plot(spr4male$gam) #looks linear

spr4maleAICc <- AICc(spr4male$mer)

#females
spr4fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
gam.check(spr4fem$gam)
summary(spr4fem$gam) #sst not sig
summary(spr4fem$mer)

plot(spr4fem$gam) #looks linear

spr4femAICc <- AICc(spr4fem$mer)

#age 5 models-------

#males
spr5male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
gam.check(spr5male$gam)
summary(spr5male$gam)
summary(spr5male$mer)

plot(spr5male$gam) #looks linear

spr5maleAICc <- AICc(spr5male$mer)

#females
spr5fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
gam.check(spr5fem$gam)
summary(spr5fem$gam)
summary(spr5fem$mer)

plot(spr5fem$gam) #looks pretty linear too

spr5femAICc <- AICc(spr5fem$mer)

#age 6 models-------

#males
spr6male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
gam.check(spr6male$gam)
summary(spr6male$gam)
summary(spr6male$mer)

plot(spr6male$gam) #looks linear

spr6maleAICc <- AICc(spr6male$mer)

#females
spr6fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
gam.check(spr6fem$gam)
summary(spr6fem$gam)
summary(spr6fem$mer)

plot(spr6fem$gam) #looks linear

spr6femAICc <- AICc(spr6fem$mer)

#age 7 models-------

#males
spr7male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
gam.check(spr7male$gam)
summary(spr7male$gam) #sst not sig
summary(spr7male$mer)

plot(spr7male$gam) #looks linear

spr7maleAICc <- AICc(spr7male$mer)

#females
spr7fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
gam.check(spr7fem$gam)
summary(spr7fem$gam) #sst not sig
summary(spr7fem$mer)

plot(spr7fem$gam) #looks linear

spr7femAICc <- AICc(spr7fem$mer)


#age 8 models-------


#males
spr8male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
gam.check(spr8male$gam)
summary(spr8male$gam) #sst not sig
summary(spr8male$mer)

plot(spr8male$gam) #looks linear

spr8maleAICc <- AICc(spr8male$mer)

#females
spr8fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
gam.check(spr8fem$gam)
summary(spr8fem$gam)
summary(spr8fem$mer)

plot(spr8fem$gam) #looks linear

spr8femAICc <- AICc(spr8fem$mer)


#age 9 models-------

#males
spr9male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])
gam.check(spr9male$gam)
summary(spr9male$gam)
summary(spr9male$mer)

plot(spr9male$gam) #looks linear

spr9maleAICc <- AICc(spr9male$mer)

#females
spr9fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                   maturity_table_3,
                 random=~(1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
gam.check(spr9fem$gam)
summary(spr9fem$gam) #sst AND mat not sig
summary(spr9fem$mer)

plot(spr9fem$gam) #looks linear

spr9femAICc <- AICc(spr9fem$mer)

#age 10 models-------

#males
spr10male <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                     maturity_table_3,
                   random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==1),])
gam.check(spr10male$gam)
summary(spr10male$gam) #sst not sig
summary(spr10male$mer)

plot(spr10male$gam) #looks linear

spr10maleAICc <- AICc(spr10male$mer)

#females
spr10fem <- gamm4(sc.weight ~ s(prevyr_apr.jul.wSST, k=4) + 
                    maturity_table_3,
                  random=~(1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
gam.check(spr10fem$gam)
summary(spr10fem$gam) #sst not sig
summary(spr10fem$mer)

spr10femAICc <- AICc(spr10fem$mer)

plot(spr10fem$gam) #looks linear

spr4maleAICc
mod4maleAICc #equiv

spr4femAICc
mod4femAICc #equiv

spr5maleAICc
mod5maleAICc #equiv

spr5femAICc
mod5femAICc #equiv

spr6maleAICc
mod6maleAICc #equiv

spr6femAICc
mod6femAICc #equiv

spr7maleAICc
mod7maleAICc #equiv

spr7femAICc
mod7femAICc #equiv

spr8maleAICc
mod8maleAICc #equiv

spr8femAICc
mod8femAICc #equiv

spr9maleAICc
mod9maleAICc #equiv

spr9femAICc
mod9femAICc #equiv

spr10maleAICc
mod10maleAICc #equiv

spr10femAICc
mod10femAICc #better


#spring models as linear-------------------------------------------------------------------
#let's repeat the spring models as linear models since they all (?) look to be linear

library(car)

#age 4 lin models-------

#males
spr4male_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                  (1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
summary(spr4male_linear)
Anova(spr4male_linear)

#interaction not significant, drop
spr4male_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                          (1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
summary(spr4male_noint)
Anova(spr4male_noint) #sst not sig


#females
spr4fem_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                         (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
summary(spr4fem_linear)
Anova(spr4fem_linear)

#interaction not significant, drop
spr4fem_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                         (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
summary(spr4fem_noint)
Anova(spr4fem_noint) #sst not sig



#age 5 lin models-------

#males
spr5male_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                          (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
summary(spr5male_linear)
Anova(spr5male_linear)

#interaction not significant, drop
spr5male_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                         (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
summary(spr5male_noint)
Anova(spr5male_noint) 


#females
spr5fem_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                         (1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
summary(spr5fem_linear)
Anova(spr5fem_linear)

#interaction not significant, drop
spr5fem_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                        (1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
summary(spr5fem_noint)
Anova(spr5fem_noint)




#age 6 lin models-------

#males
spr6male_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                          (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
summary(spr6male_linear)
Anova(spr6male_linear)

#interaction not significant, drop
spr6male_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                         (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
summary(spr6male_noint)
Anova(spr6male_noint) 


#females
spr6fem_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                         (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
summary(spr6fem_linear)
Anova(spr6fem_linear)

#interaction not significant, drop
spr6fem_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                        (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
summary(spr6fem_noint)
Anova(spr6fem_noint)




#age 7 lin models-------

#males
spr7male_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                          (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
summary(spr7male_linear)
Anova(spr7male_linear)

#interaction not significant, drop
spr7male_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                         (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
summary(spr7male_noint)
Anova(spr7male_noint) #sst not sig


#females
spr7fem_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                         (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
summary(spr7fem_linear)
Anova(spr7fem_linear)

#interaction not significant, drop
spr7fem_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                        (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
summary(spr7fem_noint)
Anova(spr7fem_noint) #sst not sig





#age 8 lin models-------

#males
spr8male_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                          (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
summary(spr8male_linear)
Anova(spr8male_linear)

#interaction not significant, drop
spr8male_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                         (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
summary(spr8male_noint)
Anova(spr8male_noint) #sst not sig


#females
spr8fem_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                         (1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
summary(spr8fem_linear)
Anova(spr8fem_linear)

#interaction not significant, drop
spr8fem_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                        (1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
summary(spr8fem_noint)
Anova(spr8fem_noint)






#age 9 lin models-------

#males
spr9male_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                          (1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])
summary(spr9male_linear)
Anova(spr9male_linear)

#interaction IS significant, won't drop



#females
spr9fem_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                         (1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
summary(spr9fem_linear)
Anova(spr9fem_linear)

#interaction IS significant, won't drop








#age 10 lin models-------

#males
spr10male_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                          (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),]) #singular fit
#singular fit, zero variance in yr/haul?
table(age10dat$year[which(age10dat$sex.code==1)], age10dat$Haul[which(age10dat$sex.code==1)])
summary(spr10male_linear)
Anova(spr10male_linear)

#interaction not significant, drop
spr10male_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                         (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),])
#also singular
summary(spr10male_noint)
Anova(spr10male_noint) 


#females
spr10fem_linear <- lmer(sc.weight ~ prevyr_apr.jul.wSST* maturity_table_3 +
                         (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
summary(spr10fem_linear)
Anova(spr10fem_linear)

#interaction not significant, drop
spr10fem_noint <- lmer(sc.weight ~ prevyr_apr.jul.wSST + maturity_table_3 +
                        (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
summary(spr10fem_noint)
Anova(spr10fem_noint) #sst not sig



visreg(spr10fem_noint, "prevyr_apr.jul.wSST",  data=age10dat[which(age10dat$sex.code==2),],
       overlay=TRUE, partial=FALSE, rug=TRUE, band=TRUE)

library(sjPlot)
library(jtools)
plot_models(spr10fem_noint, spr10male_noint)

plot_model(spr10fem_noint, 
           type="pred")

sm4 <- effect_plot(spr4male_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)
sf4 <- effect_plot(spr4fem_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)

sm5 <- effect_plot(spr5male_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)
sf5 <- effect_plot(spr5fem_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)

sm6 <- effect_plot(spr6male_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)
sf6 <- effect_plot(spr6fem_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)

sm7 <- effect_plot(spr7male_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)
sf7 <- effect_plot(spr7fem_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)

sm8 <- effect_plot(spr8male_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)
sf8 <- effect_plot(spr8fem_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)

sm9 <- effect_plot(spr9male_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)
sf9 <- effect_plot(spr9fem_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)

sm10 <- effect_plot(spr10male_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)
sf10 <- effect_plot(spr10fem_noint, pred = prevyr_apr.jul.wSST, interval = TRUE, plot.points = TRUE)


library(cowplot)
plot_grid(sm4, sm5, sm6, sm7, sm8,  sm10,
          sf4, sf5, sf6, sf7, sf8,  sf10, nrow = 2)


#null models----------------------------

#age 4 null models----


#males
spr4male_null <- lmer(sc.weight ~ maturity_table_3 +
                          (1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
summary(spr4male_null)
Anova(spr4male_null)

male4_spr_null_AIC <- AIC(spr4male_null)

#females
spr4fem_null <- lmer(sc.weight ~  maturity_table_3 +
                         (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
summary(spr4fem_null)
Anova(spr4fem_null)

fem4_spr_null_AIC <- AIC(spr4fem_null)



#age 5 null models----


#males
spr5male_null <- lmer(sc.weight ~ maturity_table_3 +
                        (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
summary(spr5male_null)
Anova(spr5male_null)

male5_spr_null_AIC <- AIC(spr5male_null)

#females
spr5fem_null <- lmer(sc.weight ~  maturity_table_3 +
                       (1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
summary(spr5fem_null)
Anova(spr5fem_null)

fem5_spr_null_AIC <- AIC(spr5fem_null)




#age 6 null models----

#males
spr6male_null <- lmer(sc.weight ~ maturity_table_3 +
                        (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
summary(spr6male_null)
Anova(spr6male_null)

male6_spr_null_AIC <- AIC(spr6male_null)

#females
spr6fem_null <- lmer(sc.weight ~  maturity_table_3 +
                       (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
summary(spr6fem_null)
Anova(spr6fem_null)

fem6_spr_null_AIC <- AIC(spr6fem_null)


#age 7 null models----

#males
spr7male_null <- lmer(sc.weight ~ maturity_table_3 +
                        (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
summary(spr7male_null)
Anova(spr7male_null)

male7_spr_null_AIC <- AIC(spr7male_null)

#females
spr7fem_null <- lmer(sc.weight ~  maturity_table_3 +
                       (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
summary(spr7fem_null)
Anova(spr7fem_null)

fem7_spr_null_AIC <- AIC(spr7fem_null)




#age 8 null models----

#males
spr8male_null <- lmer(sc.weight ~ maturity_table_3 +
                        (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
summary(spr8male_null)
Anova(spr8male_null)

male8_spr_null_AIC <- AIC(spr8male_null)

#females
spr8fem_null <- lmer(sc.weight ~  maturity_table_3 +
                       (1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
summary(spr8fem_null)
Anova(spr8fem_null)

fem8_spr_null_AIC <- AIC(spr8fem_null)



#age 9 null models----

#males
spr9male_null <- lmer(sc.weight ~ maturity_table_3 +
                        (1|year/Haul), data=age9dat[which(age9dat$sex.code==1),]) #didn't converge
summary(spr9male_null)
Anova(spr9male_null)

male9_spr_null_AIC <- AIC(spr9male_null)

#females
spr9fem_null <- lmer(sc.weight ~  maturity_table_3 +
                       (1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
summary(spr9fem_null)
Anova(spr9fem_null)

fem9_spr_null_AIC <- AIC(spr9fem_null)



#age 10 null models----

#males
spr10male_null <- lmer(sc.weight ~ maturity_table_3 +
                        (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),]) #singular fit
summary(spr10male_null)
Anova(spr10male_null)

male10_spr_null_AIC <- AIC(spr10male_null)

#females
spr10fem_null <- lmer(sc.weight ~  maturity_table_3 +
                       (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
summary(spr10fem_null)
Anova(spr10fem_null)

fem10_spr_null_AIC <- AIC(spr10fem_null)



spr4maleAICc
mod4maleAICc 
male4_spr_null_AIC #equiv
linwin4maleAICc

spr4femAICc
mod4femAICc 
fem4_spr_null_AIC #slightly better but AIC right about 4
linwin4femAICc #worse

spr5maleAICc
mod5maleAICc 
male5_spr_null_AIC #equiv
linwin5maleAICc

spr5femAICc
mod5femAICc 
fem5_spr_null_AIC #equiv
linwin5femAICc #maybe worse

spr6maleAICc
mod6maleAICc 
male6_spr_null_AIC #equiv
linwin6maleAICc

spr6femAICc
mod6femAICc 
fem6_spr_null_AIC #equiv
linwin6femAICc

spr7maleAICc
mod7maleAICc 
male7_spr_null_AIC #equiv
linwin7maleAICc

spr7femAICc
mod7femAICc 
fem7_spr_null_AIC #equiv
linwin7femAICc

spr8maleAICc
mod8maleAICc 
male8_spr_null_AIC #equiv, null maybe slightly better than spring
linwin8maleAICc

spr8femAICc
mod8femAICc 
fem8_spr_null_AIC 
linwin8femAICc #best but just by 4, likely too close to null

spr9maleAICc
mod9maleAICc 
male9_spr_null_AIC #didn't converge
linwin9maleAICc

spr9femAICc
mod9femAICc 
fem9_spr_null_AIC #equiv, null is better than spring
linwin9femAICc

spr10maleAICc
mod10maleAICc 
male10_spr_null_AIC #equiv
linwin10maleAICc

spr10femAICc
mod10femAICc #better than spring not better than null
fem10_spr_null_AIC
linwin10femAICc #better than spring and null but not winter







#repeat models w annual sst==========================================================


#reload sst data
d1 <- read.csv("./data/western.goa.sst.csv")
sst_lagged <- d1

sst_lagged$prevyr_annual.wSST <- NA
k<-1
for(k in 1:length(sst_lagged$year)){
  if(k>1){
    sst_lagged$prevyr_annual.wSST[k] <- sst_lagged$annual.wSST[k-1] 
  }
} #works now join

sstjoin <- sst_lagged[,c(1,5)]

age4dat <- left_join(age4dat, sstjoin, by="year")
age5dat <- left_join(age5dat, sstjoin, by="year")
age6dat <- left_join(age6dat, sstjoin, by="year")
age7dat <- left_join(age7dat, sstjoin, by="year")
age8dat <- left_join(age8dat, sstjoin, by="year")
age9dat <- left_join(age9dat, sstjoin, by="year")
age10dat <- left_join(age10dat, sstjoin, by="year")



#age 4 lin models-------

ggplot(age4dat[which(age4dat$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

#males
ann4male_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                          (1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
summary(ann4male_linear)
Anova(ann4male_linear, type="III")

#interaction significant
# ann4male_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
#                          (1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
# summary(ann4male_noint)
# Anova(ann4male_noint) #


#females
ggplot(age4dat[which(age4dat$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

ann4fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                         (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
summary(ann4fem_linear)
Anova(ann4fem_linear, type="III")

#interaction not significant, drop
ann4fem_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                        (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
summary(ann4fem_noint)
Anova(ann4fem_noint) #sst not sig





#age 5 lin models-------

ggplot(age5dat[which(age5dat$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

#males
ann5male_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                          (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
summary(ann5male_linear)
Anova(ann5male_linear, type="III")

#interaction not significant drop
ann5male_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                         (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
summary(ann5male_noint)
Anova(ann5male_noint) #both sig


#females
ggplot(age5dat[which(age5dat$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

ann5fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                         (1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
summary(ann5fem_linear)
Anova(ann5fem_linear, type="III")

#interaction  significant
# ann5fem_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
#                         (1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
# summary(ann5fem_noint)
# Anova(ann5fem_noint) #sst not sig




#age 6 lin models-------

ggplot(age6dat[which(age6dat$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

#males
ann6male_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                          (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
summary(ann6male_linear)
Anova(ann6male_linear, type="III")

#interaction not significant drop
ann6male_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                         (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
summary(ann6male_noint)
Anova(ann6male_noint) #both sig


#females
ggplot(age6dat[which(age6dat$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

ann6fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                         (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
summary(ann6fem_linear)
Anova(ann6fem_linear, type="III")

#interaction not significant drop
ann6fem_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                        (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
summary(ann6fem_noint)
Anova(ann6fem_noint) #both sig






#age 7 lin models-------

ggplot(age7dat[which(age7dat$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

#males
ann7male_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                          (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
summary(ann7male_linear)
Anova(ann7male_linear, type="III")

#interaction not significant drop
ann7male_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                         (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
summary(ann7male_noint)
Anova(ann7male_noint) #both sig


#females
ggplot(age7dat[which(age7dat$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

ann7fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                         (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
summary(ann7fem_linear)
Anova(ann7fem_linear, type="III")

#interaction not significant drop
ann7fem_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                        (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
summary(ann7fem_noint)
Anova(ann7fem_noint) #both sig






#age 8 lin models-------

ggplot(age8dat[which(age8dat$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

#males
ann8male_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                          (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
summary(ann8male_linear)
Anova(ann8male_linear, type="III")

#interaction not significant drop
ann8male_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                         (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
summary(ann8male_noint)
Anova(ann8male_noint) #both sig


#females
ggplot(age8dat[which(age8dat$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

ann8fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                         (1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
summary(ann8fem_linear)
Anova(ann8fem_linear, type="III")

#interaction not significant drop
ann8fem_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                        (1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
summary(ann8fem_noint)
Anova(ann8fem_noint) #both sig







#age 9 lin models-------

ggplot(age9dat[which(age9dat$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

#males
ann9male_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                          (1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])
summary(ann9male_linear)
Anova(ann9male_linear, type="III")

#interaction significant
# ann9male_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
#                          (1|year/Haul), data=age9dat[which(age9dat$sex.code==1),])
# summary(ann9male_noint)
# Anova(ann9male_noint) #


#females
ggplot(age9dat[which(age9dat$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

ann9fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                         (1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
summary(ann9fem_linear)
Anova(ann9fem_linear, type="III")

#interaction not significant drop
ann9fem_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                        (1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
summary(ann9fem_noint)
Anova(ann9fem_noint) #both sig







#age 10 lin models-------

ggplot(age10dat[which(age10dat$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

#males
ann10male_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                          (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),]) #singular
summary(ann10male_linear)
Anova(ann10male_linear, type="III")


ann10male_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                         (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),]) #singular no var in haul:year
summary(ann10male_noint)
Anova(ann10male_noint) #


#females
ggplot(age10dat[which(age10dat$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")+ facet_wrap(~maturity_table_3)

ann10fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST* maturity_table_3 +
                         (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
summary(ann10fem_linear)
Anova(ann10fem_linear, type="III")

#interaction not significant drop
ann10fem_noint <- lmer(sc.weight ~ prevyr_annual.wSST + maturity_table_3 +
                        (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
summary(ann10fem_noint)
Anova(ann10fem_noint) #both sig


#null models annual--------


#age 4 lin models-------

#males
ann4male_null <- lmer(sc.weight ~ maturity_table_3 +
                          (1|year/Haul), data=age4dat[which(age4dat$sex.code==1),])
summary(ann4male_null)
Anova(ann4male_null, type="II")

AIC(ann4male_null)
AIC(ann4male_linear) #same

r.squaredGLMM(ann4male_null)
r.squaredGLMM(ann4male_linear) #0.02 gain

#females
ann4fem_null <- lmer(sc.weight ~ maturity_table_3 +
                         (1|year/Haul), data=age4dat[which(age4dat$sex.code==2),])
summary(ann4fem_null)
Anova(ann4fem_null, type="II")

AIC(ann4fem_null) #better
AIC(ann4fem_linear)

r.squaredGLMM(ann4fem_null)
r.squaredGLMM(ann4fem_linear) #0.01 gain


#age 5 lin models-------

#males
ann5male_null <- lmer(sc.weight ~ maturity_table_3 +
                          (1|year/Haul), data=age5dat[which(age5dat$sex.code==1),])
summary(ann5male_null)
Anova(ann5male_null, type="II")

AIC(ann5male_null) #better
AIC(ann5male_linear)

r.squaredGLMM(ann5male_null) 
r.squaredGLMM(ann5male_linear)#0.12 gain

#females
ann5fem_null <- lmer(sc.weight ~ maturity_table_3 +
                         (1|year/Haul), data=age5dat[which(age5dat$sex.code==2),])
summary(ann5fem_null)
Anova(ann5fem_null, type="II")

AIC(ann5fem_null)
AIC(ann5fem_linear) #same

r.squaredGLMM(ann5fem_null)
r.squaredGLMM(ann5fem_linear) #0.09 gain



#age 6 lin models-------

#males
ann6male_null <- lmer(sc.weight ~  maturity_table_3 +
                          (1|year/Haul), data=age6dat[which(age6dat$sex.code==1),])
summary(ann6male_null)
Anova(ann6male_null, type="II")

AIC(ann6male_null) #aic 4 lower
AIC(ann6male_linear)

r.squaredGLMM(ann6male_null)
r.squaredGLMM(ann6male_linear) #0.07 gain

#females
ann6fem_null <- lmer(sc.weight ~  maturity_table_3 +
                         (1|year/Haul), data=age6dat[which(age6dat$sex.code==2),])
summary(ann6fem_null)
Anova(ann6fem_null, type="II")

AIC(ann6fem_null) #aic 4 lower
AIC(ann6fem_linear)

r.squaredGLMM(ann6fem_null)
r.squaredGLMM(ann6fem_linear) #0.08 gain





#age 7 lin models-------

#males
ann7male_null <- lmer(sc.weight ~  maturity_table_3 +
                          (1|year/Haul), data=age7dat[which(age7dat$sex.code==1),])
summary(ann7male_null)
Anova(ann7male_null, type="II")

AIC(ann7male_null) #aic 5 lower
AIC(ann7male_linear)

r.squaredGLMM(ann7male_null)
r.squaredGLMM(ann7male_linear) #0.08 gain

#females
ann7fem_null <- lmer(sc.weight ~  maturity_table_3 +
                         (1|year/Haul), data=age7dat[which(age7dat$sex.code==2),])
summary(ann7fem_null)
Anova(ann7fem_null, type="II")

AIC(ann7fem_null) #aic 5 lower
AIC(ann7fem_linear)

r.squaredGLMM(ann7fem_null)
r.squaredGLMM(ann7fem_linear) #0.08 gain




#age 8 lin models-------

#males
ann8male_null <- lmer(sc.weight ~ maturity_table_3 +
                          (1|year/Haul), data=age8dat[which(age8dat$sex.code==1),])
summary(ann8male_null)
Anova(ann8male_null, type="II")

AIC(ann8male_null) #better
AIC(ann8male_linear)

r.squaredGLMM(ann8male_null)
r.squaredGLMM(ann8male_linear) #0.09 gain

#females
ann8fem_null <- lmer(sc.weight ~  maturity_table_3 +
                         (1|year/Haul), data=age8dat[which(age8dat$sex.code==2),])
summary(ann8fem_null)
Anova(ann8fem_null, type="II")

AIC(ann8fem_null)
AIC(ann8fem_linear) #same

r.squaredGLMM(ann8fem_null)
r.squaredGLMM(ann8fem_linear) #0.10 gain


#age 9 lin models-------

#males
ann9male_null <- lmer(sc.weight ~  maturity_table_3 +
                          (1|year/Haul), data=age9dat[which(age9dat$sex.code==1),]) #failed to converge
summary(ann9male_null)
Anova(ann9male_null, type="II")

AIC(ann9male_null)
AIC(ann9male_linear) #aic 5 lower

r.squaredGLMM(ann9male_null)
r.squaredGLMM(ann9male_linear) #0.12 gain

#females
ann9fem_null <- lmer(sc.weight ~  maturity_table_3 +
                         (1|year/Haul), data=age9dat[which(age9dat$sex.code==2),])
summary(ann9fem_null)
Anova(ann9fem_null, type="II")

AIC(ann9fem_null)
AIC(ann9fem_linear) #same

r.squaredGLMM(ann9fem_null)
r.squaredGLMM(ann9fem_linear) #0.09 gain


#age 10 lin models-------

#males
ann10male_null <- lmer(sc.weight ~  maturity_table_3 +
                           (1|year/Haul), data=age10dat[which(age10dat$sex.code==1),]) #singular
summary(ann10male_null)
Anova(ann10male_null, type="II") #singular

AIC(ann10male_null)
AIC(ann10male_linear)

r.squaredGLMM(ann10male_null)
r.squaredGLMM(ann10male_linear) #0.13 gain

#females
ann10fem_null <- lmer(sc.weight ~  maturity_table_3 +
                          (1|year/Haul), data=age10dat[which(age10dat$sex.code==2),])
summary(ann10fem_null)
Anova(ann10fem_null, type="II")

AIC(ann10fem_null)
AIC(ann10fem_linear) #same

r.squaredGLMM(ann10fem_null)
r.squaredGLMM(ann10fem_linear) #0.09 gain


table(age4dat$sex.code, age4dat$maturity_table_3)
table(age5dat$sex.code, age5dat$maturity_table_3)
table(age6dat$sex.code, age6dat$maturity_table_3)
table(age7dat$sex.code, age7dat$maturity_table_3)
table(age8dat$sex.code, age8dat$maturity_table_3)
table(age9dat$sex.code, age9dat$maturity_table_3)
table(age10dat$sex.code, age10dat$maturity_table_3)




#only maturity code 3------------------

age4mat3 <- age4dat[which(age4dat$maturity_table_3=="3"),]
age5mat3 <- age5dat[which(age5dat$maturity_table_3=="3"),]
age6mat3 <- age6dat[which(age6dat$maturity_table_3=="3"),]
age7mat3 <- age7dat[which(age7dat$maturity_table_3=="3"),]
age8mat3 <- age8dat[which(age8dat$maturity_table_3=="3"),]
age9mat3 <- age9dat[which(age9dat$maturity_table_3=="3"),]
age10mat3 <- age10dat[which(age10dat$maturity_table_3=="3"),]



#age 4 lin models-------

ggplot(age4mat3[which(age4mat3$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

#males
threes4male_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                          (1|year/Haul), data=age4mat3[which(age4mat3$sex.code==1),])
summary(threes4male_linear)
Anova(threes4male_linear, type="II")


#females
ggplot(age4mat3[which(age4mat3$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

threes4fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                         (1|year/Haul), data=age4mat3[which(age4mat3$sex.code==2),])
summary(threes4fem_linear)
Anova(threes4fem_linear, type="II") #not sig





#age 5 lin models-------

ggplot(age5mat3[which(age5mat3$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

#males
threes5male_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                             (1|year/Haul), data=age5mat3[which(age5mat3$sex.code==1),])
summary(threes5male_linear)
Anova(threes5male_linear, type="II")


#females
ggplot(age5mat3[which(age5mat3$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

threes5fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                            (1|year/Haul), data=age5mat3[which(age5mat3$sex.code==2),])
summary(threes5fem_linear)
Anova(threes5fem_linear, type="II")





#age 6 lin models-------

ggplot(age6mat3[which(age6mat3$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

#males
threes6male_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                             (1|year/Haul), data=age6mat3[which(age6mat3$sex.code==1),])
summary(threes6male_linear)
Anova(threes6male_linear, type="II")


#females
ggplot(age6mat3[which(age6mat3$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

threes6fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                            (1|year/Haul), data=age6mat3[which(age6mat3$sex.code==2),])
summary(threes6fem_linear)
Anova(threes6fem_linear, type="II")





#age 7 lin models-------

ggplot(age7mat3[which(age7mat3$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

#males
threes7male_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                             (1|year/Haul), data=age7mat3[which(age7mat3$sex.code==1),])
summary(threes7male_linear)
Anova(threes7male_linear, type="II")


#females
ggplot(age7mat3[which(age7mat3$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

threes7fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                            (1|year/Haul), data=age7mat3[which(age7mat3$sex.code==2),])
summary(threes7fem_linear)
Anova(threes7fem_linear, type="II")





#age 8 lin models-------

ggplot(age8mat3[which(age8mat3$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

#males
threes8male_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                             (1|year/Haul), data=age8mat3[which(age8mat3$sex.code==1),])
summary(threes8male_linear)
Anova(threes8male_linear, type="II") #not sig


#females
ggplot(age8mat3[which(age8mat3$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

threes8fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                            (1|year/Haul), data=age8mat3[which(age8mat3$sex.code==2),])
summary(threes8fem_linear)
Anova(threes8fem_linear, type="II")




#age 9 lin models-------

ggplot(age9mat3[which(age9mat3$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

#males
threes9male_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                             (1|year/Haul), data=age9mat3[which(age9mat3$sex.code==1),])
summary(threes9male_linear)
Anova(threes9male_linear, type="II") #not sig


#females
ggplot(age9mat3[which(age9mat3$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

threes9fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                            (1|year/Haul), data=age9mat3[which(age9mat3$sex.code==2),])
summary(threes9fem_linear)
Anova(threes9fem_linear, type="II")




#age 10 lin models-------

ggplot(age10mat3[which(age10mat3$sex.code==1),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

#males
threes10male_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                             (1|year/Haul), data=age10mat3[which(age10mat3$sex.code==1),]) #singular
summary(threes10male_linear)
Anova(threes10male_linear, type="II") #not sig


#females
ggplot(age10mat3[which(age10mat3$sex.code==2),], aes(prevyr_annual.wSST, sc.weight, col=maturity_table_3)) + geom_point() + 
  geom_smooth(method="lm")

threes10fem_linear <- lmer(sc.weight ~ prevyr_annual.wSST +
                            (1|year/Haul), data=age10mat3[which(age10mat3$sex.code==2),])
summary(threes10fem_linear)
Anova(threes10fem_linear, type="II")


#null models only 3 maturity code------



#age 4 null models-------

#males
threes4male_null <- lmer(sc.weight ~ 
                             (1|year/Haul), data=age4mat3[which(age4mat3$sex.code==1),])
summary(threes4male_null)
Anova(threes4male_null, type="II")


#females

threes4fem_null <- lmer(sc.weight ~ 
                            (1|year/Haul), data=age4mat3[which(age4mat3$sex.code==2),])
summary(threes4fem_null)
Anova(threes4fem_null, type="II") #





#age 5 null models-------


#males
threes5male_null <- lmer(sc.weight ~ 
                             (1|year/Haul), data=age5mat3[which(age5mat3$sex.code==1),])
summary(threes5male_null)
Anova(threes5male_null, type="II")


#females

threes5fem_null <- lmer(sc.weight ~ 
                            (1|year/Haul), data=age5mat3[which(age5mat3$sex.code==2),])
summary(threes5fem_null)
Anova(threes5fem_null, type="II")





#age 6 null models-------

#males
threes6male_null <- lmer(sc.weight ~ 
                             (1|year/Haul), data=age6mat3[which(age6mat3$sex.code==1),])
summary(threes6male_null)
Anova(threes6male_null, type="II")


#females

threes6fem_null <- lmer(sc.weight ~ 
                            (1|year/Haul), data=age6mat3[which(age6mat3$sex.code==2),])
summary(threes6fem_null)
Anova(threes6fem_null, type="II")





#age 7 null models-------

#males
threes7male_null <- lmer(sc.weight ~ 
                             (1|year/Haul), data=age7mat3[which(age7mat3$sex.code==1),])
summary(threes7male_null)
Anova(threes7male_null, type="II")


#females

threes7fem_null <- lmer(sc.weight ~ 
                            (1|year/Haul), data=age7mat3[which(age7mat3$sex.code==2),])
summary(threes7fem_null)
Anova(threes7fem_null, type="II")





#age 8 null models-------

#males
threes8male_null <- lmer(sc.weight ~ 
                             (1|year/Haul), data=age8mat3[which(age8mat3$sex.code==1),])
summary(threes8male_null)
Anova(threes8male_null, type="II") #not sig


#females

threes8fem_null <- lmer(sc.weight ~ 
                            (1|year/Haul), data=age8mat3[which(age8mat3$sex.code==2),])
summary(threes8fem_null)
Anova(threes8fem_null, type="II")




#age 9 null models-------

#males
threes9male_null <- lmer(sc.weight ~ 
                             (1|year/Haul), data=age9mat3[which(age9mat3$sex.code==1),])
summary(threes9male_null)
Anova(threes9male_null, type="II") #not sig


#females
threes9fem_null <- lmer(sc.weight ~ 
                            (1|year/Haul), data=age9mat3[which(age9mat3$sex.code==2),])
summary(threes9fem_null)
Anova(threes9fem_null, type="II")




#age 10 null models-------

#males
threes10male_null <- lmer(sc.weight ~ 
                              (1|year/Haul), data=age10mat3[which(age10mat3$sex.code==1),]) #singular
summary(threes10male_null)
Anova(threes10male_null, type="II") #


#females

threes10fem_null <- lmer(sc.weight ~ 
                             (1|year/Haul), data=age10mat3[which(age10mat3$sex.code==2),])
summary(threes10fem_null)
Anova(threes10fem_null, type="II")


AIC(threes4male_linear) #lower by 6
AIC(threes4male_null)

AIC(threes4fem_linear) #higher by 2
AIC(threes4fem_null)


AIC(threes5male_linear) #lower by 5
AIC(threes5male_null)

AIC(threes5fem_linear) #lower by 7
AIC(threes5fem_null)


AIC(threes6male_linear) #lower by 6
AIC(threes6male_null)

AIC(threes6fem_linear) #lower by 7
AIC(threes6fem_null)


AIC(threes7male_linear) #lower by 2
AIC(threes7male_null)

AIC(threes7fem_linear) #lower by 3
AIC(threes7fem_null)


AIC(threes8male_linear) #same
AIC(threes8male_null)

AIC(threes8fem_linear) #lower by 5
AIC(threes8fem_null)


AIC(threes9male_linear) #high by 3
AIC(threes9male_null)

AIC(threes9fem_linear) #lower by 3
AIC(threes9fem_null)


AIC(threes10male_linear) #higher by 2
AIC(threes10male_null)

AIC(threes10fem_linear) #lower by 5
AIC(threes10fem_null)

library(performance)
check_model(threes5fem_linear)
