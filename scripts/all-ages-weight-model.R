#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#All ages weight models

#Krista, Jan 11, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: does age matter?
# going to use new SST from Mike
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#data is loaded in load_explor_size-at-age.R
library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(gamm4)
library(MuMIn)
theme_set(theme_bw())

#look at weight by age and sst

ggplot(all.dat, aes(annual.wSST, sc.weight, colour=as.factor(sex.code))) + geom_point() +geom_smooth()

ggplot(all.dat, aes(annual.wSST, sc.weight, colour=as.factor(Age))) + geom_point() +geom_smooth(method="lm")

all.dat$sex.code <- as.factor(all.dat$sex.code)
all.dat$age.factor <- as.factor(all.dat$Age)

#big overall model----

bigmod1 <- gamm4(sc.weight ~ s(annual.wSST, k=4) + s(annual.wSST, by=age.factor, k=4) + #s(annual.wSST, by=sex.code, k=4) +
                    maturity_table_3,
                  random=~(1|year/Haul), data=all.dat)
gam.check(bigmod1$gam)
plot(bigmod1$gam)
summary(bigmod1$gam)

bigmod2 <- gamm4(sc.weight ~  s(annual.wSST, by=age.factor, k=4), #+ #s(annual.wSST, by=sex.code, k=4),
                 random=~(1|year/Haul), data=all.dat)
gam.check(bigmod2$gam)
plot(bigmod2$gam)
summary(bigmod2$gam)
anova(bigmod2$gam)

bigmod3 <- gamm4(sc.weight ~  s(annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=all.dat[which(all.dat$sex.code==1),])
gam.check(bigmod3$gam)
plot(bigmod3$gam)
summary(bigmod3$gam)
anova(bigmod3$gam)

bigmod4 <- gamm4(sc.weight ~  s(annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=all.dat[which(all.dat$sex.code==2),])
gam.check(bigmod4$gam)
plot(bigmod4$gam)
summary(bigmod4$gam)
anova(bigmod4$gam)




#lag annual sst------
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

dat_lag <- left_join(all.dat, sstjoin)

#big models lagged 1 yr-------

bigmod5 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=dat_lag[which(dat_lag$sex.code==1),])
gam.check(bigmod3$gam)
plot(bigmod3$gam)
summary(bigmod3$gam)
anova(bigmod3$gam)

bigmod6 <- gamm4(sc.weight ~  s(prevyr_annual.wSST, by=age.factor, k=4) + maturity_table_3,
                 random=~(1|year/Haul), data=dat_lag[which(dat_lag$sex.code==2),])
gam.check(bigmod4$gam)
plot(bigmod4$gam)
summary(bigmod4$gam)
anova(bigmod4$gam)


