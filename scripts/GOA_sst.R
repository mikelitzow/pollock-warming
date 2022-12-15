library(ncdf4)
library(zoo)
library(gplots)
library(dplyr)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(ggplot2)
library(oce)


# script for calculating GOA sst anomalies wrt 1951-1980
# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1900-01-01):1:(2020-12-01T00:00:00Z)][(0.0):1:(0.0)][(54):1:(62)][(200):1:(226)]", "~temp")

# 1980-2021 version
# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1980-01-01):1:(2021-12-01T00:00:00Z)][(0.0):1:(0.0)][(54):1:(62)][(200):1:(226)]", "~temp")


# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1947-01-01):1:(2022-8-01T00:00:00Z)][(0.0):1:(0.0)][(20):1:(68)][(120):1:(250)]", "~temp")

# paste into browser for windows!


# load and process SST data
# nc <- nc_open("~temp")

# nc <- nc_open("./data/nceiErsstv5_130f_00d5_4da1.nc")

# nc <- nc_open("./data/nceiErsstv5_6fc3_2e06_d3bf.nc")

nc <- nc_open("./data/nceiErsstv5_6fc3_2e06_d3bf_1.nc")

# nc <- nc_open("./data/nceiErsstv5_0acf_1348_bf9d.nc")
# 1980-2020 GOA version:
# nc <- nc_open("./data/nceiErsstv5_7676_409f_f7a3.nc")

# process

ncvar_get(nc, "time")   # seconds since 1-1-1970
raw <- ncvar_get(nc, "time")
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))
m <- months(d)
yr <- years(d)

x <- ncvar_get(nc, "longitude")
y <- ncvar_get(nc, "latitude")

SST <- ncvar_get(nc, "sst", verbose = F)

SST <- aperm(SST, 3:1)  

SST <- matrix(SST, nrow=dim(SST)[1], ncol=prod(dim(SST)[2:3]))  

# Keep track of corresponding latitudes and longitudes of each column:
lat <- rep(y, length(x))   
lon <- rep(x, each = length(y))   
dimnames(SST) <- list(as.character(d), paste("N", lat, "E", lon, sep=""))

# plot to check

# need to drop Bristol Bay cells
BB <- c("N58E200", "N58E202", "N56E200")
SST[,BB] <- NA

# and check
temp.mean <- colMeans(SST, na.rm=T)
z <- t(matrix(temp.mean,length(y)))  
image.plot(x,y,z, col=oceColorsPalette(64), xlim=c(195,230), ylim=c(53,62))
contour(x, y, z, add=T)  
map('world2Hires',c('Canada', 'usa'), fill=T,xlim=c(130,250), ylim=c(20,66),add=T, lwd=1, col="lightyellow3")

# WGOA for cod/pollock paper

drop <- lon < 200 | lon > 210 | lat < 56 | lat > 58
wSST <- SST
wSST[,drop] <- NA


# and check
temp.mean <- colMeans(wSST, na.rm=T)
z <- t(matrix(temp.mean,length(y)))  
image.plot(x,y,z, col=oceColorsPalette(64), xlim=c(195,230), ylim=c(53,62))
map('world2Hires',c('Canada', 'usa'), fill=T,xlim=c(130,250), ylim=c(20,66),add=T, lwd=1, col="lightyellow3")

# limit to Jan - June

jan.jun.wSST <- wSST[m %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),]

jan.jun.yr <- yr[m %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun")]

annual.wSST.jan.jun <- tapply(rowMeans(jan.jun.wSST, na.rm = T), jan.jun.yr, mean)

# save
save.dat <- data.frame(year = 1947:2022, sst = annual.wSST.jan.jun)
write.csv(save.dat, "./data/annual.wSST.jan.jun.csv")



# calculate monthly anomaly
# remove seasonal means
f <- function(x) tapply(x, m, mean)  # function to compute monthly means for a single time series
mu <- apply(wSST, 2, f)	# compute monthly means for each time series (cell)
mu <- mu[rep(1:12, length(d)/12),]  # replicate means matrix for each year at each location

anom <- wSST - mu   # compute matrix of anomalies

# get rownames to calculate overall mean anomaly for each month
SST.anom <- rowMeans(anom, na.rm = T)



SST.anom <- data.frame(year = as.numeric(as.character(yr)),
                       month = as.numeric(m), 
                       anom = SST.anom)

SST.anom$dec.yr = SST.anom$year + (SST.anom$month-0.5)/12

ggplot(SST.anom, aes(dec.yr, anom)) +
  geom_line()

write.csv(SST.anom, "./data/monthly_western_GOA_SST_anomalies_wrt_1980-2021.csv", row.names = F)


## get seasonal and annual means in °C
wSST <- rowMeans(wSST, na.rm = T)

# and Apr-July
yr <- as.numeric(as.character(years(d)))
m <- months(d)
apr.jul.m <- m[m %in% c("Apr", "May", "Jun", "Jul")]
apr.jul.yr <- yr[m %in% c("Apr", "May", "Jun", "Jul")]

apr.jul.wSST <- wSST[m %in% c("Apr", "May", "Jun", "Jul")]
apr.jul.wSST <- tapply(apr.jul.wSST, apr.jul.yr, mean)

xprt <- data.frame(year=1980:2021,
                   apr.jul.wSST=apr.jul.wSST)

ggplot(xprt, aes(year, apr.jul.wSST)) +
  geom_line() +
  geom_point()


# and winter 

# set up winter year
win.yr <- yr
win.yr[m %in% c("Nov", "Dec")] <- win.yr[m %in% c("Nov", "Dec")]+1

sst.win <- wSST[m %in% c("Nov", "Dec", "Jan", "Feb", "Mar")]
win.yr <- win.yr[m %in% c("Nov", "Dec", "Jan", "Feb", "Mar")]

win.sst <- tapply(sst.win, win.yr, mean)

# add to xport while dropping 1900 and 2021 (incomplete years)
win.sst <- c(NA, win.sst[names(win.sst) %in% 1901:2020])

xprt$nov.feb.wSST <- win.sst

ggplot(xprt, aes(year, nov.feb.wSST)) +
  geom_line() +
  geom_point()


# finally, annual mean temp
ann.sst <- tapply(wSST, yr, mean)


xprt$annual.wSST <- ann.sst

ggplot(xprt, aes(year, annual.wSST)) +
  geom_line() +
  geom_point()

# and save
write.csv(xprt, "./data/western.goa.sst.csv", row.names = F)

plot <- xprt %>%
  tidyr::pivot_longer(cols = -year)

ggplot(plot, aes(year, value, color = name)) +
  geom_line() +
  geom_point()
