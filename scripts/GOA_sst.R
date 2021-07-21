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

# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1947-01-01):1:(2021-5-01T00:00:00Z)][(0.0):1:(0.0)][(20):1:(68)][(120):1:(250)]", "~temp")

# paste into browser for windows!


# load and process SST data
# nc <- nc_open("~temp")

nc <- nc_open("./data/nceiErsstv5_130f_00d5_4da1.nc")

# process

ncvar_get(nc, "time")   # seconds since 1-1-1970
raw <- ncvar_get(nc, "time")
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))

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

drop <- lon > 210 | lat < 56 | lat > 58
wSST <- SST
wSST[,drop] <- NA

drop <- 

# and check
temp.mean <- colMeans(wSST, na.rm=T)
z <- t(matrix(temp.mean,length(y)))  
image.plot(x,y,z, col=oceColorsPalette(64), xlim=c(195,230), ylim=c(53,62))
map('world2Hires',c('Canada', 'usa'), fill=T,xlim=c(130,250), ylim=c(20,66),add=T, lwd=1, col="lightyellow3")

# calculate monthly anomaly

wSST <- rowMeans(wSST, na.rm = T)

# and Apr-July
yr <- as.numeric(as.character(years(d)))
m <- months(d)
apr.jul.m <- m[m %in% c("Apr", "May", "Jun", "Jul")]
apr.jul.yr <- yr[m %in% c("Apr", "May", "Jun", "Jul")]

apr.jul.wSST <- wSST[m %in% c("Apr", "May", "Jun", "Jul")]
apr.jul.wSST <- tapply(apr.jul.wSST, apr.jul.yr, mean)

xprt <- data.frame(year=1900:2020,
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

write.csv(xprt, "./data/western.goa.sst.csv", row.names = F)

plot <- xprt %>%
  pivot_longer(cols = -year)

ggplot(plot, aes(year, value, color = name)) +
  geom_line() +
  geom_point()
