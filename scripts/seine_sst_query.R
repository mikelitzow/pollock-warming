# query Jan-Jun SST for analyzing age-0 pollock length 

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

# 2006-2021 ERSSTv5
# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(2006-01-01):1:(2021-12-01T00:00:00Z)][(0.0):1:(0.0)][(54):1:(62)][(200):1:(226)]", "~temp")

# paste into browser for windows!

# load and process SST data
# nc <- nc_open("~temp")

nc <- nc_open("./data/nceiErsstv5_d6bf_7c06_76a3.nc")

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

drop <- lon > 210 | lat < 56 | lat > 58
wSST <- SST
wSST[,drop] <- NA

# and check
temp.mean <- colMeans(wSST, na.rm=T)
z <- t(matrix(temp.mean,length(y)))  
image.plot(x,y,z, col=oceColorsPalette(64), xlim=c(195,230), ylim=c(53,62))
map('world2Hires',c('Canada', 'usa'), fill=T,xlim=c(130,250), ylim=c(20,66),add=T, lwd=1, col="lightyellow3")

# query monthly mean temp
dat <- data.frame(year = as.numeric(as.character(yr)),
                  month = as.numeric(m),
                  sst = rowMeans(wSST, na.rm = T)) %>%
  dplyr::filter(month %in% 1:6) %>%
  group_by(year) %>%
  summarise(sst = mean(sst))

ggplot(dat, aes(year,sst)) +
  geom_line() +
  geom_point() +
  theme_bw()

# and save
write.csv(dat, "./data/western.goa.jan-jul.sst.csv", row.names = F)
