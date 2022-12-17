# make a plot of SST time series

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

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1950-01-01):1:(2022-11-01T00:00:00Z)][(0.0):1:(0.0)][(54):1:(62)][(200):1:(226)]", "~temp")

# paste into browser for windows!


# load and process SST data

nc <- nc_open("./data/nceiErsstv5_554f_c21d_1d93.nc")

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

annual.wSST <- tapply(rowMeans(wSST, na.rm = T), yr, mean)

# plot
plot_dat <- data.frame(year = 1950:2022, 
                       `January-June` = annual.wSST.jan.jun,
                       Annual = annual.wSST) %>%
  pivot_longer(cols = -year)

# save 
write.csv(plot_dat, "./figs/western.goa.sst.1950.2022.csv", row.names = F)

ann_clim <- mean(annual.wSST[names(annual.wSST) < 2000])
jan.jun.clim <- mean(annual.wSST.jan.jun[names(annual.wSST.jan.jun) < 2000])

ggplot(plot_dat, aes(year, value, color = name)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = c(ann_clim, jan.jun.clim), 
             lty = 2,
             color = cb[c(1,7)]) +
  scale_color_manual(values = cb[c(1,7)]) +
  theme(legend.position = c(0.25, 0.92),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(y = "SST (°C)")

ggsave("./figs/sst_time_series.png", width = 5, height = 3.7, units = 'in')
