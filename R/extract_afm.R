#install.packages("RCurl")
#install.packages("R.utils")
#install.packages("ggplot2")
#install.packages("rgdal")
#install.packages("sp")
#install.packages("raster")
#install.packages("gdalUtils")
#install.packages("parallel")
#install.packages("downloader")
#install.packages("rgeos")

library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
library(rgeos)
require(parallel) 

library(adehabitat)

#Read Indbia map (shapefile)
# "../../map" is a relative path from your working directory - it means up two directories, then inside the map directory
mw_lhz=readOGR("D:/udeltemp/shapefiles/livelihood zone 2012/MW_Admin1_LHZ_2012.3/MW_Admin1_LHZ_2012.3.shp")

proj.latlon <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
mw_lhz_proj <- spTransform(mw_lhz, CRS = proj.latlon)
 

## Stack raster layers in a list/tmax

#setwd("/Users/yujunzhou/Google Drive/dataImprove/zhou100/AfricanDrought /tmax")
rlist <- list.files(path="D:/afm/tmin--GFS_ANALYSIS_BC", 
                    pattern = "asc$",
                    full.names=TRUE)

r <- stack(rlist)
 
 
detach("package:R.utils", unload=TRUE)

#nlayers(r)


starttimeInd <- proc.time() #begin processing timer

mat.data <- c()
for(i in 1:nlayers(r)) {
#for(i in 1:5) {
#ex <- extract(r[[i]], Ind)
  crs(r[[i]]) = "+proj=longlat +ellps=WGS84"
  raster_proj <- projectRaster(r[[i]], crs = proj.latlon)
  clip1_ind <- crop(r[[i]], extent(mw_lhz)) #crop to extent of polygon
  clip2_ind <- rasterize(mw_lhz, clip1_ind, mask=TRUE)
  ex <- extract(clip2_ind, mw_lhz)
  #ext <- getValues(clip2_ind)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4))
  mat <- sapply(ex, function(x){mean(x,na.rm = TRUE)})
  mat.data <-rbind(mat.data, mat)
  }

colnames(mat.data) <- as.character(format(mw_lhz$FNID, scientific=FALSE))

name_list<-gsub(pattern = "D:/afm/tmin--GFS_ANALYSIS_BC/",x=rlist,replacement = "")
name_list<- gsub(pattern = "_GFS_ANALYSIS_BC_",x=name_list,replacement = "")
name_list<- gsub(pattern = "tmin",x=name_list,replacement = "")
name_list<- gsub(pattern = "_daily.asc",x=name_list,replacement = "")


date1 = as.Date(name_list,"%Y%m%d")
date1 = format(date1, "%m/%d/%Y")

mat.data.final<-cbind(mat.data,date1)


end_time <- proc.time() #end timer
end_time - starttimeInd
 
write.csv(mat.data.final,"mw_daily_tmin.csv")



