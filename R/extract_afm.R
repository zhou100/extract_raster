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

 
#Read Indbia map (shapefile)
# "../../map" is a relative path from your working directory - it means up two directories, then inside the map directory
#zmw_ward=readOGR("D:/udeltemp/shapefiles/livelihood zone 2012/MW_Admin1_LHZ_2012.3/MW_Admin1_LHZ_2012.3.shp")

zmw_ward=readOGR("shapefiles/zmw/zwe_polbnda_adm3_250k_cso.shp")
crs(zmw_ward) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

proj.latlon <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

zmw_ward_proj <- spTransform(zmw_ward, CRS = proj.latlon)
 

## Stack raster layers in a list/tmax

#setwd("/Users/yujunzhou/Google Drive/dat aImprove/zhou100/AfricanDrought /tmax")
rlist <- list.files(path="D:/zmw_temp/tmin_GFS_ANALYSIS_BC", 
                    pattern = "asc$",
                    full.names=TRUE)

r <- stack(rlist)

detach("package:R.utils", unload=TRUE)

#nlayers(r)


starttime <- proc.time() #begin processing timer

mat.data <- c()
for(i in 1:nlayers(r)) {
#for(i in 1:5) {
#ex <- extract(r[[i]], Ind)
  crs(r[[i]]) = "+proj=longlat +ellps=WGS84"
  raster_proj <- projectRaster(r[[i]], crs = proj.latlon)
  clip1_zmw <- crop(r[[i]], extent(zmw_ward)) #crop to extent of polygon
  clip2_zmw <- rasterize(zmw_ward, clip1_zmw, mask=TRUE)
  ex <- extract(clip2_zmw, zmw_ward)
  #ext <- getValues(clip2_ind)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4))
  mat <- sapply(ex, function(x){mean(x,na.rm = TRUE)})
  mat.data <-rbind(mat.data, mat)
  }

colnames(mat.data) <- as.character(format(zmw_ward$WARDPCODE, scientific=FALSE))

# 
# rlist <- list.files(path="D:/zmw_temp/tmin_GFS_ANALYSIS_BC", 
#                     pattern = "asc$",
#                     full.names=TRUE)
#name_list<-gsub(pattern = "D:/zmw_temp/tmin--GFS_ANALYSIS_BC/",x=rlist,replacement = "")
name_list<-gsub(pattern = "D:/zmw_temp/tmax--PGF/",x=rlist,replacement = "")

#name_list<- gsub(pattern = "_GFS_ANALYSIS_BC_",x=name_list,replacement = "")

name_list<- gsub(pattern = "_PGF_",x=name_list,replacement = "")

name_list<- gsub(pattern = "tmax",x=name_list,replacement = "")
name_list<- gsub(pattern = "_daily.asc",x=name_list,replacement = "")


date1 = as.Date(name_list,"%Y%m%d")
date1 = format(date1, "%m/%d/%Y")

mat.data.final<-cbind(mat.data,date1)


end_time <- proc.time() #end timer
end_time - starttimeInd
 
write.csv(mat.data.final,"zmw_daily_tmax70_08.csv")



