
#extract monthly temp data for cropland 

library(maps)
library(maptools)
library(class)

library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
library(rgeos)
require(spatialEco)
require(sp)

tz_lhz=readOGR("D:/udeltemp/shapefiles/TZ_LHZ_2009/TZ_LHZ_2009.shp")

#newproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

proj.latlon <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
unionbuffer <- spTransform(ug_lhz, CRS = proj.latlon)

cropland_proj <- spTransform(cropland,CRS=CRS(newproj))


mean_temp.master<-c()
yrs=2006:2014
month=1:12
for (yr in yrs) {
 
    #read in Udel file to get lat-lon of Udel grids. first two columns are lon and lat.
    #Column 3-14 are repectively Jan to Dec weather.
    temp=read.table(paste('D:/udeltemp/air_temp_2014/air_temp.',yr,sep=""),header=F)
    # first two columns are lon and lat. read them as coordinates so that 
    coordinates(temp) <- ~ V1 + V2
    
    # align the projection systems
    crs(temp) <-"+proj=longlat +ellps=WGS84"
    temp_proj <- spTransform(temp,CRS=CRS(newproj))
    
    # spatiallly join the point temperature data and cropland shapefile
    pts.poly <- point.in.poly(temp_proj, cropland_proj)
    
    #loop over the month
    for (mo in month) {
    month_ex = paste("V",mo+2,sep = "")
    mean_temp<-tapply(pts.poly@data[,month_ex], pts.poly@data$ISO3V10, FUN=function(x){mean(x,na.rm=TRUE)})
    #mean_temp<-as.data.frame(mean_temp)
    # document the years and month for joining data later
    mean_temp$year = yr
    mean_temp$month = mo

    mean_temp.master<-rbind(mean_temp.master,mean_temp)
  }

}

mean_temp.master<-as.data.frame(mean_temp.master) 
write.csv(mean_temp.master,"udel_temperature_cropland.csv")
   