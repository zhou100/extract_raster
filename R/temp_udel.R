
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

cropland=readOGR("shapefiles/cropland_sage/cropland_country.shp")

newproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

cropland_proj <- spTransform(cropland,CRS=CRS(newproj))


mean_temp.master<-c()
yrs=1960:2013
month=1:12
for (yr in yrs) {
 
    #read in Udel file to get lat-lon of Udel grids. first two columns are lon and lat.
    #Column 3-14 are repectively Jan to Dec weather.
    temp=read.table(paste('C:/Users/Administrator/Desktop/air_temp_2014/air_temp.',yr,sep=""),header=F)
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
 
write.csv(mean_temp.master,"udel_temperature_cropland.csv")
   