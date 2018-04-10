
#extract monthly precip data for noncropland 

library(maps)
library(class)

library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
require(spatialEco)
require(sp)

noncropland=readOGR("shapefiles/noncropland/non_cropland.shp")

newproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

noncropland_proj <- spTransform(noncropland,CRS=CRS(newproj))


mean_precip.master<-c()
yrs=1960:2014
month=1:12

for (yr in yrs) {
 
    #read in Udel file to get lat-lon of Udel grids. first two columns are lon and lat.
    #Column 3-14 are repectively Jan to Dec weather.
    precip=read.table(paste('D:/precip_2014/precip.',yr,sep=""),header=F)
    # first two columns are lon and lat. read them as coordinates so that 
    coordinates(precip) <- ~ V1 + V2
    
    # align the projection systems
    crs(precip) <-"+proj=longlat +ellps=WGS84"
    precip_proj <- spTransform(precip,CRS=CRS(newproj))
    
    # spatiallly join the point preciperature data and noncropland shapefile
    pts.poly <- point.in.poly(precip_proj, noncropland_proj)
    
    #loop over the month
    for (mo in month) {
    month_ex = paste("V",mo+2,sep = "")
    mean_precip<-tapply(pts.poly@data[,month_ex], pts.poly@data$ISO3V10, FUN=function(x){mean(x,na.rm=TRUE)})
    #mean_precip<-as.data.frame(mean_precip)
    # document the years and month for joining data later
    mean_precip$year = yr
    mean_precip$month = mo

    mean_precip.master<-rbind(mean_precip.master,mean_precip)
  }

}

mean_precip.master<-as.matrix(mean_precip.master) 
write.csv(mean_precip.master,"udel_rain_noncropland.csv")
   