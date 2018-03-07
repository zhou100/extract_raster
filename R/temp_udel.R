
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
#read in Udel file to get lat-lon of Udel grids. first two columns are lon and lat.

#temp=read.table('C:/Users/brian/Box Sync/Research/Indonesia/Migration and Crime/Data/Rainfall/UDel/data/temp14/air_temp.2014',header=F)
temp=read.table("/Users/yujunzhou/Box Sync/Research/air_temp_2014/air_temp.2014",header=F)

coordinates(temp) <- ~ V1 + V2
projection(cropland) <- CRS("+proj=longlat +ellps=WGS84")


newproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

cropland_proj <- spTransform(cropland,CRS=CRS(newproj))


crs(temp) <-"+proj=longlat +ellps=WGS84"
temp_proj <- spTransform(temp,CRS=CRS(newproj))


pts.poly <- point.in.poly(temp_proj, cropland_proj)

plot(pts.poly)
tapply(pts.poly@data$V3, pts.poly@data$ISO3V10, FUN=length)
 
# Mean lead in each polygon


# temp_raster <- rasterize(cropland, temp, mask=TRUE)
# 
# 
# ex<-extract(temp,cropland)
# r <- rasterize(temp, r, , fun=min)
# o <- over(cropland,temp)




plot(temp_rast)

gridloc=paste(temp[,1],temp[,2],sep="_")
detach("package:R.utils", unload=TRUE) # to prevent errors in the extract below

data <- data.frame(coordinates(temp[,1:2]),
                   cropland$ISO3V10, 
                   extract(cropland, temp))
names(data) <- c("x", "y", "name", "value")


#now loop over UDel data
#################### yrs=1950:2008
yrs=2014:2014
#setwd('/Documents/climate/data/udel/')
for (yr in yrs) {
  #################### To use a different Udel dataset: change the code below:
  #################### 	temp=read.table(paste('temp/air_temp.',yr,sep=""),header=F)
  #################### 	prec=read.table(paste('precip/precip.',yr,sep=""),header=F)
  temp=read.table(paste('temp14/air_temp.',yr,sep=""),header=F)
  hholdt=cbind(hholdt,temp[loc,3:14])
  hholdp=cbind(hholdp,prec[loc,3:14])
  print(yr)
}

nm=c()
for (yr in yrs) {
  nm=c(nm,paste("year",yr,1:12,sep="_"))
}
names(hholdt)=names(hholdp)=c(names(hhold)[1:5],"udel_lat","udel_lon",nm)

#setwd('/Documents/climate/random/jeremy_indo/')
setwd('C:/Users/brian/Box Sync/Research/Indonesia/Migration and Crime/Data/Rainfall/UDel/data')
#################### To use a different Udel dataset: change the code below:
#################### write.csv(hholdt,'hhold_temp.csv',row.names=T)
#################### write.csv(hholdp,'hhold_prec.csv',row.names=T)
write.csv(hholdt,'hhold_temp14_311017.csv',row.names=T)
write.csv(hholdp,'hhold_prec14_311017.csv',row.names=T)


#look at the annual cycle for some hholds
xx=apply(hholdp[,8:187],2,mean)
plot(xx,type="l")
rg=92:187
plot(as.numeric(as.character(hholdp[1000,rg])),type="l")
zz=seq(min(rg),max(rg),12)
text(zz-min(rg),500,names(hholdp)[zz])