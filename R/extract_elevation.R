#install.packages("RCurl")
#install.packages("R.utils")
#install.packages("ggplot2")
#install.packages("rgdal")
#install.packages("sp")
#install.packages("raster")
#install.packages("gdalUtils")
#install.packages("parallel")
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

#Read  (shapefile)
Nepal=readOGR("shapefiles/nepal/NPL_new.shp")

# Raster data downloaded from this site
# https://data.humdata.org/dataset/nepal-digital-model-elevation-dem

elevation<-raster("srtm_cgiar_nepal_boundary.img")
#check if the projection system is the same 

crs(elevation)
crs(Nepal)

# new CRS used in this post, feel free to change it to another one 
#https://gis.stackexchange.com/questions/252830/what-crs-to-use-to-georeference-map

newproj<-"+proj=tmerc +lat_0=0 +lon_0=87 +k=0.9999 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=296.207,737.545,273.001,0,0,0,0 +units=m +no_defs"# transform to another 
elevation_proj <- projectRaster(elevation, crs=newproj)
Nepal_proj <- spTransform(Nepal,CRS=CRS(newproj))

 
clip_nepal <- crop(elevation_proj, extent(Nepal_proj)) #crop the elevtaion to extent of nepal polygon
mask_nepal <- rasterize(Nepal_proj, clip_nepal, mask=TRUE) # use mask function to rasterize

detach("package:R.utils",unload=TRUE)

ex <- extract(mask_nepal, Nepal_proj) # extract the values  
mat <- as.data.frame(t(lapply(ex, FUN = mean ))) # average the values for each polygon

# formatting 
extracted = as.data.frame(t(mat))
colnames(extracted)="Elevation"
extracted$NAME = as.character(Nepal$NAME)
extracted$DISTRICT = as.character(Nepal$DISTRICT)
extracted$F_ID = as.character(Nepal$F_ID)
extracted$N_ID = as.character(Nepal$N_ID)
extracted$Elevation = unlist(extracted$Elevation)

extracted <- extracted[c("F_ID","N_ID","Elevation","NAME","DISTRICT")] # reorder the columns

sum(is.na(extracted$Elevation)) # no missings in the elevation


write.csv(extracted,"nepal_elevation.csv")
  