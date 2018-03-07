
#extract monthly climate data for hholds in IFLS data for jeremy magruder

library(maps)
library(maptools)
library(class)

#world_det=readShapePoly("/Documents/climate/borders/world_borders/TM_WORLD_BORDERS-0.2.shp")  #more detailed world map
#world_det=readShapePoly("D:/Documents/Research/Migration Indonesia/climate/borders/world_borders/TM_WORLD_BORDERS-0.2.shp")  #more detailed world map
world_det=readShapePoly("C:/Users/brian/Box Sync/Research/Indonesia/Migration and Crime/Data/Rainfall/Raw/TM_WORLD_BORDERS-0.2.shp")  #more detailed world map

#setwd('/Documents/climate/random/jeremy_indo/')
#setwd('D:/Documents/Berkeley/Research/Migration Indonesia/climate/random/jeremy_indo')
setwd("C:/Users/brian/Box Sync/Research/Indonesia/Migration and Crime/Data/Rainfall/UDel/data")
#hhold=read.csv('hhold_latlon.csv')
#################### hhold=read.csv('locations.csv')
hhold=read.csv('Locations.csv')

map(,xlim=c(95,130),ylim=c(-10,5))
points(hhold[,5:4],pch=19,col="red")

pdf(file="indo_hhold_map.pdf",height=5,width=8)
par(mar=c(3,3,3,3))
plot(world_det,xlim=c(97,123),ylim=c(-10,6),main="IFLS household locations")
abline(h=seq(-10,10,0.5),col="grey80",lty=1)
abline(v=seq(90,125,0.5),col="grey80",lty=1)
points(hhold[,5:4],pch=21,bg="red",cex=0.6)
box()
dev.off()

#read in Udel file to get lat-lon of Udel grids. first two columns are lon and lat.
#temp=read.table('/Documents/climate/data/udel/temp/air_temp.2000',header=F)
#################### To use a different Udel dataset: change the code below:
#################### temp=read.table('D:/Dropbox/Linked/Research/Migration Indonesia/climate/data/udel/temp/air_temp.2000',header=F)
temp=read.table('C:/Users/brian/Box Sync/Research/Indonesia/Migration and Crime/Data/Rainfall/UDel/data/temp14/air_temp.2014',header=F)
gridloc=paste(temp[,1],temp[,2],sep="_")


#drop hholds w/o latlon data, because knn can't work with missing values
hhold1=hhold[is.na(hhold[,4])==F,]
loc=knn(temp[,1:2],hhold1[,5:4],1:dim(temp)[1])  #gives you the row in the udel grids that the hholds match to

length(unique(gridloc[loc])) #the unique grids that we're pulling from = 230

#plot grids that we're pulling from to make sure we did it right
pdf(file="indo_hhold_map_matched.pdf",height=5,width=8)
par(mar=c(3,3,3,3))
un=unique(temp[loc,1:2])
plot(world_det,xlim=c(97,123),ylim=c(-10,6),main="IFLS household locations")
points(un,pch=22,col="blue",cex=1.6)
points(hhold[,5:4],pch=21,bg="red",cex=0.6)
box()
dev.off()

#now loop over UDel data
hholdt=hholdp=data.frame(hhold1,temp[loc,2:1])
#################### yrs=1950:2008
yrs=1900:2014
#setwd('/Documents/climate/data/udel/')
setwd('C:/Users/brian/Box Sync/Research/Indonesia/Migration and Crime/Data/Rainfall/UDel/data')
for (yr in yrs) {
  #################### To use a different Udel dataset: change the code below:
  #################### 	temp=read.table(paste('temp/air_temp.',yr,sep=""),header=F)
  #################### 	prec=read.table(paste('precip/precip.',yr,sep=""),header=F)
  temp=read.table(paste('temp14/air_temp.',yr,sep=""),header=F)
  prec=read.table(paste('precip14/precip.',yr,sep=""),header=F)
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