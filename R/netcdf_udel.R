library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)

packages <-c("chron","RColorBrewer","lattice","ncdf4")
lapply(packages[4],function(x){install.packages(x)})



# 3-14 

#udel1980-2010.nc
ncin<-nc_open( "/Users/yujunzhou/Downloads/air.mon.mean.v401.nc", write=FALSE, readunlim=TRUE, verbose=FALSE,
         auto_GMT=TRUE, suppress_dimvals=FALSE )

print(paste("The file has",ncin$nvars,"variables"))
v1 <- ncin$var[[1]]
temp<-ncvar_get(ncin)

lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
tunits
print(ncin)

# get temperature
dname <- "air"
tmp_array <- ncvar_get(ncin,"air")
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)


tmp_array[tmp_array==fillvalue$value] <- NA

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tyear))

length(na.omit(as.vector(tmp_array[,,1])))

tyear <- as.integer(unlist(tdstr)[1])


m <- 1
tmp_slice <- tmp_array[,,m]


tmp_array[1,,]
# quick map

lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)
 
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)



grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


# set path and filename
csvpath <- ""
csvname <- "cru_tmp_1.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(tmp_df01),csvfile, row.names=FALSE, sep=",")
