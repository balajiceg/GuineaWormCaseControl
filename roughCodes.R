library(sf)
library(rgdal)
library(raster)
library(dplyr)
library(RColorBrewer)
library(mapview)
library(ggplot2)
library(openxlsx)
library(randomcoloR)
library(rgeos)
library(ggmap)
library(leaflet)

#change working directory 
setwd('D:\\GuineaWorm_DrRebecca')

#---- read data and create spatial columns ----
#read trial one file
vilGps<-as_tibble(read.xlsx('compare_village_list_gps_new.xlsx'))

# convert to spatial
vilGpsOld<-st_as_sf(vilGps,coords = c("LonOld","LatOld"),crs=CRS("+proj=longlat +datum=WGS84"),remove=F) %>% st_transform(CRS("+init=epsg:32633"))
#plot(vilGpsSub['Village'],axes = TRUE)
vilGpsNew<-st_as_sf(vilGps,coords = c("Longitude","Latitude"),crs=CRS("+proj=longlat +datum=WGS84"),remove=F) %>% st_transform(CRS("+init=epsg:32633"))

#draw buffers and plot
vilGpsOldBuff<- st_buffer(vilGpsOld,dist = 3000)
VilGpsNewBuff<- st_buffer(vilGpsNew,dist = 3000)

#mapview
mapview(vilGpsOldBuff,col.regions='red',label='Village') + mapview(VilGpsNewBuff,col.regions='blue',label='Village')
