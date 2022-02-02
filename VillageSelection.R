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
library(ggsn)
library(leaflet)

#change working directory 
setwd('D:\\GuineaWorm_DrRebecca')

#---- read data and create spatial columns ----
#read trial one file
vilGps<-as_tibble(read.xlsx('chad_fbz_clinical_trial1.xlsx',sheet='GPS')) %>% rename(Province=`<U+FEFF>Province`)  %>% print

#select only district, village, lat and lon, and remove na in latitude and longitude
vilGpsSub<- vilGps %>% dplyr::select(Province,District, Village, Latitude, Longitude) %>%
                        mutate(across(Latitude | Longitude,as.numeric)) %>%
                        filter(!is.na(Longitude))%>% distinct() %>% print

# convert to spatial
vilGpsSub<-st_as_sf(vilGpsSub,coords = c("Longitude","Latitude"),crs=CRS("+proj=longlat +datum=WGS84"),remove=F)
#plot(vilGpsSub['Village'],axes = TRUE)

#filter the upper cluster alone
vilGpsSub<- filter(vilGpsSub,Longitude<17 & Latitude < 13)
#plot(vilGpsSub['Village'],axes = TRUE)

#group by village
vilGpsGrouped<-vilGpsSub%>%
  group_by(Village) %>%
  summarise(Loc_count=n())

#form centroid
centroids<-vilGpsGrouped %>% mutate(geometry=st_centroid(geometry))

#merge new coordinates from Adam  and change old coordinates
vilGps.new<-as_tibble(read.xlsx('compare_village_list_gps_new.xlsx')) %>% select(Village,Longitude,Latitude)
centroids <- centroids %>% merge(vilGps.new,by="Village",all.x=T,all.y=F)
centroids<- centroids %>% mutate(Latitude = ifelse(is.na(Latitude),st_coordinates(geometry)[,2],Latitude),
                               Longitude = ifelse(is.na(Longitude),st_coordinates(geometry)[,1],Longitude))
centroids<- centroids %>% st_drop_geometry() %>%
              st_as_sf(coords = c("Longitude","Latitude"),crs=CRS("+proj=longlat +datum=WGS84"),remove=F)
# medians<-vilGpsSub%>%
#   group_by(Province,Village) %>%
#   summarise(Latitude=median(Latitude),Longitude=mean(Longitude)) %>%
#   st_drop_geometry() %>%
#   st_as_sf(coords = c("Longitude","Latitude"),crs=CRS("+proj=longlat +datum=WGS84"),remove=F)
#   
# centroids$disToMedians<-st_distance(medians,centroids,by_element = T)

#pal<-distinctColorPalette(77)
#mapView(medians,col.regions='red',label='Village')+
#  mapView(centroids,zcol='disToMedians',label='Village')+ mapView(vilGpsSub,zcol='Village',col.regions=pal)

#not much difference so delete the medians
#rm(medians)

#transform centroid to planar coordinates
VilCentroids<-st_transform(centroids,CRS("+init=epsg:32633"))

ggplot(VilCentroids) + geom_sf(shape='+',size=3,aes(col=Village)) + 
  theme_bw() + theme(text = element_text(size=14),legend.position="none")
remove(vilGps,vilGpsSub)
#mapView(VilCentroids)
#compute nearest distance
#disMat<-units::drop_units(st_distance(VilCentroids))
#disMat[disMat==0]<- NA
#VilCentroids$minNeighDistance<-apply(disMat,1,min,na.rm=T)

#---- create buffer and test ----
VilBuffers<-st_buffer(VilCentroids,dist = 3000)
VilBuffers$intersects<-lapply(st_intersects(VilBuffers),length)>1

#ggplot(VilBuffers) + geom_sf(aes(fill=intersects)) + 
#  theme_bw() + theme(text = element_text(size=14),legend.position=c(0.9, 0.9))

#mapview(VilBuffers,zcol='intersects',label='Village')

#check the is any of the village names approx matches other
# library(stringdist)
# strDist<-expand.grid(VilBuffers$Village,VilBuffers$Village)
# strDist$dist<-stringdist(strDist$Var1,strDist$Var2,method='jw')
# View(strDist[strDist$dist!=0,])
# #filter first match other than exact match
# strDist %>% filter(dist!=0) %>% arrange(dist) %>% as_tibble() %>%
#             distinct(Var1,.keep_all = T) %>% View

#### ----- merge dog count and cases ---- ####
#read dog survey and compute min max median
dogsCount<-as_tibble(read.xlsx('chad_fbz_clinical_trial1.xlsx',sheet='Eligible'))%>% 
          rename(nDogs=`#.de.Chiens_Eligibles`,Province=`<U+FEFF>Province`) %>% distinct() %>%
          dplyr::select(Province,District, Village, Month,nDogs) %>% print

#summarize by month and count
dogsCount<-dogsCount %>% mutate(vilNameAlt=tolower(gsub('\\s','',Village))) %>% print
dogsCount<-dogsCount %>% group_by(vilNameAlt) %>%
              summarise(MonthsCount=n(),minDogs=min(nDogs,na.rm=T),maxDogs=max(nDogs,na.rm=T),medDogs=median(nDogs,na.rm=T),months=paste0(Month,collapse = ' '),nDogs=paste0(nDogs,collapse = ' ')) %>%  print

#merge dogs count with village locations
VilBuffers<-VilBuffers %>% mutate(vilNameAlt=tolower(gsub('\\s','',Village))) %>% print

VilBufDogs<-VilBuffers %>% left_join(dogsCount,by = c("vilNameAlt")) %>%
                dplyr::select("Village","intersects","vilNameAlt","minDogs",
                       "maxDogs","medDogs","nDogs",intersects) %>% print
remove(VilBuffers)
#understanding the range of difference in dogcounts in each villages
quantile(VilBufDogs$maxDogs - VilBufDogs$minDogs,seq(0,1,.05),na.rm=T)
#merge dog cases
# casesCount<-as_tibble(read.xlsx('chad_fbz_clinical_trial1.xlsx',sheet='Cases'))%>% 
#   rename(Province=`<U+FEFF>Province`,totalCas2021=`Total.2021`) %>% 
#   dplyr::select(Province,District, Village,Quartier ,totalCas2021) %>% distinct() %>%
#   mutate(vilNameAlt=tolower(gsub('\\s','',Village)))# %>%print
# 
# #groupby to combine quartir counts
# casesCount<- casesCount %>% group_by(vilNameAlt) %>% summarise(totalCas2021=sum(totalCas2021,na.rm=T))
# sorVilDogsCas<-sorVilDogs %>% left_join(casesCount,by = c("vilNameAlt")) %>%
#   dplyr::select("Village","intersects","vilNameAlt","minDogs",
#                 "maxDogs","medDogs",'totalCas2021',"nDogs",minNeighDistance,intersects) 
# 
# #second case file
# casesCount<-as_tibble(read.xlsx('chad_fbz_clinical_trial.xlsx',sheet='Cases'))%>% 
#   dplyr::select(-Grand.Total) %>% distinct() %>%
#   mutate(vilNameAlt=tolower(gsub('\\s','',Village))) %>%print
# 
# #groupby to combine quartir counts
# casesCount<-
#   casesCount %>% group_by(vilNameAlt) %>%  summarise(Cases2019 =sum(Cases2019 ,na.rm=T),Cases2020 =sum(Cases2020 ,na.rm=T))
# 
# VilBufDogs<-sorVilDogsCas %>% left_join(casesCount,by = c("vilNameAlt")) %>%
#   dplyr::select("Village","intersects","vilNameAlt","minDogs",
#                 "maxDogs","medDogs",totalCas2021,Cases2019,Cases2020,minNeighDistance,intersects,nDogs) 

#VilBufDogs %>% dplyr::select(Village,minNeighDistance,intersects,medDogs) %>% st_drop_geometry 
#mapview(final,label='Village',zcol="medDogs")+mapview(centroids) + mapview(final,label='Village',zcol="intersects") + mapView(vilGpsSub)

VilBufDogs.copy<-VilBufDogs

#village with dog count and >100 dog count
removedVillages <- VilBufDogs.copy %>% filter(medDogs>=100 | is.na(medDogs))
#filter only applicable and with dogs <103 #filters 61 village
#set to 102 to include one village with name "loumia centre"
VilBufDogs <- VilBufDogs.copy %>% filter(medDogs<103)


#ggplot(VilBufDogs) + geom_sf(aes(fill=medDogs)) + 
  #theme_bw() + theme(text = element_text(size=14),legend.position=c(0.9, 0.8))
####----merging villages that overlap to form groups----
#remove two villages that have wrong locations
VilBufDogs<-VilBufDogs %>% filter(vilNameAlt!='guelendengcentre' & vilNameAlt!='wadjika')
#first lets form a collapsed polygon
unionVillages<- st_union(VilBufDogs) %>% st_cast(to='POLYGON') %>% 
                          data.frame %>% mutate(groupId=row_number()) %>% st_as_sf
unionVillages$groupId<-rank(st_coordinates(st_centroid(unionVillages$geometry))[,'Y'])

#spatial join to determine clusters
VilBufGroup<-st_join(VilBufDogs,unionVillages)
VilBufGroup <- VilBufGroup %>% mutate(groupId=as.factor(groupId))
remove(unionVillages)
VilBufGroup<-VilBufGroup %>% mutate(villageId=row_number()) %>% relocate(villageId,.before = Village)
VilBufGroup$villageId<-rank(st_coordinates(st_centroid(VilBufGroup$geometry))[,'Y'])

#created merged villages shape for display
mergedVils<-VilBufGroup %>% group_by(groupId) %>% summarise(VilsCount=n(),dogsCount=sum(medDogs)) %>% st_cast(to='POLYGON')
#ggplot(mergedVils) + geom_sf(aes(fill=groupId))

ggplot(VilBufGroup) + geom_sf(aes(fill=as.factor(groupId))) + scale_fill_manual(values=distinctColorPalette(22)) +
geom_sf_text(data=st_centroid(mergedVils),aes(label=groupId),position = position_dodge(0.9),colour = "black",fontface = "bold",size=5) +
theme_bw() + theme(text = element_text(size=14),legend.position='none')

# mapView(VilBufGroup,zcol='groupId',col.regions=distinctColorPalette(21))@map %>%
#   addLabelOnlyMarkers(label =~groupId,data = st_transform(st_centroid(VilBufGroup),st_crs(4326)),
#                       labelOptions = labelOptions(noHide = T,
#                                                   direction = 'top',
#                                                   textOnly = T,textsize = 20))

#group villages as per group id and summarize dog counts
groupedVil<-VilBufGroup %>% group_by(groupId) %>% summarise(count=n(),dogsCount=sum(medDogs)) %>% st_drop_geometry %>%
                arrange(dogsCount)
#st_write(VilBufGroup,'temp.gpkg',delete_layer = TRUE)



##---- checking number of possible combinations ----
comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}

groupedVil %>% arrange(dogsCount) %>% select(dogsCount) %>% cumsum  %>% print(n=21)
groupedVil %>% arrange(-dogsCount) %>% select(dogsCount) %>% cumsum

#we can have starting from 4 groups upto 17 groups. 4 -because adding the 4 
#large groups (as per dog counts) adds ups to 1000 and similarly adding 17 small groups adds to 1000
#total number of possible combiations = 22C4 to 22C17 which is equal to 22C4 to 22C10
#iterative function to compute this
tot_comb=0.0
for(i in 4:9){
  tot_comb = tot_comb  + comb(19,i)
}
print(tot_comb)

#####---- generate all combinations ----
#function defenition
absDiff<-function(treats,cntrls){
  if(nrow(treats)>=nrow(cntrls)){
    treatDogs<-sort(treats$dogsCount)
    cntrlDogs<-sort(c(cntrls$dogsCount,rep(0,nrow(treats)-nrow(cntrls))))
  }else{
    treatDogs<-sort(c(treats$dogsCount,rep(0,nrow(cntrls)-nrow(treats))))
    cntrlDogs<-sort(cntrls$dogsCount)
  }
  return(sum(abs(treatDogs-cntrlDogs)))
}

groupIds<-as.integer(groupedVil$groupId)
combsList<-list()
for(i in 4:9){ #because 19C10 == 19C9 and so on
  combsList<- c(combsList,combn(groupIds,i,simplify = F))
}

combsDf<-data.frame()
i<-1
counter<-0
for(treatsList in combsList){ 
  counter<- counter+1
  if (counter%%1000 ==0) print(counter)
  #assign rest to controls
  cntrls<-groupedVil[!(groupedVil$groupId %in% treatsList),] %>% arrange(groupId)
  treats<-groupedVil[groupedVil$groupId %in% treatsList,] %>% arrange(groupId)
  #skip this combination if the dog count difference is greater than 30% of total dogs (=635)
  if(abs(sum(treats$dogsCount)-sum(cntrls$dogsCount))> 635) next
  #skip combination with village count diff greater than 20
  if(abs(sum(treats$count)-sum(cntrls$count))> 20) next
  
  #creat entry in the dataframe
  combsDf[i,c('treatDogs','ctrlDogs',
              'treatVils','ctrlVills',
              'ntreatGroups','nctrlGroups')]<-c(sum(treats$dogsCount), sum(cntrls$dogsCount),
                                                sum(treats$count),sum(cntrls$count),
                                                nrow(treats),nrow(cntrls))
  #compute abs dog count difference
  combsDf[i,'absDogDiff']=absDiff(treats,cntrls)
  
  #preserve the groups
  combsDf[i,'treatGroups'][[1]]<- list(sort(treats$groupId))
  combsDf[i,'ctrlGroups'][[1]]<- list(sort(cntrls$groupId))
  i<-i+1
  #print(c('i',i))
}

#difference in dogs and villages
combsDf[,'dogsDiff']<-abs(combsDf$treatDogs-combsDf$ctrlDogs)
combsDf[,'vilsDiff']<-abs(combsDf$treatVils-combsDf$ctrlVills)
combsDf[,'grpDiff']<-abs(combsDf$ntreatGroups-combsDf$nctrlGroups)

combsDf<-combsDf %>% as_tibble %>% mutate(rowId=row_number())
save(combsDf,file='Robjects//combsDf.Rdata')

####---- extract only required combinations ----
summary(combsDf %>% select(vilsDiff,grpDiff,dogsDiff,absDogDiff))
quantile(combsDf$dogsDiff,seq(0,1,.05))
subCombsDf<-combsDf %>% filter(vilsDiff==0,grpDiff<=1,dogsDiff<=136)

#check village wise abs dog difference
VilBufGroupSub<- VilBufGroup %>% select(Village,medDogs,groupId) %>% rename(dogsCount=medDogs)
subCombsDf$VilabsDogDiff<-subCombsDf %>% apply(1,function(rec){
  treats<-VilBufGroupSub[VilBufGroupSub$groupId %in% as.integer((unlist(rec['treatGroups']))),] 
  cntrls<-VilBufGroupSub[VilBufGroupSub$groupId %in% as.integer((unlist(rec['ctrlGroups']))),]
  return(absDiff(treats,cntrls))
})
remove(VilBufGroupSub)

summary(subCombsDf[,c('VilabsDogDiff','absDogDiff',"dogsDiff")])

####---- spatial structure analyse ---- 
library(spdep)
nb3<- knn2nb(knearneigh(st_centroid(mergedVils), k=3))
nb2<- knn2nb(knearneigh(st_centroid(mergedVils), k=2))
plot(mergedVils$geometry,axes=T)
plot(nb3, st_centroid(mergedVils$geometry), add=TRUE, pch=".")

subCombsDf[,c('jc3True','jc3False','jc3TrueP','jc3FalseP')]<-t(apply(subCombsDf,1,function(x){
  treatGrps<-as.numeric(unlist(x['treatGroups']))
  mergedVils$treatment<-F
  mergedVils$treatment[mergedVils$groupId %in% treatGrps]<-T
  jcRes<-joincount.test(as.factor(mergedVils$treatment),listw2U(nb2listw(nb3,style='B')),alternative = 'greater')
  return(c(jcRes[[2]]$estimate[1],jcRes[[1]]$estimate[1],jcRes[[2]]$p.value,jcRes[[1]]$p.value))
}))

# subCombsDf[,c('jc2False','jc2True')]<-t(apply(subCombsDf,1,function(x){
#   treatGrps<-as.numeric(unlist(x['treatGroups']))
#   mergedVils$treatment<-F
#   mergedVils$treatment[mergedVils$groupId %in% treatGrps]<-T
#   jcRes<-joincount.mc(as.factor(mergedVils$treatment),listw2U(nb2listw(nb2,style='B')),nsim =100)
#   return(c(jcRes[[1]]$p.value,jcRes[[2]]$p.value))
# }))
save(subCombsDf,file='RObjects//subCombsDf.RData')

####---- plot the randomly selected combination after 52 filtered based on p value ----####
subCombsDf %>% filter(jc3TrueP>0.9,jc3FalseP>0.9) %>% select(rowId) %>% sample_n(1,replace = T)

mergedVils$treatment<-F
mergedVils$treatment[mergedVils$groupId %in% unlist(subCombsDf[subCombsDf$rowId==135481  ,'treatGroups'])]<-T
ggplot(mergedVils) + geom_sf(aes(fill=treatment)) +
  geom_sf_text(data=st_centroid(mergedVils),aes(label=round(dogsCount)),position = position_dodge(0.9),size=4) +
  geom_sf_text(aes(label=groupId),size=4,color='blue', nudge_x=5e3,nudge_y = 5e3) +
  theme_bw() + theme(text = element_text(size=14),legend.position='bottom')

#assign treatment and control to villlages
VilBufGroup$Treatment<-F
VilBufGroup$Treatment[VilBufGroup$groupId %in% unlist(subCombsDf[subCombsDf$rowId==135481  ,'treatGroups'])]<-T
(mapView(VilBufGroup,zcol='Treatment',col.regions=distinctColorPalette(2),label='villageId') + mapView(VilBufGroup,zcol='Team',col.regions=c('red','green'),label='villageId'))@map %>%
  addLabelOnlyMarkers(label =~medDogs ,data = st_transform(st_centroid(VilBufGroup),st_crs(4326)),
                      labelOptions = labelOptions(noHide = T,
                                                  direction = 'top',
                                                  textOnly = T,textsize = 20))


####---- assign teams ----
VilBufGroup$Team<-'A'
VilBufGroup$Team[as.numeric(VilBufGroup$villageId)<30]<-'B'
VilBufGroup$Team[VilBufGroup$villageId %in% c(29,28,27)]<-'A'
VilBufGroup %>% st_drop_geometry %>% group_by(Team,Treatment) %>% summarise(villages=n(),groups=n_distinct(groupId),medDogs=sum(medDogs))

(mapView(VilBufGroup[!VilBufGroup$Treatment,],zcol='Team',col.regions=c('red','green'),label='villageId'))@map %>%
  addLabelOnlyMarkers(label =~medDogs ,data = st_transform(st_centroid(VilBufGroup),st_crs(4326)),
                      labelOptions = labelOptions(noHide = T,
                                                  direction = 'top',
                                                  textOnly = T,textsize = 20))
####---- final map ----
VilCentroids<-st_centroid(VilBufGroup) %>% st_transform(crs=4326)
basemap <- get_stamenmap(
  bbox = unname(st_bbox(st_buffer(VilCentroids,10000))),
  zoom = 10,
  maptype = 'toner-lite'
  #color = "bw"
)

ggmap(basemap) + 
  geom_sf(data=st_buffer(VilCentroids,3000),aes(fill=Treatment,color=Team),size = 1,alpha = 0.7,inherit.aes = FALSE) +
  geom_sf_text(data=VilCentroids,aes(label=villageId),size=2,inherit.aes = FALSE) +
  geom_sf_text(data=st_centroid(st_transform(mergedVils,crs=4326)),aes(label=groupId),color='#C04000', nudge_x = 0.04,nudge_y = 0.03,inherit.aes = FALSE)+
  north(VilCentroids,scale = 0.07) + scalebar(VilCentroids,height = 0.015,dist = 20, dist_unit = "km",transform = TRUE, model = "WGS84", location = 'bottomleft',st.size = 4)+
  scale_fill_manual(values=c("#999999", "#E69F00"))+scale_color_manual(values=c('#0072B2','#009E73'))+
  theme_bw() + theme(legend.position="bottom")

####---- save objects ----
#save map
ggsave('TreatmentTeamMap.pdf',width=20,height=25,units='cm',dpi = 300)


#save df
VilCentroids %>% select(villageId,Village,groupId,Treatment,Team,minDogs,medDogs,maxDogs) %>% 
  mutate(latitude=st_coordinates(geometry)[,2],longitude=st_coordinates(geometry)[,1]) %>% st_drop_geometry() %>% write.csv('FinalTreatCntrlVil.csv',row.names=F)

#save combinations from which a random was taken
subCombsDf %>% filter(jc3TrueP>0.9,jc3FalseP>0.9) %>%
  save(file = "FinalCombinations.Rdata")


##---- individual maps
MajorPlaces<-st_read('qgis_layers.gpkg',layer = 'MajorPlaces')

filename<-'TreatmentB'
subVilCentroids<- VilCentroids %>% filter(Treatment==T, Team=='B') %>% mutate(latitude=st_coordinates(geometry)[,2],longitude=st_coordinates(geometry)[,1])
subVilCentroids %>% select(villageId,Village,groupId,Treatment,Team,minDogs,medDogs,maxDogs) %>% 
  st_drop_geometry() %>% write.csv(paste0(filename,'.csv'),row.names=F)

basemap <- get_stamenmap(
  bbox = unname(st_bbox(st_buffer(subVilCentroids,15000))),
  zoom = 10,
  maptype = 'toner-lite'
  #color = "bw"
)

ggmap(basemap) + 
  geom_sf_text(data=MajorPlaces,aes(label=Name),color='#7D7D7D',size=5, inherit.aes = FALSE) +
  geom_sf(data=st_buffer(subVilCentroids,1000),aes(fill=Treatment,color=Team),size = 1,alpha = 0.7,inherit.aes = FALSE) +
  geom_sf_text(data=subVilCentroids,aes(label=villageId),size=4,color='#C04000', nudge_x = 0.03,inherit.aes = FALSE) +
  scalebar(st_buffer(subVilCentroids,10000),height = 0.01,dist = 10, dist_unit = "km",transform = TRUE, model = "WGS84", location = 'bottomleft',st.size = 3)+
  scale_fill_manual(values=c("#E69F00", "#E69F00"))+scale_color_manual(values=c('#0072B2','#009E73'))+
  theme_bw() + theme(legend.position="none")
ggsave(paste0(filename,'.pdf'),width=25,height=15,units='cm',dpi = 300)
