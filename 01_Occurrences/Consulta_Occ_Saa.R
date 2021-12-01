install.packages("devtools")
library(devtools)
install_github("iobis/robis")
library(robis)
library(spocc)
library(Rtools)
library(robis)
library(raster)
library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(EDAWR)
library(lattice)
library(mapview)


######Descargar Datos de GBIF, VerNet, OBIS, iNaturalist, Berkeley Ecoengine, Biodiversity Information Serving Our Nation (BISON), iDigBio#####
spp= c("Stenella attenuata")
occurrence = spocc::occ(spp, from = c('gbif', 'vertnet','inat','ecoengine','bison','obis','idigbio'), limit = 50000)
head(occurrence)

Occurrencias=occ2df(occurrence)
Occurrencias_df<-as.data.frame(Occurrencias)
Occurrencias_df$name<-as.factor(Occurrencias_df$name)
levels(Occurrencias_df$name)
Saa_Total<- subset(Occurrencias_df, name== "Stenella attenuata attenuata" | name== "Stenella attenuata"| name== "Stenella attenuata (c/f)"| name== "Stenella attenuata (Gray, 1846)"| name== "Stenella attenuata Gray, 1846"| name== "stenella attenuata?"| name== "STENELLA ATTENUATA"| name== "Stenella attenuata (Gray 1846)")

#Consulta en el OBIS
occu_obis<- robis::occurrence("Stenella attenuata")
occu_obis_df<-as.data.frame(occu_obis)
seleccion_obis <- dplyr::select(occu_obis_df, scientificName,  decimalLongitude, decimalLatitude, occurrenceStatus,eventDate, occurrenceID )
occu_obis_modif<-`colnames<-`(seleccion_obis, c("name",    "longitude", "latitude",  "prov", "date", "key"))
occu_obis_modif$prov<-"obis"
occu_total<-rbind(Saa_Total, occu_obis_modif)
occu_total$date<-as.Date(occu_total$date)
occu_total$month<-as.numeric(format(as.Date(occu_total$date), "%m"))
occu_total$year<-as.numeric(format(as.Date(occu_total$date), "%Y"))

#######Ocurrencias por temporadas#####
#Invierno
Saa_occ_Inv<-subset(occu_total, occu_total$month >0 & occu_total$month < 5)
Saa_occ_Inv<-na.omit(Saa_occ_Inv)
Saa_occ_Inv_nodupli<-duplicated(Saa_occ_Inv)
Saa_occ_Inv_nodupli<-Saa_occ_Inv[!Saa_occ_Inv_nodupli,]
Saa_occ_Inv_nodupli$longitude<-as.numeric(Saa_occ_Inv_nodupli$longitude)
Saa_occ_Inv_nodupli$latitude<-as.numeric(Saa_occ_Inv_nodupli$latitude)
#Verano
Saa_occ_Ver<-subset(occu_total, occu_total$month >4)
Saa_occ_Ver<-na.omit(Saa_occ_Ver)
Saa_occ_Ver_nodupli<-duplicated(Saa_occ_Ver)
Saa_occ_Ver_nodupli<-Saa_occ_Ver[!Saa_occ_Ver_nodupli,]
Saa_occ_Ver_nodupli$longitude<-as.numeric(Saa_occ_Ver_nodupli$longitude)
Saa_occ_Ver_nodupli$latitude<-as.numeric(Saa_occ_Ver_nodupli$latitude)


install.packages("spThin")
library(spThin)

#####Remover los segos de los datos#####

#Verano
thin(Saa_occ_Ver_nodupli,
     lat.col = "latitude",
     long.col = "longitude",
     spec.col = "name",
     thin.par=10,
     reps=10,
     locs.thinned.list.return = TRUE,
     write.files = TRUE,
     max.files = ,
     out.dir="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano\\Saa_Occ_Bias_Ver",
     out.base = "thinned_data",
     write.log.file = TRUE,
     log.file = "spatial_thin_log.txt",
     verbose = TRUE)
thinned<-list.files("E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano\\Saa_Occ_Bias_Ver", pattern="*.csv")

rm(thinned[1])

plotThin(thinned,which = c(1:3),ask = prod(par("mfcol")) < length(which) && dev.interactive())

#Invierno
thin(Saa_occ_Inv_nodupli,
     lat.col = "latitude",
     long.col = "longitude",
     spec.col = "name",
     thin.par=10,
     reps=10,
     locs.thinned.list.return = TRUE,
     write.files = TRUE,
     max.files = ,
     out.dir="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Inv",
     out.base = "thinned_data",
     write.log.file = TRUE,
     log.file = "spatial_thin_log.txt",
     verbose = TRUE)
thinned<-list.files("E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Inv", pattern="*.csv")

rm(thinned[1])

plotThin(thinned,which = c(1:3),ask = prod(par("mfcol")) < length(which) && dev.interactive())


#####Crear las ocurrencias#####

#Invierno
Saa_Inv<-read.csv(file="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Inv\\Saa_Occ2.csv", sep=',')
Saa_occ_Inv<-Saa_Inv[,2:3]
Saa_occ_Inv<-na.omit(Saa_occ_Inv)
Saa_occ_Inv <- Saa_occ_Inv[,c('longitude','latitude')]
Saa_occ_Inv$longitude<-as.numeric(Saa_occ_Inv$longitude)
Saa_occ_Inv$latitude<-as.numeric(Saa_occ_Inv$latitude)
Saa_occ_Inv$species <- 1
sp::coordinates(Saa_occ_Inv) <- ~ longitude + latitude
proj4string(Saa_occ_Inv)<-projection(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
proj4string(Saa_occ_Inv)
mapview(Saa_occ_Inv)
Saa_occ_Inv <- spTransform(Saa_occ_Inv,
                           crs("+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
mapview(Saa_occ_Inv)

Pacifico_pelagico<-readOGR("Pacifico_pelagico.shp")
mapview(Pacifico_pelagico)
mapview(Saa_occ_winkel_Pac)

#Estas son las ocurrencias Finales
Saa_occ_Inv_winkel<-raster::crop(Saa_occ_Inv, Pacifico_AE)
mapview(Saa_occ_Inv_winkel)
writeOGR(Saa_occ_Inv_winkel, "E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Ver", "Saa_occ_Inv_winkel", driver="ESRI Shapefile")


#Verano
Saa_Ver<-read.csv(file="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano\\Saa_Occ_Bias_Ver\\thinned_data_thin1.csv", sep=',')
Saa_occ_Ver<-Saa_Ver[,2:3]
Saa_occ_Ver<-na.omit(Saa_occ_Ver)
Saa_occ_Ver <- Saa_occ_Ver[,c('longitude','latitude')]
Saa_occ_Ver$longitude<-as.numeric(Saa_occ_Ver$longitude)
Saa_occ_Ver$latitude<-as.numeric(Saa_occ_Ver$latitude)
Saa_occ_Ver$species <- 1
sp::coordinates(Saa_occ_Ver) <- ~ longitude + latitude
proj4string(Saa_occ_Ver)<-projection(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
proj4string(Saa_occ_Ver)
mapview(Saa_occ_Ver)
Saa_occ_Ver <- spTransform(Saa_occ_Ver,
                           crs("+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))


mapview(Saa_occ_Ver)

#Estas son las ocurrencias Finales
Saa_occ_Ver_winkel<-raster::crop(Saa_occ_Ver, Pacifico_pelagico)
mapview(Saa_occ_Ver_winkel)
writeOGR(Saa_occ_Ver_winkel, "E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano\\Occ_Bias_Ver", "Saa_occ_Ver_winkel", driver="ESRI Shapefile")


