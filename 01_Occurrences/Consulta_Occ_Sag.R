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

#if(!dir.exists("D:\\DOCUMENTOS\\2020\\Articulo_Pinnipedos\\02_Resultados")) dir.create("D:\\DOCUMENTOS\\2020\\Articulo_Pinnipedos\\02_Resultados")
getwd()
setwd("D:\\DOCUMENTOS\\2020\\Articulo_Pinnipedos\\02_Resultados") #######################Escribir la ruta deseada donde se guardaran los archivos.


######Descargar Datos de GBIF, VerNet, OBIS, iNaturalist, Berkeley Ecoengine, Biodiversity Information Serving Our Nation (BISON), iDigBio#####
spp= c("Stenella attenuata graffmani")
occurrence = occ(spp, from = c('gbif', 'vertnet','inat','ecoengine','bison','obis','idigbio'), limit = 50000)
head(occurrence)

Occurrencias=occ2df(occurrence)
Occurrencias_df<-as.data.frame(Occurrencias)
Occurrencias_df$name<-as.factor(Occurrencias_df$name)
levels(Occurrencias_df$name)
Sag_Total<- subset(Occurrencias_df, name== "Stenella attenuata graffmani"| name== "Stenella attenuata graffmani (Lönnberg, 1934)"| name=="stenella attenuata graffmani")

#Consulta en el OBIS
occu_obis<- occurrence("Stenella attenuata graffmani")
occu_obis_df<-as.data.frame(occu_obis)
seleccion_obis <- dplyr::select(occu_obis_df, scientificName,  decimalLongitude, decimalLatitude, occurrenceStatus,eventDate, occurrenceID )
occu_obis_modif<-`colnames<-`(seleccion_obis, c("name",    "longitude", "latitude",  "prov", "date", "key"))
occu_obis_modif$prov<-"obis"
occu_total<-rbind(Sag_Total, occu_obis_modif)
occu_total$date<-as.Date(occu_total$date)
occu_total$month<-as.numeric(format(as.Date(occu_total$date), "%m"))
occu_total$year<-as.numeric(format(as.Date(occu_total$date), "%Y"))

#######Ocurrencias por temporadas#####
#Invierno
Sag_occ_Inv<-subset(occu_total, occu_total$month >0 & occu_total$month < 5)
Sag_occ_Inv<-na.omit(Sag_occ_Inv)
Sag_occ_Inv_nodupli<-duplicated(Sag_occ_Inv)
Sag_occ_Inv_nodupli<-Sag_occ_Inv[!Sag_occ_Inv_nodupli,]
Sag_occ_Inv_nodupli$longitude<-as.numeric(Sag_occ_Inv_nodupli$longitude)
Sag_occ_Inv_nodupli$latitude<-as.numeric(Sag_occ_Inv_nodupli$latitude)
#Verano
Sag_occ_Ver<-subset(occu_total, occu_total$month >4)
Sag_occ_Ver<-na.omit(Sag_occ_Ver)
Sag_occ_Ver_nodupli<-duplicated(Sag_occ_Ver)
Sag_occ_Ver_nodupli<-Sag_occ_Ver[!Sag_occ_Ver_nodupli,]
Sag_occ_Ver_nodupli$longitude<-as.numeric(Sag_occ_Ver_nodupli$longitude)
Sag_occ_Ver_nodupli$latitude<-as.numeric(Sag_occ_Ver_nodupli$latitude)


install.packages("spThin")
library(spThin)

#####Remover los segos de los datos#####

#Verano
thin(Sag_occ_Ver_nodupli,
     lat.col = "latitude",
     long.col = "longitude",
     spec.col = "name",
     thin.par=10,
     reps=10,
     locs.thinned.list.return = TRUE,
     write.files = TRUE,
     max.files = ,
     out.dir="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Ver",
     out.base = "thinned_data",
     write.log.file = TRUE,
     log.file = "spatial_thin_log.txt",
     verbose = TRUE)
thinned<-list.files("E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Ver", pattern="*.csv")

rm(thinned[1])

plotThin(thinned,which = c(1:3),ask = prod(par("mfcol")) < length(which) && dev.interactive())

#Invierno
thin(Sag_occ_Inv_nodupli,
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
Sag_Inv<-read.csv(file="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Inv\\SAG_Occ2.csv", sep=',')
Sag_occ_Inv<-Sag_Inv[,2:3]
Sag_occ_Inv<-na.omit(Sag_occ_Inv)
Sag_occ_Inv <- Sag_occ_Inv[,c('longitude','latitude')]
Sag_occ_Inv$longitude<-as.numeric(Sag_occ_Inv$longitude)
Sag_occ_Inv$latitude<-as.numeric(Sag_occ_Inv$latitude)
Sag_occ_Inv$species <- 1
sp::coordinates(Sag_occ_Inv) <- ~ longitude + latitude
proj4string(Sag_occ_Inv)<-projection(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
proj4string(Sag_occ_Inv)
mapview(Sag_occ_Inv)
Sag_occ_Inv <- spTransform(Sag_occ_Inv,
                           crs("+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
mapview(Sag_occ_Inv)

Pacifico_AE<-readOGR("Pacifico_Winkel.shp")
mapview(Pacifico_AE)
mapview(Sag_occ_winkel_Pac)

#Estas son las ocurrencias Finales
Sag_occ_Inv_winkel<-raster::crop(Sag_occ_Inv, Pacifico_AE)
mapview(Sag_occ_Inv_winkel)
    writeOGR(Sag_occ_Inv_winkel, "E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_invierno\\Occ_Bias_Ver", "Sag_occ_Inv_winkel", driver="ESRI Shapefile")


#Verano
Sag_Ver<-read.csv(file="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano\\Occ_Bias_Ver\\thinned_data_thin1.csv", sep=',')
Sag_occ_Ver<-Sag_Ver[,2:3]
Sag_occ_Ver<-na.omit(Sag_occ_Ver)
Sag_occ_Ver <- Sag_occ_Ver[,c('longitude','latitude')]
Sag_occ_Ver$longitude<-as.numeric(Sag_occ_Ver$longitude)
Sag_occ_Ver$latitude<-as.numeric(Sag_occ_Ver$latitude)
Sag_occ_Ver$species <- 1
Sag_coor<-matrix(data=c(Sag_occ_Ver$longitude,Sag_occ_Ver$latitude), nrow =213 , ncol = 2)
Sag_data<-as.data.frame(cbind(Sag_occ_Ver$species, Sag_occ_Ver$longitude,Sag_occ_Ver$latitude))
colnames(Sag_data)<-c("species", "longitude", "latitude")
lo<-SpatialPointsDataFrame(Sag_coor, Sag_data,  match.ID = TRUE)
proj4string(lo)<-projection(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
Sag_occ_Ver <- spTransform(lo,
                           crs("+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
mapview(Pacifico_AE)

#Estas son las ocurrencias Finales
Sag_occ_Ver_winkel<-raster::crop(Sag_occ_Ver, Pacifico_AE)
mapview(Sag_occ_Ver_winkel)
writeOGR(Sag_occ_Ver_winkel, "E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano\\Occ_Bias_Ver", "Sag_occ_Ver_winkel", driver="ESRI Shapefile")


