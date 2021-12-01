#####An?lisis de Idoneidad de H?bitat para la especies Mustelirallus erythrops
#Fecha 17/05/2020
#Autores: Christian Berm?dez-Rivas, Maria del Pilar Aguirre-Tapiero


#####Instalaci?n de paquetes requeridos
install.packages("shiny")
install.packages("uuid")
install.packages("MigClim")
install.packages("Rtools")
install.packages("Rcpp")
install.packages("rJava")
install.packages("parallel")
#for java-home
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-10.0.1/")

#C:\Program Files\Java\jre1.8.0_271
#C:\Program Files (x86)\Java

#####Cargar los paquetes requeridos

library(dismo)
library(raster)
library(sp)
library(mapview)
library(sdm)
library(Rcpp)
library(usdm)
library(shiny)
library(sf)
library(ggplot2)
library(maptools)
library(sp)
library(raster)
library(rgeos)
library(dismo)
library(ENMeval)
library(rJava)
library(SSDM)
library(usdm)
library(sdm)
library(plyr)
library(rgdal)
library(MigClim)
library(shiny)
library(uuid)
library(readr)
library(gridExtra)
#####Cargar todos los programas de SDM

installAll()
#####Establecer el directorio de trabajo
getwd()


#####Cargar los datos de Ocurrencia y transformarlos a un objeto geogr?fico
#Total
Sa<-read.csv("Occ_Pa_In.csv", header = T)
Sa_occ<-Sa[,9:10]
Sa_occ<-na.omit(Sa_occ)
Sa_occ <- Sa_occ[,c('Lon','Lat')]
Sa_occ$species <- 1
coordinates(Sa_occ) <- ~ Lon + Lat


proj4string(Sa_occ)<-projection(CRS("+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

proj4string(Sa_occ)

mapview(Sa_occ)



#####


#Predictores
#Pelagicos
Pelagicos_list<- list.files(path="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano", pattern = ".tif$", full.names = TRUE)
nombres_base<-basename(Pelagicos_list)
Pelagicos <- raster::stack(Pelagicos_list)
Pelagicos<-dropLayer(Pelagicos, c(2,5))

#Costeros
Costeros_list<- list.files(path="E:\\Pasantias\\Tesis_Laura\\Predictores_ambientales_Laura\\Pacifico_verano", pattern = ".tif$", full.names = TRUE)
nombres_base2<-basename(Costeros_list)
Costeros <- raster::stack(Costeros_list)
Costeros<-dropLayer(Costeros, c(4,5))



CCC<-readOGR("CCC.shp")
CPC<-readOGR("CPC.shp")
Paises<-readOGR("Paises_HD2.shp")
Cuenca_Caribe<-readOGR("Cuenca_Caribe.shp")
Cuenca_Pacifica<-readOGR("Cuenca_Pacifica.shp")

options(scipen = 99999)

#Transformar los datos del stack
bat<-Costeros$Bathymetry
DF_bat<-as.data.frame(bat, xy=TRUE)%>%drop_na()
bahty__plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_bat, aes(x=x, y=y, fill=Bathymetry)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = -2000) +
  coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))


Micro<-Costeros$Micronekton_mean
DF_Micro<-as.data.frame(Micro, xy=TRUE)%>%drop_na()
Micro__plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_Micro, aes(x=x, y=y, fill=Micronekton_mean)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = 2) +
 coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))

Mix_layer<-Costeros$Mix_layer_std
DF_Mix_layer<-as.data.frame(Mix_layer, xy=TRUE)%>%drop_na()
Mix_layer_plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_Mix_layer, aes(x=x, y=y, fill=Mix_layer_std)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = 4.5) +
  coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))



Transp<-Costeros$Transp_mean
DF_Transp<-as.data.frame(Transp, xy=TRUE)%>%drop_na()
Transp_plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_Transp, aes(x=x, y=y, fill=Transp_mean)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = 14.5) +
coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))


grafica_predictores_Costeros<-grid.arrange(bahty__plot, Micro__plot, Mix_layer_plot,Transp_plot,  nrow = 1)

#Cargue del Background
Sag_BG<- read_csv("Sag_bg.csv")
Sag_BG2<-cbind(Sag_BG$Longitude, Sag_BG$Latitude)
Sag_BG2<-as.data.frame(Sag_BG2)
colnames(Sag_BG2)<-c("longitude","latitude")
coordinates(Sag_BG2)<- ~ longitude + latitude
proj4string(Sag_BG2)<-projection(CRS("+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
rasValue=raster::extract(Costeros, Sag_BG2)

Sag_BG2<-as.data.frame(Sag_BG2)
colnames(Sag_BG2)<-c("longitude","latitude")
Sag_BG2=cbind(Sag_BG2,rasValue)


########Total

Sag_Entrenamiento<- sdmData(species~., train=Sag_occ_Ver_winkel, predictors= Costeros, bg=Sag_BG2)

system.time(Sag_modelamiento_maxent<- sdm::sdm(species ~ . , Sag_Entrenamiento, methods=c('maxent'), replicatin='cv', test.percent=30,n=10, parallelSettings=list(ncore=5, method="parallel")))
system.time(Sag_maxent<- sdm::ensemble(Sag_modelamiento_maxent, Costeros, filename = 'Sag_maxent_cv_bg_COR9km.img',  nc=7,setting=list(method='weighted',stat='COR', weights=1:50, id=1:50,wtest='test.dep'), overwrite=TRUE))
plot(Sag_maxent)
sdm::gui(Sag_modelamiento_maxent)

options(scipen=999)

####################Grafica para las variables
tiff(filename = "Sa_rcurve9km.tif", res = 300, width = 9000, height = 3000, units = "px", pointsize = 12, compression = "lzw")
rcurve(Sag_modelamiento_maxent)
dev.off()
getVarImp(Sag_modelamiento_maxent)

tiff(filename = "Sa_getVarImp9km.tif", res = 300, width = 5000, height = 2500, units = "px", pointsize = 12, compression = "lzw")
plot(getVarImp(Sag_modelamiento_maxent), main = substitute(paste(italic('Stenella attenuata'))), 'cor', col = "steelblue")
dev.off()
evl_pred_costero<-getVarImp(Sag_modelamiento_maxent,id=1:50,wtest='test.dep')

library(gridExtra)

dos<-rcurve(Sag_modelamiento_maxent)
uno<-plot(evl_pred_costero, main = substitute(paste(italic('Stenella attenuata graffmani'))), 'cor', col = "steelblue")

tiff(filename = "Sa_predictors_evaluation.tif", res = 300, width = 45, height = 20, units = "cm", pointsize = 20, compression = "lzw")
grid.arrange(uno, dos,grafica_predictores_Costeros,  nrow = 3)
dev.off()

#######Evaluaci?n del modelo: las opt se refieren a la opci?n de evaluaci?n basada en el treshold op 1= sp=se opt= max(se+sp)

Sa_evaluacion_maxent<-getEvaluation(Sa_modelamiento_maxent, stat = c("AUC", "TSS", "COR", "threshold", "Deviance", 'sensitivity', 'specificity'), opt=2)
Sa_evaluacion_maxent$specie<-c("Stenella attenuata")
write.table(Sa_evaluacion_maxent, "evaluacion_total_Sa.csv", col.names = TRUE, row.names = FALSE, sep=",")

mean(Sa_evaluacion_maxent$threshold)



#####Saa####

Saa_BG<- read_csv("Saa_BG.csv")
#Cargue del Background
Saa_BG2<-cbind(Saa_BG$longitude, Saa_BG$latitude)
Saa_BG2<-as.data.frame(Saa_BG2)
colnames(Saa_BG2)<-c("longitude","latitude")
coordinates(Saa_BG2)<- ~ longitude + latitude
proj4string(Saa_BG2)<-projection(CRS("+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
rasValue=raster::extract(ZEE_Col, Saa_BG2)

Saa_BG2<-as.data.frame(Saa_BG)
colnames(Saa_BG2)<-c("longitude","latitude")
Saa_BG2=cbind(Saa_BG2,rasValue)


#Transformar los datos del stack
bat_saa<-Pelagicos$Bathymetry
DF_bat_saa<-as.data.frame(bat_saa, xy=TRUE)%>%drop_na()
bahty_saa_plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_bat_saa, aes(x=x, y=y, fill=Bathymetry)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = -3000) +
  coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))


Mix_layer_saa<-Pelagicos$Mix_layer_std
DF_Mix_layer_saa<-as.data.frame(Mix_layer_saa, xy=TRUE)%>%drop_na()
Mix_layer_saa_plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_Mix_layer_saa, aes(x=x, y=y, fill=Mix_layer_std)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = 4) +
  coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))

Temp_saa<-Pelagicos$Temp_mean
DF_Temp_saa<-as.data.frame(Temp_saa, xy=TRUE)%>%drop_na()
Temp_saa_plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_Temp_saa, aes(x=x, y=y, fill=Temp_mean)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = 24.5) +
  coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))

Transp_saa<-Pelagicos$Transp_mean
DF_Transp_saa<-as.data.frame(Transp_saa, xy=TRUE)%>%drop_na()
Transp_saa_plot<-ggplot()+
  geom_polygon(data=Paises, aes(x= long, y= lat, group=group), colour="black", fill="white")+
  geom_raster(data=DF_Transp_saa, aes(x=x, y=y, fill=Transp_mean)) +
  scale_fill_gradient2(low = "black", high = "steelblue", mid = "#dedede", midpoint = 15) +
  coord_sf(xlim = c(-8400000, -5186528 ), ylim = c(-477261.5 , 2713283), expand = FALSE)+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position = "bottom",legend.key.height= unit(1, 'line'),
        legend.key.width= unit(2, 'line'))


grafica_predictores_Pelagicos<-grid.arrange(bahty_saa_plot, Mix_layer_saa_plot,Temp_saa_plot , Transp_saa_plot,  nrow = 1)




Saa_Entrenamiento<- sdmData(species~., train=Saa_occ_Ver_winkel, predictors= Pelagicos, bg=Saa_BG2)

system.time(Saa_modelamiento_maxent<- sdm::sdm(species ~ . , Saa_Entrenamiento, methods=c('maxent'), replicatin='cv', test.percent=30,n=10, parallelSettings=list(ncore=5, method="parallel")))
system.time(Saa_maxent<- sdm::ensemble(Saa_modelamiento_maxent, ZEE_Col, filename = 'Saa4_maxent_cv_bg_COR9km.img',  nc=7,setting=list(method='weighted',stat='COR', weights=1:50, id=1:50,wtest='test.dep'), overwrite=TRUE))
plot(Saa_maxent)
sdm::gui(Saa_modelamiento_maxent)

options(scipen=999)

####################Grafica para las variables
tiff(filename = "Saa_rcurve9km.tif", res = 300, width = 9000, height = 3000, units = "px", pointsize = 12, compression = "lzw")
rcurve(Saa_modelamiento_maxent)
dev.off()
getVarImp(Saa_modelamiento_maxent)

tiff(filename = "Saa_getVarImp9km.tif", res = 300, width = 5000, height = 2500, units = "px", pointsize = 12, compression = "lzw")
plot(getVarImp(Saa_modelamiento_maxent), main = substitute(paste(italic('Stenella attenuata attenuata'))), 'cor', col = "steelblue")
dev.off()
Saa_evl_pred<-getVarImp(Saa_modelamiento_maxent,id=1:50,wtest='test.dep')

library(gridExtra)

dos<-rcurve(Saa_modelamiento_maxent)
uno<-plot(Saa_evl_pred, main = substitute(paste(italic('Stenella attenuata attenuata'))), 'cor', col = "steelblue")

tiff(filename = "Saa_predictors_evaluation.tif", res = 300, width = 45, height = 20, units = "cm", pointsize = 20, compression = "lzw")
grid.arrange(uno, dos,grafica_predictores_Pelagicos, nrow = 3)
dev.off()

#######Evaluaci?n del modelo: las opt se refieren a la opci?n de evaluaci?n basada en el treshold op 1= sp=se opt= max(se+sp)

Sa_evaluacion_maxent<-getEvaluation(Sa_modelamiento_maxent, stat = c("AUC", "TSS", "COR", "threshold", "Deviance", 'sensitivity', 'specificity'), opt=2)
Sa_evaluacion_maxent$specie<-c("Stenella attenuata")
write.table(Sa_evaluacion_maxent, "evaluacion_total_Sa.csv", col.names = TRUE, row.names = FALSE, sep=",")

mean(Sa_evaluacion_maxent$threshold)

