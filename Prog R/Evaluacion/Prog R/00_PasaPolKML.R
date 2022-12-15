

# Librerias
library(sf)
library(sp)
library(rgdal)
library(tidyverse)

######################################



jkt = st_read("Prog R/Evaluacion/SHP/TRES CRUCES límites.kml")
jkt_n <- st_zm(jkt[1], drop=T, what='ZM')
jkt_n = st_transform(jkt_n, crs = 32721)
jkt.df = as.data.frame(jkt_n)
polZona = jkt_n[1,]
#polZona = as(jkt_n[1,], "Spatial")
# st_write(polygon.jkt, dsn= "SHP", driver= "ESRI Shapefile",'jkt.shp',delete_layer = T)

save(polZona, file = 'Prog R/Evaluacion/SHP/polZONA_sf.RData')

