
library(tidyverse)
library(here)
library(data.table)
library(sf)
library(htmlwidgets)
library(leaflet)
library(lubridate)
# library(parallel)
library(viridis)



######################################################
###### FUnciones
fun = dir('Funciones/')
fun = fun[grep('\\.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))
######################################################
###### LEE base

datos = read.table('Datos/etwazetrafficjam_202111191210.csv',sep = ',',header = T)

#############################################
#### Me quedo solo con los atascos
datJam = subset(datos,delay >= 0)

#############################################
#### Paso las fecas de character a dates

datJam = datJam %>% mutate(datecreated = as.POSIXct(strptime(datecreated, "%Y-%m-%d %H:%M:%S")),
                           datemodified = as.POSIXct(strptime(datemodified, "%Y-%m-%d %H:%M:%S")),
                           datepublished = as.POSIXct(strptime(datepublished, "%Y-%m-%d %H:%M:%S")))

#############################################
##### EXTRAE LAS COORDENADAS

### del Centroide y de los extremos del segemnto
datJam = datJam %>% getCoordCent(.) %>% getCoordLines(.) %>% 
  select(-location,-city,-country,-location_centroid)


### Armo los segmentos como objeto sf
# https://stackoverflow.com/questions/61958296/convert-character-linestring-to-geometry-in-sf

aux = datJam

aux[,'geometry'] = apply(aux[,c('X_lini','Y_lini','X_lfin','Y_lfin')],1,
                         function(xx) {
                           res = paste0('LINESTRING (',paste(xx[c(1,2)], collapse = ' '),' , ',
                                  paste(xx[c(3,4)], collapse = ' '),')')
                           return(res)
                         })

aux_sf <- sf::st_as_sf(aux, wkt = "geometry" )

### Guardo los datos

save(datJam,file = 'BasesR/datJam.RData')

length(unique(c(datJam$street,datJam$endnode)))
length(unique(datJam$endnode))




