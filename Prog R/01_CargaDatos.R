
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


### Guardo los datos

save(datJam,file = 'BasesR/datJam.RData')




