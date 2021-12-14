
library(tidyverse)
library(here)
library(data.table)
library(sf)
library(htmlwidgets)
library(leaflet)
library(lubridate)
# library(parallel)
library(nngeo)
library(viridis)

library(geojsonsf)
library(rgdal)



######################################################
###### FUnciones
fun = dir('Funciones/')
fun = fun[grep('\\.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))
######################################################
###### Carga shapes

### Escuelas privadas

escPriv = st_read('SHP/EDUCACIÓN/escuelas privadas.shp')
st_crs(escPriv) = 32721

escPriv_lf = escPriv %>% st_transform(.,crs = 4326)

mutualistas = st_read('SHP/SALUD/mutualistas.shp')
st_crs(mutualistas) = 32721

mutualistas_lf = mutualistas %>% st_transform(.,crs = 4326)

listSHP = list(list(shp = escPriv_lf,
                    color = 'orange',
                    Group = 'Escuelas Privadas'),
               list(shp = mutualistas_lf,
                    color = 'blue',
                    Group = 'Mutualistas'))


save(listSHP, file = 'Resultados/002_listSHPServicios.RData')

