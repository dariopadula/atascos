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


baseElim = c('01_31_032022')

##### Carga Las base a depurar
########################################
####### carga las bases con y sin direccion
load('BasesR/011_datJam.RData')
#### GUARDO LA BASE CON LOS CODIGOS
load('Resultados/011_datJamDir.RData')
#### Actualizo las bases ingresadas
load('BasesActualiza/jamBaseNames.RData')
#### Carga Insumos shiny
load('Shiny/Insumos/XXX_DatosShiny.RData')
load('BasesActualiza/jamDatosShiny.RData')

##############################################################
########### ELIMINA LAS FILAS ASOCIADAS A ESE REGISTRO DE BASE
datJam = datJam %>% filter(!nomBase %in% baseElim)
datJamDir = datJamDir %>% filter(!nomBase %in% baseElim)
jamBaseNames = jamBaseNames[!jamBaseNames %in% baseElim]
datJam_df = datJam_df %>% filter(!nomBase %in% baseElim)
datJamSegm_df = datJamSegm_df %>% filter(!nomBase %in% baseElim)
jamDatosShiny = jamDatosShiny[!jamDatosShiny %in% baseElim]
##############################################################
##############################################################

##### Actualiza tablas
##### Tabla con dias y meses y semana
dsma = datJam_df %>% dplyr::select(diaStr,finDeSem,diaSem) %>% 
  group_by(diaStr) %>% 
  slice_head() %>%
  ungroup() %>%
  mutate(diaNum = as.numeric(substr(diaStr,9,10)),
         mes = substr(diaStr,6,7),
         anio = substr(diaStr,1,4),
         levelDia = as.numeric(diaSem),
         diaSem = as.character(diaSem)) %>%
  arrange(anio,mes,diaNum) %>% data.frame()

rownames(dsma) = dsma$diaStr


##############################################
##### Tabla para resumen de la base

datJam_resum = datJam_df %>% 
  mutate(
    tiempoMP = as.numeric(difftime(datemodified,datepublished,units = 'mins')),
    diaMes = substr(diaStr,1,7)) %>% 
  group_by(entity_id,street) %>%
  summarise(count = n(),
            diaMes = last(diaMes),
            tiempoMP = max(tiempoMP),
            delay = mean(delay,na.rm = T),
            speedkmh = mean(speedkmh,na.rm = T),
            level = max(level,na.rm = T)) %>%
  ungroup() %>%
  mutate(duracionEv = ifelse(abs(tiempoMP - 2*count) > 2,2*count,tiempoMP)) %>%
  suppressMessages()


###############################################
######### GUARDA TODOS LAS BASES E INSUMOS CON LOS REGISTROS BORRADOS
save(dsma, file = 'Shiny/Insumos/XXX_dsma.RData')
### Guarda base para hacer el resumen de la base
save(datJam_resum, file = 'Shiny/Insumos/XXX_datJam_resum.RData')
########################################
####### GUarda las bases con y sin direccion
save(datJam,file = 'BasesR/011_datJam.RData')
#### GUARDO LA BASE CON LOS CODIGOS
save(datJamDir,file = 'Resultados/011_datJamDir.RData')

save(jamBaseNames,file = 'BasesActualiza/jamBaseNames.RData')

save(datJam_df,datJamSegm_df,segmentUnic, file = 'Shiny/Insumos/XXX_DatosShiny.RData')

save(jamDatosShiny,file = 'BasesActualiza/jamDatosShiny.RData')



