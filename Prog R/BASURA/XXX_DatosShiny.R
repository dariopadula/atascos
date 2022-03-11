

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

#######################################
###### PARCHE DIRECCION
load('Resultados/011_datJamDir.RData')
datJam = datJamDir
########################################

datJam_df = datJamDir %>% st_set_geometry(NULL)

###### Genera los segmentos
datJamSegm = st_segments(datJamDir,progress = FALSE) %>% mutate(ID_fila = row_number()) %>%
  rename('geometry' = 'result')

##### Genera los centroides
datJamCentr = st_centroid(datJamSegm,progress = FALSE) %>% filter(!st_is_empty(.)) %>%
  suppressWarnings()
##### Me quedo solo con los segmentos que tienen geometria
datJamSegm = subset(datJamSegm,ID_fila %in% datJamCentr$ID_fila)

datJamSegm = datJamSegm %>% mutate(ID_coord = as.character(geometry))
dim(datJamSegm)

##### Segmentos unicos
segmentUnic = unique(datJamSegm[,c('geometry','ID_coord')]) %>% mutate(ID_segmento = row_number())
dim(segmentUnic)


centSegUnic = st_centroid(segmentUnic) %>% filter(!st_is_empty(.)) %>% #st_transform(.,32721)
  suppressWarnings()

######## Agrega ID segmento

datJamCentr = datJamCentr %>% st_join(centSegUnic[,'ID_segmento'], 
                                      join=nngeo::st_nn, maxdist= Inf,k=1) %>%
  suppressMessages()


datJamSegm = datJamSegm %>% left_join(datJamCentr %>% 
                                        st_set_geometry(NULL) %>%
                                        dplyr::select(ID_fila,ID_segmento),by = 'ID_fila')


#################################
##### GUARDO LOS SEGMENTOS
save(datJamSegm,file = 'Resultados/XXX_datJamSegm.RData')
###############################################################
########### ARREGLO LOS DIAS 
disOrd = names(table(datJamSegm$diaStr))
datJamSegm = datJamSegm %>% mutate(diaStr = factor(diaStr,levels = disOrd))


#### PASO LA BASE DE SEGMENTOS A DATA FRAME
datJamSegm_df = datJamSegm %>% st_set_geometry(NULL)
#########################################################
########### GUardo insumos shiny

save(datJam_df,datJamSegm_df,segmentUnic, file = 'Shiny/Insumos/XXX_DatosShiny.RData')


