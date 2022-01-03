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
load('BasesR/011_datCor.RData')

########################################

datCor_df = datCor %>% st_set_geometry(NULL)

###### Genera los segmentos
datCorSegm = st_segments(datCor,progress = FALSE) %>% mutate(ID_fila = row_number()) %>%
  rename('geometry' = 'result')

##### Genera los centroides
datCorCentr = st_centroid(datCorSegm,progress = FALSE) %>% filter(!st_is_empty(.)) %>%
  suppressWarnings()
##### Me quedo solo con los segmentos que tienen geometria
datCorSegm = subset(datCorSegm,ID_fila %in% datCorCentr$ID_fila)
dim(datCorSegm)

##### Segmentos unicos
segmentUnicCor = unique(datCorSegm[,c('geometry')]) %>% mutate(ID_segmento = row_number())
dim(segmentUnicCor)

centSegUnic = st_centroid(segmentUnicCor) %>% filter(!st_is_empty(.)) %>% #st_transform(.,32721)
  suppressWarnings()

######## Agrega ID segmento

datCorCentr = datCorCentr %>% st_join(centSegUnic[,'ID_segmento'], 
                                      join=nngeo::st_nn, maxdist= Inf,k=1) %>%
  suppressMessages()


datCorSegm = datCorSegm %>% left_join(datCorCentr %>% 
                                        st_set_geometry(NULL) %>%
                                        dplyr::select(ID_fila,ID_segmento),by = 'ID_fila')


#################################
##### GUARDO LOS SEGMENTOS
save(datCorSegm,file = 'Resultados/XXX_datCorSegm.RData')
###############################################################
########### ARREGLO LOS DIAS 
disOrd = names(table(datCorSegm$diaStr))
datCorSegm = datCorSegm %>% mutate(diaStr = factor(diaStr,levels = disOrd))


#### PASO LA BASE DE SEGMENTOS A DATA FRAME
datCorSegm_df = datCorSegm %>% st_set_geometry(NULL)
#########################################################
########### GUardo insumos shiny

save(datCor_df,datCorSegm_df,segmentUnicCor, file = 'Shiny/Insumos/XXX_DatosShinyCortes.RData')
