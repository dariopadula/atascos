

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
datJam_new = datJamDir

######################################
##### Registra las bases que se van poniendo
##### e indica si hay que calcular o no
nameBase = unique(datJam_new$nomBase)

IndAgg = FALSE
IndIni = FALSE
ID_BaseSegRef = 0
ID_segUnicRef = 0
if(file.exists('BasesActualiza/jamDatosShiny.RData')) {
  load('BasesActualiza/jamDatosShiny.RData')
  IndAgg = sum(!nameBase %in% jamDatosShiny) > 0 
  nomBaseAgg = nameBase[!nameBase %in% jamDatosShiny]
  jamDatosShiny = unique(c(jamDatosShiny,nomBaseAgg))
  
  ###### Cargo las bases previas
  if(IndAgg) {
    load('Shiny/Insumos/XXX_DatosShiny.RData')
    datJamSegm_dfPrev = datJamSegm_df
    segmentUnicPrev = segmentUnic
    datJamDir = datJamDir[datJamDir$nomBase %in% nomBaseAgg,]
    ID_BaseSegRef = nrow(datJamSegm_dfPrev)
    ID_segUnicRef = nrow(segmentUnicPrev)
  }
} else {
  IndIni = TRUE
  jamDatosShiny = nameBase
}



if((IndAgg | IndIni)) {
  ###### Genera los segmentos
  datJamSegm = st_segments(datJamDir,progress = FALSE) %>% mutate(ID_fila = row_number() + ID_BaseSegRef) %>%
    rename('geometry' = 'result') %>% filter(!st_is_empty(.))  %>% 
    mutate(ID_coord = as.character(geometry)) %>% 
    suppressWarnings()
  
  
  ##### Segmentos unicos
  segmentUnic = unique(datJamSegm[,c('geometry','ID_coord')]) 
  
  #### Pregunta si es el inicio o si ya hay segemntos existentes
  
  if(IndIni) {
    segmentUnic = segmentUnic %>% mutate(ID_segmento = row_number())
  } else {
    segmentUnic = subset(segmentUnic,!ID_coord %in% segmentUnicPrev$ID_coord)
    if(nrow(segmentUnic) > 0) {
      segmentUnic = segmentUnic %>% mutate(ID_segmento = row_number() + ID_segUnicRef) 
      segmentUnic = rbind(segmentUnicPrev,segmentUnic)
    } else {
      segmentUnic = segmentUnicPrev  
    }
  }
  
  
  dim(segmentUnic)
  
  
  datJamSegm = datJamSegm %>% left_join(segmentUnic %>% 
                                          st_set_geometry(NULL) %>%
                                          dplyr::select(ID_coord,ID_segmento),
                                        by = 'ID_coord')
  
  
  #### PASO LA BASE DE SEGMENTOS A DATA FRAME
  datJamSegm_df = datJamSegm %>% st_set_geometry(NULL)
  
  #######################################
  ####### Veo si agrego datos o no
  
  if(IndAgg) {
    datJamSegm_dfPrev$diaStr = as.character(datJamSegm_dfPrev$diaStr)
    datJamSegm_df = datJamSegm_df %>% dplyr::select(-ID_coord)
    
    datJamSegm_df = rbind(datJamSegm_dfPrev[,names(datJamSegm_df)],datJamSegm_df)
  }
  
  #################################
  ##### GUARDO LOS SEGMENTOS
  # save(datJamSegm,file = 'Resultados/XXX_datJamSegm.RData')
  ###############################################################
  ########### ARREGLO LOS DIAS 
  disOrd = names(table(datJamSegm_df$diaStr))
  datJamSegm_df = datJamSegm_df %>% mutate(diaStr = factor(diaStr,levels = disOrd))
  
  
  
  #############################################
  ##### data frame de la base datJamDir
  ########################################
  datJam_df = datJam %>% st_set_geometry(NULL)
  
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
  
  #########################################################
  ########### GUardo insumos shiny
  
  save(datJam_df,datJamSegm_df,segmentUnic, file = 'Shiny/Insumos/XXX_DatosShiny.RData')
  
  ### Guardo la tabla de dias mes y anio
  
  save(dsma, file = 'Shiny/Insumos/XXX_dsma.RData')
  
  #### Actualiaza datos cargados
  save(jamDatosShiny,file = 'BasesActualiza/jamDatosShiny.RData')
} else {
  print('Todas las bases que figuran en la base de jam ya fueron segmentizadas')
}

