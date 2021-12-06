

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

###################################
##### FUncion Arregla caracteres
arreglaCaracteres = function(x) {
  x = gsub('Ã\u0091','N',x)
  x = gsub('ã\u0081','A',x)
  x = gsub('Ã\u0081','A',x)
  
  x = gsub('ã¼','u',x)
  x = gsub('Ã¼','u',x)
  
  x = gsub('Ã‘','N',x)
  
  
  x = gsub('\\.','',x)
  x = tolower(x)
}

######################################################
###### LEE base
load('BasesR/datJam.RData')
######################################################
#### Nomenclator IM
vias = st_read('SHP/v_mdg_vias')


######################################################
#### Calles a buscar
calleBusDF = data.frame(calleOrig = c(datJam$street,datJam$endnode)) %>% group_by(calleOrig) %>%
  summarise(cont = n()) %>% ungroup() %>% filter(calleOrig != '') %>%
  mutate(callesBuscar = arreglaCaracteres(calleOrig),
         NOM_CALLE = NA,
         COD_NOMBRE = NA) %>% data.frame()

rownames(calleBusDF) = calleBusDF[,'calleOrig']


###################################################
###### NOMENCLATOR IM

nomCalles = vias %>% st_set_geometry(NULL) %>% dplyr::select(NOM_CALLE,COD_NOMBRE) %>%
  group_by(COD_NOMBRE) %>% slice_head() %>% ungroup() %>%
  mutate(NOM_CALLE = arreglaCaracteres(NOM_CALLE))


###########################################
##### Encuentra las que estan igual

for(ii in rownames(calleBusDF)) {
  buscar = calleBusDF[ii,'callesBuscar']
  expr = paste0('^',buscar,'$')
  aux = nomCalles[grep(expr,nomCalles$NOM_CALLE,ignore.case = T),]
  
  if(nrow(aux) > 0) {
    calleBusDF[ii,c('NOM_CALLE','COD_NOMBRE')] = aux[1,c('NOM_CALLE','COD_NOMBRE')]
  }
}

#######################################
###### Agrego codigos a datJam

datJam[,'NOM_CALLE'] =  calleBusDF[as.character(datJam$street),'NOM_CALLE']
datJam[,'COD_NOMBRE'] =  calleBusDF[as.character(datJam$street),'COD_NOMBRE']

############################################
###### INTENTO UN JOIN ESPACIAL

vias_bf = st_buffer(vias,10)



datVacios = subset(datJamImp,street == '' & is.na(COD_NOMBRE))
datNoVacios = subset(datJamImp,street != '' & is.na(COD_NOMBRE)) %>% group_by(street) %>% slice_head()

datMerge = datNoVacios %>% rbind(datVacios) %>% select(-COD_NOMBRE,-NOM_CALLE)

datMerge_bf = st_buffer(datMerge,1)

datMerge_bf_cod = datMerge_bf %>% st_join(vias_bf[,c('NOM_CALLE','COD_NOMBRE')],
                                      join = st_intersects,largest = T)


resMerge_bf_cod = datMerge_bf_cod %>% st_set_geometry(NULL) %>% 
  dplyr::select(ID_Base,street,NOM_CALLE,COD_NOMBRE) %>% 
  mutate(Vacia = ifelse(street == '',1,0),
    street = ifelse(Vacia == 1,as.character(ID_Base),street)) %>%
  dplyr::select(-ID_Base) %>%
  rename('calleOrig' = 'street') %>% data.frame()


rownames(resMerge_bf_cod) = as.character(resMerge_bf_cod$calleOrig)
calleBusDF$Vacia = 0
codCallesAll = rbind(calleBusDF[!is.na(calleBusDF$NOM_CALLE),colnames(resMerge_bf_cod)],
                     resMerge_bf_cod)

rownames(codCallesAll) = as.character(codCallesAll$calleOrig)


datJam$ID_CODCALLE = ifelse(datJam$street == '',as.character(datJam$ID_Base),datJam$street)


datJam[,'NOM_CALLE'] =  codCallesAll[as.character(datJam$ID_CODCALLE),'NOM_CALLE']
datJam[,'COD_NOMBRE'] =  codCallesAll[as.character(datJam$ID_CODCALLE),'COD_NOMBRE']


#################################################
####### GUARDO LA BASE CON LOS CODIGOS



