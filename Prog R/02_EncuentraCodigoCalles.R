

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
load('BasesR/datJam.RData')

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
#### Calles a buscar
calleBusDF = data.frame(calleOrig = c(datJam$street,datJam$endnode)) %>% group_by(calleOrig) %>%
  summarise(cont = n()) %>% ungroup() %>% filter(calleOrig != '') %>%
  mutate(callesBuscar = arreglaCaracteres(calleOrig),
         NOM_CALLE = NA,
         COD_NOMBRE = NA) %>% data.frame()



rownames(calleBusDF) = calleBusDF[,'calleOrig']
######################################################
#### Nomenclator IM
vias = st_read('SHP/v_mdg_vias')

nomCalles = vias %>% st_set_geometry(NULL) %>% dplyr::select(NOM_CALLE,COD_NOMBRE) %>%
  group_by(COD_NOMBRE) %>% slice_head() %>% ungroup() %>%
  mutate(NOM_CALLE = arreglaCaracteres(NOM_CALLE))

callFind = 'herrera'
calleBusDF$calleOrig[grep(callFind,calleBusDF$calleOrig,ignore.case = T)]
calleBusDF$callesBuscar[grep(callFind,calleBusDF$callesBuscar,ignore.case = T)]
nomCalles[grep(callFind,nomCalles$NOM_CALLE,ignore.case = T),]



######################################
##### Encuentra las que estan igual

for(ii in rownames(calleBusDF)) {
  buscar = calleBusDF[ii,'callesBuscar']
  expr = paste0('^',buscar,'$')
  aux = nomCalles[grep(buscar,nomCalles$NOM_CALLE,ignore.case = T),]
  
  if(nrow(aux) > 0) {
    calleBusDF[ii,c('NOM_CALLE','COD_NOMBRE')] = aux[1,c('NOM_CALLE','COD_NOMBRE')]
  }
  
}

#########################################
#### Para las que no son iguales hace una busqueda palabra por palabra
aa = calleBusDF[is.na(calleBusDF$NOM_CALLE),]


############################################
###### INTENTO UN JOIN ESPACIAL

vias_bf = st_buffer(vias,10)
datJam_bf = st_buffer(datJam,1)

pp = datJam_bf %>% st_join(vias_bf[,c('NOM_CALLE','COD_NOMBRE')],join = st_intersects,largest = T)


pp = pp %>% st_set_geometry(NULL) %>% group_by(street,NOM_CALLE,COD_NOMBRE) %>% slice_head()

# for(ii in rownames(aa)) {
#   aux = aa[ii,'callesBuscar']
#   auxSplit = strsplit(aux,' ')[[1]]
#   # auxSplit = auxSplit[nchar(auxSplit) > 1]
#   
#   ref = data.frame(calle = unique(nomCalles$NOM_CALLE),cont = 0)
#   rownames(ref) = ref$calle
#   
#   for(jj in auxSplit) {
#     aux = ref$calle[grep(jj,ref$calle,ignore.case = T)]
#     
#     ref[aux,'cont'] = ref[aux,'cont'] + 1
#     
#     ref[ref$cont == max(ref$cont),]
#     
#   }
#   
# }




######## Agrego codigos de calles

## Street 

datJamCods = datJam %>% left_join(calleBusDF %>% select(calleOrig,COD_NOMBRE), 
                              by = c('street' = 'calleOrig')) %>%
  rename('street_cod' = 'COD_NOMBRE') %>%
  left_join(calleBusDF %>% select(calleOrig,COD_NOMBRE), 
            by = c('endnode' = 'calleOrig')) %>%
  rename('endnode_cod' = 'COD_NOMBRE')
  
sum(is.na(datJamCods$street_cod))
sum(is.na(datJamCods$endnode_cod))

sum(is.na(datJamCods$endnode_cod) & is.na(datJamCods$street_cod))


#################################################
####### GUARDO LA BASE CON LOS CODIGOS

save(datJamCods,file = 'BasesR/datJamCods.RData')

