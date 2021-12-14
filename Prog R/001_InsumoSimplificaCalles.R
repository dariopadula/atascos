################################################
####### Librerias

library(tidyverse)
library(here)
library(data.table)
library(sf)
library(htmlwidgets)
library(leaflet)
library(lubridate)
library(parallel)
library(viridis)
library(nngeo)

#########################################
#########################################
##### Cargo funciones 
fun = dir(here('Funciones'))
fun = fun[grep('\\.R',fun,ignore.case = T)]
for(ii in fun) source(here('Funciones',ii))


######################################################
#### Nomenclator IM
vias = st_read('SHP/v_mdg_vias')
#########################################
#########################################
#### Represento las calles de una forma simplificada


### Saco codigos repetidos
veoRepetidos = vias %>% st_set_geometry(NULL) %>% group_by(COD_NOMBRE,NOM_CALLE) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(NOM_CALLE) %>% summarise(count = n()) %>%
  filter(count > 2)

sacar = c(veoRepetidos$NOM_CALLE)


#### Identifico calles a simplificar
nomUsar = vias$NOM_CALLE[!vias$NOM_CALLE %in% sacar]
nomUsar = unique(nomUsar[-grep('PEATONAL',nomUsar,ignore.case = T)])

callesUsar = unique(vias$COD_NOMBRE[vias$NOM_CALLE %in% nomUsar])


##### Realiza la simplificacion

resList = list()
puntList = list()
cont = 0

cellsizeMax = 200
for(ii in callesUsar) {
  cont = cont + 1
  if((cont %% 50) == 0) print(cont)
  
  # print(ii)
  auxCalle = vias %>% filter(COD_NOMBRE == ii)
  res = simplificaCalleV2(auxCalle,cellsizeMax = cellsizeMax,npart = 10,distSegm = 10,
                          varsKeep = c('NOM_CALLE','COD_NOMBRE'))
  
  resList[[length(resList) + 1]] = res[[1]]
  puntList[[length(puntList) + 1]] = res[[2]]
}


names(resList) = as.character(callesUsar)
names(puntList) = as.character(callesUsar)


#########################################################
####### GUARDO LOS RESULTADOS de las simplificaciones

save(resList,file = 'Resultados/001_colleSimpLines.RData')
save(puntList,file = 'Resultados/001_colleSimpPuntos.RData')

#########################################################
####### GENERO LAS CALLES COMO PARTICIONES DE SEGMENTOS
### Genera el data frame con todas
lineasDF = as.data.frame(data.table::rbindlist(resList)) %>% 
  st_as_sf(.,sf_column_name = 'geometry')


codCalles = lineasDF$COD_NOMBRE

trials = 1:length(codCalles)
bufferSize = 30

funPar = function(jj) {
  calleInt = codCalles[jj]
  resaux = getCallesSegment(lineasDF,puntList,calleInt = calleInt,bufferSize = bufferSize)
  
  return(resaux)
}



#### Detecta cores
cl <- makeCluster(detectCores())
#### Carga librerias e insumos  
clusterExport(cl, c("lineasDF","codCalles","puntList","bufferSize","cargaCoords","getCallesSegment"))
clusterEvalQ(cl, {
  library(tidyverse)
  library(sf)})

### Corre en paralelo   
system.time({
  results <- parallel::parLapply(cl,trials,funPar)
})

### Cierra los clusters
stopCluster(cl)

names(results) = as.character(codCalles)

sum(do.call(c,lapply(results,function(xx) nrow(xx) == 0)))


### Detecta vacias
vacias = do.call(c,lapply(results,is_empty))
### Se queda con las no vacias
res = results[!vacias]
### Genera el data frame con todas
segmProy = as.data.frame(data.table::rbindlist(res)) 


#### GUarda segmentos de las intesecciones
save(segmProy,file = 'Resultados/001_segmProy.RData')
################################################



