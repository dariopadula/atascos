

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

veoRepetidos = vias %>% st_set_geometry(NULL) %>% group_by(COD_NOMBRE,NOM_CALLE) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(NOM_CALLE) %>% summarise(count = n()) %>%
  filter(count > 2)

sacar = c(veoRepetidos$NOM_CALLE)


nomUsar = vias$NOM_CALLE[!vias$NOM_CALLE %in% sacar]
nomUsar = unique(nomUsar[-grep('PEATONAL',nomUsar,ignore.case = T)])



callesUsar = unique(vias$COD_NOMBRE[vias$NOM_CALLE %in% nomUsar])




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
####### GUARDO LOS RESULTADOS

save(resList,file = 'Resultados/colleSimpLines.RData')
save(puntList,file = 'Resultados/colleSimpPuntos.RData')

#########################################################
####### GENERO LAS CALLES COMO PARTICIONES DE SEGMENTOS
### Genera el data frame con todas
lineasDF = as.data.frame(data.table::rbindlist(resList)) %>% st_as_sf(.,sf_column_name = 'geometry')


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


#### GUarda segmentos

save(segmProy,file = 'Resultados/segmProy.RData')
################################################
#### 

resP_sf <- sf::st_as_sf(resFin, wkt = "geometry",crs = 32721) %>%
    st_transform(.,crs = 4326)



# cont = 0
# for(ii in lineasDF$COD_NOMBRE) {
#   cont = cont + 1
#   resaux = getCallesSegment(lineasDF,puntList,calleInt = ii,bufferSize = 30)
# }

#### 1) Generar las calles con una grillla más pequena (5 metros)
#### 2) Encontrar la calle que intercepta cada calle simplificada
#### 3) la calle que se intercepta hay que hacer un pequeno buufer por si existen cruces donde cambia de nombre la calle
#### 4) construir la calle como un conjuntos de segmentos de cada interceccion
#### 5) para identificar el segemnto de origen, se busca el segmento con centroide mas
##### cercano al centroide de la base
#### 6) luego encontrar el segmento final encontrando el cruce con el end point (ver tema de direccion)





lineasDF[grep('18 de ju',lineasDF$NOM_CALLE,ignore.case = T),]
lineasDF[grep('cuareim',lineasDF$NOM_CALLE,ignore.case = T),]
lineasDF[grep('rivera',lineasDF$NOM_CALLE,ignore.case = T),]

lineasDF_lf = st_transform(lineasDF,crs = 4326)

tretMap = lineasDF_lf %>% filter(COD_NOMBRE %in% c(2383)) #,4441


calleInt = 6051









# res_lf = st_transform(res,crs = 4326)


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = resP_sf %>% filter(COD_NOMBRE == calleInt),weight = 0.7) #%>%
  # addPolylines(data = tretMap,weight = 0.7)%>%
  # addCircles(data = points_sf,weight = 0.7,color = 'red')


pp



pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = tretMap,weight = 3)%>%
  addPolylines(data = resP_sf,weight = 3,color = 'green')%>%
  addCircles(data = res_lf,weight = 0.7,color = 'red')


pp


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = lineasDF_lf,weight = 0.2)


pp



