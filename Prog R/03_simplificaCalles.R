

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
for(ii in callesUsar) {
  cont = cont + 1
  if((cont %% 50) == 0) print(cont)
  
  # print(ii)
  auxCalle = vias %>% filter(COD_NOMBRE == ii)
  res = simplificaCalleV2(auxCalle,cellsizeMax = 500,npart = 10,distSegm = 10,
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


#### 1) Generar las calles con una grillla más pequena (5 metros)
#### 2) Encontrar la calle que intercepta cada calle simplificada
#### 3) la calle que se intercepta hay que hacer un pequeno buufer por si existen cruces donde cambia de nombre la calle
#### 4) construir la calle como un conjuntos de segmentos de cada interceccion
#### 5) para identificar el segemnto de origen, se busca el segmento con centroide mas
##### cercano al centroide de la base
#### 6) luego encontrar el segmento final encontrando el cruce con el end point (ver tema de direccion)


### Genera el data frame con todas
lineasDF = as.data.frame(data.table::rbindlist(resList)) %>% st_as_sf(.,sf_column_name = 'geometry')

lineasDF[grep('18 de ju',lineasDF$NOM_CALLE,ignore.case = T),]
lineasDF[grep('cuareim',lineasDF$NOM_CALLE,ignore.case = T),]
lineasDF[grep('zelmar mi',lineasDF$NOM_CALLE,ignore.case = T),]

lineasDF_lf = st_transform(lineasDF,crs = 4326)

tretMap = lineasDF_lf %>% filter(COD_NOMBRE %in% c(7572,2265)) #,4441


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = tretMap,weight = 0.7)

pp
calleInt = 7572





res = st_intersection(lineasDF %>% filter(COD_NOMBRE == calleInt)  %>% st_buffer(.,4) %>% select(geometry),
                      lineasDF %>% filter(COD_NOMBRE != calleInt)) %>% st_cast(.,'POINT')

puntCalles = puntList[[as.character(calleInt)]]


matchAux <- res %>% 
  st_join(puntCalles[,'ID_calle'], join=nngeo::st_nn, maxdist= Inf,k=1) %>% 
  suppressMessages()


table(matchAux$ID_calle)



