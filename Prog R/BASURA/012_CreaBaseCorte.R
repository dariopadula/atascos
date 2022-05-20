

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

datos = read.table('Datos/etwazetrafficjam_202112151544_COR.csv',sep = ',',header = T)


#############################################
#### Me quedo solo con los atascos
datCor = subset(datos,delay == -1)

######################################################
#### Nomenclator IM
vias = st_read('SHP/v_mdg_vias')

#############################################
#### Paso las fecas de character a dates

datCorAux = datCor %>% mutate(datecreated = as.POSIXct(strptime(datecreated, "%Y-%m-%d %H:%M:%S")),
                           datemodified = as.POSIXct(strptime(datemodified, "%Y-%m-%d %H:%M:%S")),
                           datepublished = as.POSIXct(strptime(datepublished, "%Y-%m-%d %H:%M:%S"))) 
# %>%
#   filter(datepublished >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))


# aux = datCor %>% group_by(entity_id) %>% 
#   summarise(count = n(),
#             datecreated = sd(datecreated),
#             datemodified = sd(datemodified),
#             datepublished = sd(datepublished),
#             length = sd(length),
#             ngeom = length(unique(location)))
#############################################
##### EXTRAE LAS COORDENADAS

### del Centroide y de los extremos del segemnto
# datCor = datCor %>% getCoordCent(.) %>% getLineGeomV2(.) %>%
datCor = datCorAux %>% arrange(desc(entity_id),desc(datemodified)) %>% 
  dplyr::select(-city,-country,-location_centroid,-entity_type,-fiware_servicepath,-pubmillis,-turntype) %>%
  mutate(diaStr = substr(datemodified,1,10),
         diaSem = weekdays(datemodified),
         finDeSem = ifelse(diaSem %in% c('sábado','domingo'),'Fin de semana','Lunes a viernes'),
         hora = hour(datemodified),
         minuto = minute(datemodified),
         minutoHora = 60*hora + minuto,
         diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo'))) %>%
  group_by(entity_id,diaStr) %>%
  slice_max(datemodified) %>%
  ungroup() %>%
  as.data.frame() %>%
  getLineGeomV2(.) %>%
  dplyr::select(-location) %>%
  group_by(entity_id) %>%
  arrange(desc(datemodified)) %>%
  mutate(ID_EntityDia = row_number()) %>%
  ungroup() %>% 
  rowwise() %>% mutate(datecreated = max(c(datecreated,datepublished))) %>%
  ungroup() %>%
  arrange(entity_id,desc(ID_EntityDia)) %>%
  st_as_sf(.,wkt = 'geometry',crs = 4326) %>%
  st_transform(.,crs = 32721) %>%
  mutate(ID_Base = row_number(),
         difCreate = as.numeric(difftime(datemodified,datecreated, units = 'mins')),
         minutoIniDia = 
           ifelse(difCreate < minutoHora,round(minutoHora - difCreate),0),
         minutoLastDia = minutoHora)


dd = datCorAux[datCorAux$id == 'waze:jam:1167598302_2022-04-17 23:46:05',] 
dd = datCorAux[datCorAux$entity_id == 'waze:jam:1167598302',] 


dd = dd %>% arrange(desc(datemodified))
########################################
####### GUarda las bases con y sin direccion
save(datCor,file = 'BasesR/011_datCor.RData')

