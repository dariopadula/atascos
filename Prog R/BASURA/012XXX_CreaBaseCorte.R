
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
arreglaCaracteres = function(x,tolower = T,sacPunto = T) {
  x = gsub('Ã\u0091','N',x)
  x = gsub('ã\u0081','A',x)
  x = gsub('Ã\u0081','A',x)
  
  x = gsub('ã¼','u',x)
  x = gsub('Ã¼','u',x)
  
  x = gsub('Ã‘','N',x)
  
  
  if(sacPunto) x = gsub('\\.','',x)
  if(tolower) x = tolower(x)
  
  return(x)
}

######################################################
######################################################
###### LEE base

datos = read.table('Datos/Cortes/cortes202204.csv',sep = ',',header = T)

#############################################
#### Paso las fecas de character a dates

sacaFecha = function(xx) {
  yy = strsplit(xx,'_')[[1]]
  return(yy[2])
}

# datCor$datecreated = do.call(c,sapply(datCor$id,function(xx) list(sacaFecha(xx))))

datCor = datos %>% filter(city == 'Montevideo') %>% 
  rowwise() %>% mutate(datecreated = sacaFecha(id)) %>%
  ungroup() %>%
  mutate(datemodified = as.POSIXct(strptime(gsub('T',' ',max_datemodified), "%Y-%m-%d %H:%M:%S")),
         datecreated = as.POSIXct(strptime(gsub('T',' ',datecreated), "%Y-%m-%d %H:%M:%S"))) %>%
  filter(datemodified < as.POSIXct(strptime('2022-05-01 00:00:01', "%Y-%m-%d %H:%M:%S")))


datCor = datCor %>% 
  arrange(desc(id),desc(datemodified)) %>% 
  dplyr::select(-city,-location_centroid_lat,-location_centroid_lon) %>%
  mutate(hora = hour(datemodified),
         minuto = minute(datemodified),
         minutoHora = 60*hora + minuto) %>%
  group_by(id,diaStr) %>%
  slice_max(datemodified) %>%
  ungroup() %>%
  as.data.frame() %>%
  getLineGeomV2(.) %>%
  dplyr::select(-location) %>%
  group_by(id) %>%
  arrange(desc(datemodified)) %>%
  mutate(ID_EntityDia = row_number()) %>%
  ungroup() %>% 
  # rowwise() %>% mutate(datecreated = max(c(datecreated,datepublished))) %>%
  # ungroup() %>%
  arrange(id,desc(ID_EntityDia)) %>%
  st_as_sf(.,wkt = 'geometry',crs = 4326) %>%
  # st_transform(.,crs = 32721) %>%
  mutate(ID_Base = row_number(),
         difCreate = as.numeric(difftime(datemodified,datecreated, units = 'mins')),
         minutoIniDia = 
           ifelse(difCreate < minutoHora,round(minutoHora - difCreate),0),
         minutoLastDia = minutoHora) %>%
  filter(!st_is_empty(st_geometry(.)))

######################################################
######################################################
######################################################

########################################

###############################################################
########### ARREGLO LOS DIAS 
disOrd = names(table(datCor$diaStr))
datCor = datCor %>% mutate(diaStr = factor(diaStr,levels = disOrd))


datCor_df = datCor %>% st_set_geometry(NULL)

###### Genera los segmentos
datCorSegm = st_segments(datCor,progress = FALSE) %>% 
  rename('geometry' = 'result') %>% 
  mutate(ID_coord = as.character(geometry))

##### Segmentos unicos
segmentUnicCor = unique(datCorSegm[,c('geometry')]) %>% mutate(ID_coord = as.character(geometry),
                                                               ID_segmento = row_number())
dim(segmentUnicCor)

######## Agrega ID segmento

datCorSegm = datCorSegm %>% left_join(segmentUnicCor %>% 
                                        st_set_geometry(NULL) %>%
                                        dplyr::select(ID_coord,ID_segmento))


#################################
##### GUARDO LOS SEGMENTOS
# save(datCorSegm,file = 'Resultados/XXX_datCorSegm.RData')


#### PASO LA BASE DE SEGMENTOS A DATA FRAME
datCorSegm_df = datCorSegm %>% st_set_geometry(NULL)
#########################################################
########### GUardo insumos shiny

save(datCor_df,datCorSegm_df,segmentUnicCor, file = 'Shiny/Insumos/XXX_DatosShinyCortes.RData')




