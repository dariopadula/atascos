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
# nameBase = 'info_detector_carriles_202201'
nameBase = 'info_detector_carriles_02_03_2022'

datos = read.table(paste0('Datos/Conteo/',nameBase,'.csv'),sep = ',',header = T)

## Quedarse solo con los SP
# colnames(datos) = gsub('info_detector_totales.','',names(datos))
# usar como unico dsc_avenida, dsc_int_anterior y dsc_int_siguiente, con la base de totales

colnames(datos) = gsub('info_detector_carriles.','',names(datos))

############## Datos dep

varSelect = c("id_zona","id_subzona","dsc_zona","dsc_subzona","dsc_avenida",
              "dsc_int_anterior","dsc_int_siguiente","tipo_sensor",'dsc_sentido',"latitud",
              "longitud","sensor_carril","cod_concentrador","cod_detector","fecha_hora","dsc_status",
              "tipo_dia","volume","spacemeanspeed","cod_nivel_servicio")

# spacemeanspeed = velocidad

datAScop_df = datos %>% filter(!is.na(latitud) & !is.na(longitud)) %>%
  dplyr::select_at(vars(dplyr::one_of(varSelect))) 


#########################################
############# Coordenadas unicas
puntAScoptUnic = datAScop_df %>% dplyr::select(latitud,longitud) %>% 
  group_by(latitud,longitud) %>% slice_head() %>%
  mutate(ID_posSens = paste(latitud,longitud,sep = '_')) %>% data.frame()


datAScop_df = datAScop_df %>% 
  mutate(datemodified = as.POSIXct(strptime(fecha_hora, "%Y-%m-%d %H:%M:%S")),
         diaStr = substr(datemodified,1,10),
         # diaSem = weekdays(datemodified),
         # finDeSem = ifelse(diaSem %in% c('sábado','domingo'),'Fin de semana','Lunes a viernes'),
         hora = hour(datemodified),
         minuto = minute(datemodified),
         minutoHora = 60*hora + minuto,
         minutoHoraFin = minutoHora + 5,
         # diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')),
         ID_posSens = paste(latitud,longitud,sep = '_')) %>%
  dplyr::select(-latitud,-longitud)


##########################################
############# Guardo los datos para el shiny
save(datAScop_df,puntAScoptUnic, file = 'Shiny/Insumos/XXX_DatosAScopShiny.RData')


