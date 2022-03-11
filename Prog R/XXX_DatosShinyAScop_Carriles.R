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
nameBase = 'info_detector_carriles_202201'

datos = read.table(paste0('Datos/Conteo/',nameBase,'.csv'),sep = ',',header = T)

## Quedarse solo con los SP
# colnames(datos) = gsub('info_detector_totales.','',names(datos))
# usar como unico dsc_avenida, dsc_int_anterior y dsc_int_siguiente, con la base de totales

colnames(datos) = gsub('info_detector_carriles.','',names(datos))

############## Datos dep

datos = subset(datos,tipo_sensor == 'SP')

varSelect = c("dsc_avenida","dsc_int_anterior","dsc_int_siguiente",'cod_sentido',"latitud",
              "longitud","fecha_hora","volume",
              "spacemeanspeed","cod_nivel_servicio")



# dd = unique(datos[,c("id_zona","id_subzona","dsc_zona","dsc_subzona","dsc_avenida",
#                   "dsc_int_anterior","dsc_int_siguiente",'dsc_sentido',"latitud","longitud")])

# spacemeanspeed = velocidad

datAScop_df = datos %>% filter(!is.na(latitud) & !is.na(longitud)) %>%
  dplyr::select_at(vars(dplyr::one_of(varSelect))) %>%
  mutate(ID_posSens = paste(dsc_avenida,dsc_int_anterior,dsc_int_siguiente,sep = '_'))


#########################################
############# Coordenadas unicas
puntAScoptUnic = datAScop_df %>% 
  dplyr::select(latitud,longitud,dsc_avenida,dsc_int_anterior,dsc_int_siguiente,cod_sentido) %>% 
  group_by(latitud,longitud,dsc_avenida,dsc_int_anterior,dsc_int_siguiente,cod_sentido) %>% slice_head() %>%
  mutate(ID_posSens = paste(dsc_avenida,dsc_int_anterior,dsc_int_siguiente,sep = '_')) %>% data.frame()


datAScop_df = datAScop_df %>% 
  mutate(datemodified = as.POSIXct(strptime(fecha_hora, "%Y-%m-%d %H:%M:%S")),
         diaStr = substr(datemodified,1,10),
         hora = hour(datemodified),
         minuto = minute(datemodified),
         minutoHora = 60*hora + minuto,
         minutoHora = minutoHora - minutoHora %% 5,
         ID_posSens = paste(dsc_avenida,dsc_int_anterior,dsc_int_siguiente,sep = '_')) %>%
  dplyr::select(-latitud,-longitud,-dsc_avenida,-dsc_int_anterior,-dsc_int_siguiente)



datAScop_df = datAScop_df %>% group_by(ID_posSens,diaStr,minutoHora) %>%
  summarise(count = n(),
            volume = sum(volume,na.rm = T),
            spacemeanspeed = mean(spacemeanspeed,na.rm = T),
            cod_nivel_servicio = first(cod_nivel_servicio))
  
##########################################
############# Guardo los datos para el shiny
save(datAScop_df,puntAScoptUnic, file = 'Shiny/Insumos/XXX_DatosAScopShiny.RData')


