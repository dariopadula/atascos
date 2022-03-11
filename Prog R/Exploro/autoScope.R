
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
nameBase = 'info_detector_totales_202112'

datos = read.table(paste0('Datos/Conteo/',nameBase,'.csv'),sep = ',',header = T)

colnames(datos) = gsub('info_detector_totales.','',names(datos))

datUnique = unique(datos[,c("id_zona","id_subzona","dsc_zona","dsc_subzona","dsc_avenida",
                    "dsc_int_anterior","dsc_int_siguiente","tipo_sensor",'cod_sentido',"latitud",
                    "longitud","sensor_carril","cod_concentrador","cod_detector")])


ddd = unique(datos[,c("latitud","longitud","cod_concentrador","cod_detector")])

ddd = ddd %>% arrange(latitud,longitud) 
dd = unique(datos[,c("latitud","longitud")])



write.table(datUnique,'uniqueAutoScope.csv',sep = ';',row.names = F,dec = ',')

head(datUnique)

length(unique(datos$cod_concentrador))

res = datos %>% group_by(latitud,longitud) %>%
  summarise(n = n(),
            sentidos = paste(unique(sensor_carril),collapse = '//'))


res = datos %>% 
  group_by(latitud,longitud,
           sensor_carril) %>%
  slice_head() %>% select(latitud,longitud,sensor_carril)


############## Datos dep

varSelect = c("id_zona","id_subzona","dsc_zona","dsc_subzona","dsc_avenida",
              "dsc_int_anterior","dsc_int_siguiente","tipo_sensor",'dsc_sentido',"latitud",
              "longitud","sensor_carril","cod_concentrador","cod_detector","fecha_hora","dsc_status",
              "tipo_dia","volume","spacemeanspeed","cod_nivel_servicio")

# spacemeanspeed = velocidad

datosDep = datos %>% filter(!is.na(latitud) & !is.na(longitud)) %>%
  dplyr::select_at(vars(dplyr::one_of(varSelect))) 


datAScop_df = datosDep %>% 
  mutate(datemodified = as.POSIXct(strptime(fecha_hora, "%Y-%m-%d %H:%M:%S")),
         diaStr = substr(datemodified,1,10),
         diaSem = weekdays(datemodified),
         finDeSem = ifelse(diaSem %in% c('sábado','domingo'),'Fin de semana','Lunes a viernes'),
         hora = hour(datemodified),
         minuto = minute(datemodified),
         minutoHora = 60*hora + minuto,
         minutoHoraFin = minutoHora + 5,
         diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')),
         ID_posSens = paste(latitud,longitud,sep = '_')) %>%
  dplyr::select(-latitud,-longitud)


############# Coordenadas unicas
puntAScoptUnic = datosDep %>% dplyr::select(latitud,longitud) %>% 
  group_by(latitud,longitud) %>% slice_head() %>%
  mutate(ID_posSens = paste(latitud,longitud,sep = '_')) %>% data.frame()


# save(datAScop_df,puntAScoptUnic, file = 'Shiny/Insumos/XXX_DatosAScopShiny.RData')

############ Pruebo filtros

aux = datAScop_df[datAScop_df$finDeSem == 'Lunes a viernes',]

horaIni = 17
minIni = 0
horaFin = 19
minFin = 0

minutoIni = horaIni*60 + minIni
minutoFin = horaFin*60 + minFin

aux$posMin = aux$minutoHora - minutoIni
aux$posFin = minutoFin - aux$minutoHora

auxFil = aux %>% filter(posMin >= - 4 & posFin >= - 4) %>%
  mutate(volume_aj = ifelse(posMin < 0,((5 + posMin)/5)*volume,
                                 ifelse(posFin < 0,((5 + posFin)/5)*volume,volume)))



factorInterv = (minutoFin - minutoIni)/5

auxAgg =  auxFil %>% group_by(ID_posSens) %>%
  summarise(count = n(),
            valume_mean = round(mean(volume_aj,na.rm = T)),
            valume_intTime = valume_mean*factorInterv,
            velocidad_mean = mean(spacemeanspeed,na.rm = T))



