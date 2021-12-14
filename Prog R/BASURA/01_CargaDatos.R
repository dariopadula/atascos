
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



######################################################
###### FUnciones
fun = dir('Funciones/')
fun = fun[grep('\\.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))
######################################################
###### LEE base

datos = read.table('Datos/etwazetrafficjam_202111261100.csv',sep = ',',header = T)

#############################################
#### Me quedo solo con los atascos
datJam = subset(datos,delay >= 0)

#############################################
#### Paso las fecas de character a dates

datJam = datJam %>% mutate(datecreated = as.POSIXct(strptime(datecreated, "%Y-%m-%d %H:%M:%S")),
                           datemodified = as.POSIXct(strptime(datemodified, "%Y-%m-%d %H:%M:%S")),
                           datepublished = as.POSIXct(strptime(datepublished, "%Y-%m-%d %H:%M:%S"))) %>%
  filter(datecreated >= as.POSIXct(strptime('2021-11-24 00:00:01', "%Y-%m-%d %H:%M:%S")))

#############################################
##### EXTRAE LAS COORDENADAS

### del Centroide y de los extremos del segemnto
# datJam = datJam %>% getCoordCent(.) %>% getLineGeomV2(.) %>%
datJam = datJam %>% arrange(desc(entity_id),desc(datemodified)) %>% 
  getLineGeomV2(.) %>% 
    select(-city,-country,-location_centroid,-location,-entity_type,-fiware_servicepath,-pubmillis,-turntype) %>% 
  st_as_sf(.,wkt = 'geometry',crs = 4326) %>%
  st_transform(.,crs = 32721) %>% 
  mutate(ID_Base = row_number())


###### Genera los segmentos
datJamSegm = st_segments(datJam,progress = FALSE) %>% mutate(ID_fila = row_number()) %>%
  rename('geometry' = 'result')

##### Genera los centroides
datJamCentr = st_centroid(datJamSegm,progress = FALSE) %>% filter(!st_is_empty(.)) %>%
  suppressWarnings()
##### Me quedo solo con los segmentos que tienen geometria
datJamSegm = subset(datJamSegm,ID_fila %in% datJamCentr$ID_fila)
dim(datJamSegm)

##### Segmentos unicos
segmentUnic = unique(datJamSegm[,c('geometry')]) %>% mutate(ID_segmento = row_number())
dim(segmentUnic)

centSegUnic = st_centroid(segmentUnic) %>% filter(!st_is_empty(.)) %>% #st_transform(.,32721)
  suppressWarnings()



sum(st_is_empty(centSegUnic))
sum(st_is_empty(datJamSegm))
sum(st_is_empty(datJamCentr))


# datJamCentr[st_is_empty(datJamCentr),]
######## Agrega ID segmento

datJamCentr = datJamCentr %>% st_join(centSegUnic[,'ID_segmento'], 
                                    join=nngeo::st_nn, maxdist= Inf,k=1) %>%
  suppressMessages()


datJamSegm = datJamSegm %>% left_join(datJamCentr %>% 
                                        st_set_geometry(NULL) %>%
                                        select(ID_fila,ID_segmento),by = 'ID_fila')


###############################################################
########### Calculo variables para agreagar

## Uso el date modification para buscar por tiempo

datJamSegm = datJamSegm %>% mutate(difTime = as.numeric(difftime(datemodified,datecreated,units = 'mins')),
                                   difTime01 = as.numeric(difftime(datemodified,datepublished,units = 'mins')),
                                   diaStr = substr(datemodified,1,10),
                                   diaSem = weekdays(datemodified),
                                   finDeSem = ifelse(diaSem %in% c('sábado','domingo'),1,0),
                                   hora = hour(datemodified),
                                   minuto = minute(datemodified),
                                   minutoHora = 60*hora + minuto)



###########################################
##### GUARDO SHAPE SEGEMNTOS

# datJamSegm_sp = as(datJamSegm,Class = 'Spatial')
#####################################################
####### GUARDO EL SHAPE
# writeOGR(datJamSegm_sp, ".", "SHP_datJamSegm_sp", driver="ESRI Shapefile",overwrite_layer = T)

datJamSegm_df = datJamSegm %>% st_set_geometry(NULL)
##########################################
####### Datos filtrados

horaIni = 17
minIni = 0

horaFin = 18
minFin = 0



minHoraIni = horaIni*60 + minIni
minHoraFin = horaFin*60 + minFin



datFiltro = subset(datJamSegm_df,(minutoHora >= minHoraIni & minutoHora <= minHoraFin) & 
                     finDeSem == 0 & diaStr == "2021-11-25")


nDays = length(unique(as.character(datFiltro$diaStr)))
larInter = 5
nInter = ((minHoraFin - minHoraIni)/larInter + 1)*nDays



####### Datos para agregar
varsGroup = c('ID_segmento','street','roadtype')
varsSum = c('delay','length','level','speed','speedkmh','minutoHora')


datAgg = datFiltro %>% group_by_at(vars(dplyr::one_of(varsGroup))) %>%
  summarise(nJam = n(),
            porcTimeJam = round(100*nJam/nInter,1),
            delayMean = mean(delay,na.rm = T),
            levelMean = mean(level,na.rm = T),
            lengthMean = mean(length,na.rm = T),
            speedkmhMean = mean(speedkmh,na.rm = T),
            minutoHoraMean = mean(minutoHora,na.rm = T)) %>%
  ungroup() %>%
  mutate(
            horaMedia = floor(minutoHoraMean/60),
            minMedio = round(60*(minutoHoraMean/60 - horaMedia)),
            timeStr = ifelse(nchar(minMedio) == 1,paste0(horaMedia,':0',minMedio),
                             paste0(horaMedia,':',minMedio)),
            content = paste(sep = "<br/>",
                            paste("<b><a href='http://www.samurainoodle.com'>ID Segmentno</a></b>:",ID_segmento),
                            paste("<b><a href='http://www.samurainoodle.com'>Total</a></b>:",nJam),
                            paste("<b><a href='http://www.samurainoodle.com'>% Tiempo</a></b>:",porcTimeJam),
                            paste("<b><a href='http://www.samurainoodle.com'>Delay</a></b>:",round(delayMean)),
                            paste("<b><a href='http://www.samurainoodle.com'>Level</a></b>:",round(levelMean)),
                            paste("<b><a href='http://www.samurainoodle.com'>Speed Kmh</a></b>:",round(speedkmhMean,2)),
                            paste("<b><a href='http://www.samurainoodle.com'>Hora promedio</a></b>:",timeStr)),
            opacity = porcTimeJam/100)  %>% 
  left_join(segmentUnic, by = 'ID_segmento') %>% st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>%
  st_transform(.,crs = 4326) 


pal <- colorBin(
  bins = 9,
  palette = 'Reds',
  domain = datAgg$porcTimeJam)


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = datAgg,
               weight = ~porcTimeJam*0.05,
               color = ~pal(porcTimeJam),
               fillColor = ~pal(porcTimeJam),
               fillOpacity = ~opacity,
               popup = ~content) %>%
  addLegend(position = 'topleft',pal = pal,values = datAgg$porcTimeJam,
            title = '% Tiempo con reporte',group = 'Flujo')


pp



# highlightOptions = highlightOptions(stroke = 4, weight = 2)
# %>%


### Armo los segmentos como objeto sf
# https://stackoverflow.com/questions/61958296/convert-character-linestring-to-geometry-in-sf

aux = datJam

aux[,'geometry'] = apply(aux[,c('X_lini','Y_lini','X_lfin','Y_lfin')],1,
                         function(xx) {
                           res = paste0('LINESTRING (',paste(xx[c(1,2)], collapse = ' '),' , ',
                                  paste(xx[c(3,4)], collapse = ' '),')')
                           return(res)
                         })

aux_sf <- sf::st_as_sf(aux, wkt = "geometry" )

### Guardo los datos

save(datJam,file = 'BasesR/datJam.RData')

length(unique(c(datJam$street,datJam$endnode)))
length(unique(datJam$endnode))




