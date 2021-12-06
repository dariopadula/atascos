
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
######################################################
###### LEE base

# datos = read.table('Datos/etwazetrafficjam_202111261100.csv',sep = ',',header = T)
# datos = read.table('Datos/etwazetrafficjam_202112012024.csv',sep = ',',header = T)
datos = read.table('Datos/etwazetrafficjam_202112061701.csv',sep = ',',header = T)

#############################################
#### Me quedo solo con los atascos
datJam = subset(datos,delay >= 0)

#############################################
#### Paso las fecas de character a dates

datJam = datJam %>% mutate(datecreated = as.POSIXct(strptime(datecreated, "%Y-%m-%d %H:%M:%S")),
                           datemodified = as.POSIXct(strptime(datemodified, "%Y-%m-%d %H:%M:%S")),
                           datepublished = as.POSIXct(strptime(datepublished, "%Y-%m-%d %H:%M:%S"))) %>%
  filter(datecreated >= as.POSIXct(strptime('2021-11-27 00:00:01', "%Y-%m-%d %H:%M:%S")))

#############################################
##### EXTRAE LAS COORDENADAS

### del Centroide y de los extremos del segemnto
# datJam = datJam %>% getCoordCent(.) %>% getLineGeomV2(.) %>%
datJam = datJam %>% arrange(desc(entity_id),desc(datemodified)) %>% 
  getLineGeomV2(.) %>% 
    dplyr::select(-city,-country,-location_centroid,-location,-entity_type,-fiware_servicepath,-pubmillis,-turntype) %>%
  mutate(diaStr = substr(datemodified,1,10),
         diaSem = weekdays(datemodified),
         finDeSem = ifelse(diaSem %in% c('sábado','domingo'),'Fin de semana','Lunes a viernes'),
         hora = hour(datemodified),
         minuto = minute(datemodified),
         minutoHora = 60*hora + minuto,
         diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo'))) %>%
  st_as_sf(.,wkt = 'geometry',crs = 4326) %>%
  st_transform(.,crs = 32721) %>%
  mutate(ID_Base = row_number())



datJam_df = datJam %>% st_set_geometry(NULL)

save(datJam,file = 'BasesR/datJam.RData')
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



######## Agrega ID segmento

datJamCentr = datJamCentr %>% st_join(centSegUnic[,'ID_segmento'], 
                                    join=nngeo::st_nn, maxdist= Inf,k=1) %>%
  suppressMessages()


datJamSegm = datJamSegm %>% left_join(datJamCentr %>% 
                                        st_set_geometry(NULL) %>%
                                        dplyr::select(ID_fila,ID_segmento),by = 'ID_fila')


save(datJamSegm,file = 'Resultados/datJamSegm.RData')
###############################################################
########### Calculo variables para agreagar

## Uso el date modification para buscar por tiempo

# datJamSegm = datJamSegm %>% mutate(diaStr = substr(datemodified,1,10),
#                                    diaSem = weekdays(datemodified),
#                                    finDeSem = ifelse(diaSem %in% c('sábado','domingo'),'Fin de semana','Entre semana'),
#                                    hora = hour(datemodified),
#                                    minuto = minute(datemodified),
#                                    minutoHora = 60*hora + minuto)


disOrd = names(table(datJamSegm$diaStr))

datJamSegm = datJamSegm %>% mutate(diaStr = factor(diaStr,levels = disOrd))



###########################################
##### GUARDO SHAPE SEGEMNTOS

# datJamSegm_sp = as(datJamSegm,Class = 'Spatial')
#####################################################
####### GUARDO EL SHAPE
# writeOGR(datJamSegm_sp, ".", "SHP_datJamSegm_sp", driver="ESRI Shapefile",overwrite_layer = T)

# writeOGR(meuse, "test_geojson", layer="meuse", driver="GeoJSON")

# class(datJamSegm)
# 
# st_write(datJamSegm, "datJamSegm.geojson")
####################################
##### Lo paso a geoJson


# geoJ = sf_geojson(datJamSegm)
# geoJ_at = sf_geojson(sf, atomise = T)


datJamSegm_df = datJamSegm %>% st_set_geometry(NULL)


#########################################################
########### GUardo insumos shiny

save(datJam_df,datJamSegm_df,segmentUnic, file = 'Shiny/Insumos/01_InsumosShiny.RData')
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
            porcTimeJamO = round(100*nJam/nInter,1),
            porcTimeJam = min(100,round(100*nJam/nInter,1)),
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
  domain = seq(0,100,5))


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

setView(map = pp, lng =  -56.164532, lat = -34.903112, zoom = 13, options = list())

# highlightOptions = highlightOptions(stroke = 4, weight = 2)
# %>%
# lng =  -56.164532, lat = -34.901112

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


######################################################
####### Descriptivos Jams

horaIni = 17
minIni = 0

horaFin = 18
minFin = 0



minHoraIni = horaIni*60 + minIni
minHoraFin = horaFin*60 + minFin



datJam_df_filtro = datJam_df %>% filter(finDeSem == 'Entre semana' & minutoHora >= minHoraIni &
                                          minutoHora <= minHoraFin)


nDays = length(unique(as.character(datJam_df_filtro$diaStr)))
larInter = 5
nInter = ((minHoraFin - minHoraIni)/larInter + 1)*nDays


summar = datJam_df_filtro %>% group_by(street) %>%
  summarise(count = n(),
            delayMean = round(mean(delay,na.rm = T)),
            levelMean = mean(level,na.rm = T),
            level45 = round(100*sum(level %in% c(4,5)/nInter)),
            lengthMean = round(mean(length,na.rm = T)),
            speedkmhMean = round(mean(speedkmh,na.rm = T)),
            minutoHoraMean = mean(minutoHora,na.rm = T)) %>%
  ungroup() %>%
  mutate(horaMedia = floor(minutoHoraMean/60),
          minMedio = round(60*(minutoHoraMean/60 - horaMedia)),
           timeStr = ifelse(nchar(minMedio) == 1,paste0(horaMedia,':0',minMedio),
                           paste0(horaMedia,':',minMedio))) %>%
  dplyr::select(-horaMedia,-minMedio,-minutoHoraMean) %>%
  arrange(desc(count))


####################################
###### GRAFICO DE CALOR



varArrang = 'count'
nCalles = 50

datFull = datJam_df %>% mutate(interv = ceiling(minutoHora/largoInt)) %>% 
  group_by(street) %>%
  summarise(count = n(),
            delayMean = round(mean(delay,na.rm = T)),
            levelMean = mean(level,na.rm = T),
            level345 = sum(level %in% c(3,4,5)),
            lengthMean = round(mean(length,na.rm = T))) %>%
  ungroup() %>%
  arrange(desc(across(all_of(varArrang)))) %>% head(nCalles) 



largoInt = 15

interv = unique(ceiling(1:(23*60 + 60)/largoInt))

tiempoHora = floor(interv*largoInt/60)
tiempoMin = 60*(interv*largoInt/60 - tiempoHora)
tiempo = ifelse(nchar(tiempoMin) == 1,paste0(tiempoHora,':0',tiempoMin),
                paste0(tiempoHora,':',tiempoMin))


calles = unique(datFull$street)
ncalles = length(calles)

dfInterv = data.frame(interv = rep(interv,ncalles),
                      street = rep(calles,each = length(interv)),
                      hora = rep(tiempo,ncalles))


datFilter = subset(datJam_df,street %in% datFull$street)

dataAux = datFilter %>% 
  mutate(interv = ceiling(minutoHora/largoInt)) %>% 
  group_by(street,interv) %>%
  summarise(count = n(),
            delayMean = round(mean(delay,na.rm = T)),
            levelMean = mean(level,na.rm = T),
            level345 = sum(level %in% c(3,4,5)),
            lengthMean = round(mean(length,na.rm = T))) %>%
  ungroup() %>%
  right_join(dfInterv,by = c('street','interv')) %>%
  replace(is.na(.), 0) %>%
  mutate(streetF = factor(street,levels = datFull$street),
         hora = factor(hora,levels = tiempo))


##### Plot

library(viridis)
library(ggExtra)
library(plotly)


p <-ggplot(dataAux,aes(hora,streetF,fill=level345))+
  geom_tile() + 
  scale_fill_gradient(low = "white",
                      high = "red") + 
  # scale_fill_gradient2(low = "white",
  #                      mid = "yellow",
  #                      high = "red",
  #                      midpoint = 10) +
  theme_minimal()


# p <-p + theme_minimal(base_size = 8)
#p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(axis.text.x=element_text(angle=90)) +
  theme(strip.background = element_rect(colour="white"))+
  # theme(plot.title=element_text(hjust=0))+
  # theme(axis.ticks.y=element_blank())+
  theme(axis.text=element_text(size=7)) +
  guides(fill=guide_legend(title="pepe"))
  # theme(legend.title=element_text(title='pepe'))
  # theme(legend.text=element_text(size=6))+
  # removeGrid()

ggplotly(p)





