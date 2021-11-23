

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



###############################################
###### Exploro casustica

#### Datos atascos
load('BasesR/datJam.RData')

###
head(datJam)

### Paso a sf un df de lineas
datJam[,'geometry'] = apply(datJam[,c('X_lini','Y_lini','X_lfin','Y_lfin')],1,
                         function(xx) {
                           res = paste0('LINESTRING (',paste(xx[c(2,1)], collapse = ' '),' , ',
                                        paste(xx[c(4,3)], collapse = ' '),')')
                           return(res)
                         })

segment_sf <- sf::st_as_sf(datJam, wkt = "geometry" )


datJam[,'geometry'] = apply(datJam[,c('Xc','Yc')],1,
                                function(xx) {
                                  res = paste0('POINT (',paste(xx[c(2,1)], collapse = ' '),')')
                                  return(res)
                                })


points_sf <- sf::st_as_sf(datJam, wkt = "geometry" )


###################################
##
sum(datJam$street == '')

sinCalleL = segment_sf[segment_sf$street == '',]
sinCalleP = points_sf[points_sf$street == '',]

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = sinCalleL) %>%
  addCircles(data = sinCalleP,weight = 2,fill = TRUE,
             group = 'Flujo',
             color = 'red',
             fillColor = 'red') # %>%
  
  



ids = segment_sf %>% st_set_geometry(NULL) %>% group_by(entity_id) %>% 
  summarise(n = n()) %>% arrange(desc(n))

ID = 'waze:jam:136009864'

auxL = segment_sf[segment_sf$entity_id == ID,] %>% arrange(desc(datemodified))
auxP = points_sf[segment_sf$entity_id == ID,] %>% arrange(desc(datemodified))


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = auxL,weight = 0.7) %>%
  addCircles(data = auxP,weight = 0.5,fill = TRUE,
             group = 'Flujo',
             color = 'red',
             fillColor = 'red')


  pp
  
  
  
  
  addLegend(position = 'topleft',pal = pal,values = dat$SumTot,
            title = 'Flujo',group = 'Flujo') %>% 
  addLayersControl(
    baseGroups = c("CartoDB.Positron","OSM"),
    overlayGroups = c('Flujos','Paradas'),
    options = layersControlOptions(collapsed = F)) %>%
  hideGroup('Paradas') 


pp


aa = auxL %>% st_set_geometry(NULL) %>% select(Xc,Yc,X_lini,Y_lini,X_lfin,Y_lfin) %>%
  unique(.)

dim(aa)


datJam[grep('quijano',datJam$endnode,ignore.case = T),]
