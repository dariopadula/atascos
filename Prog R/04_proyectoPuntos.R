

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


#########################################
#########################################
##### Cargo bases
# atascos
load('BasesR/datJam.RData')
# Carga los segmentos
load('Resultados/segmProy.RData')

### Paso a sf un df de lineas
datJam[,'geometry'] = apply(datJam[,c('X_lini','Y_lini','X_lfin','Y_lfin')],1,
                            function(xx) {
                              res = paste0('LINESTRING (',paste(xx[c(1,2)], collapse = ' '),' , ',
                                           paste(xx[c(3,4)], collapse = ' '),')')
                              return(res)
                            })

segment_sf <- sf::st_as_sf(datJam, wkt = "geometry" )


datJam[,'geometry'] = apply(datJam[,c('Xc','Yc')],1,
                            function(xx) {
                              res = paste0('POINT (',paste(xx[c(1,2)], collapse = ' '),')')
                              return(res)
                            })


points_sf <- sf::st_as_sf(datJam, wkt = "geometry",crs = 4326) %>% st_transform(.,crs = 32721)




#############################################################
########### Encuentra segemnto de origen
head(segmProy)

segmProy_cent =segmProy %>% st_as_sf(.,wkt = "geometry",crs = 32721) %>% 
  st_centroid(.) %>% 
  suppressWarnings()


prueba = points_sf %>% st_join(segmProy_cent[,'COD_NOMBRE','ID_segm'], 
                               join=nngeo::st_nn, maxdist= Inf,k=1)



prueba = prueba[!is.na(prueba$street_cod),]

ind = ifelse(prueba$street_cod == prueba$COD_NOMBRE,1,0)
table(ind)


dd = '{coordinates=[[-34.916728, -56.15474], [-34.916025, -56.154332], [-34.915037, -56.153752], [-34.914519, -56.153453], [-34.914336, -56.153405], [-34.914101, -56.153428]], type=LineString}'

ddd = gsub('\\{coordinates=\\[\\[','LINESTRING (',dd) 
ddd = gsub('\\,','',ddd)
ddd = gsub('\\] \\[',', ',ddd)
ddd = gsub('\\]\\] type=LineString\\}',')',ddd)

df = data.frame(ddd)
df = st_as_sf(df,wkt = 'ddd',crs = 4326)


opar = par(mfrow = c(1, 2))

plot(df)
seg = st_segments(df, progress = FALSE)
plot(seg, col = rainbow(nrow(seg)))
text(st_coordinates(st_centroid(seg)), as.character(1:nrow(seg)))
