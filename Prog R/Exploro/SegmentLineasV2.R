


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


vias = st_read('SHP/v_mdg_vias')

vias_sf = vias %>% st_transform(x = ., crs = 4326)

load('Resultados/001_segmProy.RData')
segm = segmProy %>% st_as_sf(.,wkt = 'geometry',crs = 32721)
segm_sf = st_transform(x = segm, crs = 4326)


### Paradas calles
paradas = st_read('SHP/v_uptu_paradas/v_uptu_paradas.shp',crs = 32721)

paradasUni = paradas %>% group_by(COD_UBIC_P,COD_CALLE1) %>%
  slice_head() %>%
  ungroup()


load('Resultados/001_colleSimpLines.RData')
lineasDF = as.data.frame(data.table::rbindlist(resList)) %>% 
  st_as_sf(.,sf_column_name = 'geometry')

dim(lineasDF)
lineasDF_lf = st_transform(x = lineasDF, crs = 4326)


segm_sf = segm_sf %>% 
  mutate(content = paste(sep = "<br/>",
         paste("<b><a href='http://www.samurainoodle.com'>Calle</a></b>:",NOM_CALLE),
         paste("<b><a href='http://www.samurainoodle.com'>Calle esq</a></b>:",NOM_CALLE_ESQ),
         paste("<b><a href='http://www.samurainoodle.com'>Calle Fin</a></b>:",NOM_CALLE_ESQ_f)))




lineasNew = read.table('SHP/v_uptu_lsv_todas.csv',sep = ',',header = T)
lineasNew_sf = st_as_sf(lineasNew,wkt = 'the_geom',crs = 4326)
lineasNewRef = st_transform(x = lineasNew_sf, crs = 32721)


load('Resultados/001_colleSimpPuntos.RData')
puntosCalles = as.data.frame(data.table::rbindlist(puntList))
puntosCalles = st_as_sf(puntosCalles,sf_column_name = 'geometry',crs = 32721)

###########################################################
#### Intercepto una una calle con el resto de las calles

allCalles = unique(lineasDF$COD_NOMBRE) 
nomCalles = unique(lineasDF[c('NOM_CALLE','COD_NOMBRE')]) %>% st_set_geometry(NULL) 
nomCalles[grep('18 de julio',nomCalles$NOM_CALLE,ignore.case = T),]

listIntCalles = list()
for(hh in allCalles) {

  auxCalle = subset(lineasDF,COD_NOMBRE == hh)
  pruebaInt =  st_intersection(auxCalle %>% select(geometry),
                             subset(lineasDF,COD_NOMBRE != hh)) %>% 
  suppressMessages() %>% suppressWarnings() %>% 
  rename('NOM_ESQ' = 'NOM_CALLE','COD_ESQ' = 'COD_NOMBRE')

if(nrow(pruebaInt) > 0) {
  pruebaInt = pruebaInt %>% mutate(NOM_CALLE = auxCalle$NOM_CALLE,
                                   COD_NOMBRE = hh) %>% st_cast(.,'POINT') %>%
    group_by(COD_ESQ) %>% slice_head() %>% suppressWarnings()
  
  listIntCalles[[length(listIntCalles) + 1]] = pruebaInt
  
  }
}


intCallesDF = as.data.frame(data.table::rbindlist(listIntCalles)) %>% 
  st_as_sf(.,sf_column_name = 'geometry')

###########################################################


listaCallesEsq = list()
for(hh in allCalles) {
  
  pruebaIntB = st_intersection(subset(lineasDF,COD_NOMBRE == hh) %>% select(geometry) %>% st_buffer(.,30),
                               subset(lineasDF,COD_NOMBRE != hh)) %>% 
    st_centroid() %>% 
    suppressMessages() %>% suppressWarnings()
  
  if(nrow( pruebaIntB) > 0) {
    callePunto = subset(puntosCalles,COD_NOMBRE %in% c(hh)) %>% cargaCoords(.) 
    aa = pruebaIntB %>% st_join(callePunto, 
                              join=nngeo::st_nn, maxdist= Inf,k=1) %>%
    arrange(ID_calle) %>% suppressMessages() %>% rename('NOM_ESQ' = 'NOM_CALLE.x',
                                                        'COD_ESQ' = 'COD_NOMBRE.x',
                                                        'NOM_CALLE' = 'NOM_CALLE.y',
                                                        'COD_NOMBRE' = 'COD_NOMBRE.y') %>%
      st_set_geometry(NULL) %>% st_as_sf(.,coords = c('X','Y'),crs = 32721)
  
  
  
  listaCallesEsq[[length(listaCallesEsq) + 1]] = aa
  }

}

CallesEsqDF = as.data.frame(data.table::rbindlist(listaCallesEsq)) %>% 
  st_as_sf(.,sf_column_name = 'geometry')



#############################################
####### Asigna paradas a puntos
callesParadas = sort(unique(paradasUni$COD_CALLE1))
sum(!callesParadas %in% puntosCalles$COD_NOMBRE)
callesParadas = intersect(callesParadas,puntosCalles$COD_NOMBRE)

puntoParList = list()
for(ii in callesParadas) {
      auxC = puntosCalles %>% filter(COD_NOMBRE == ii)
      auxP = paradasUni %>% filter(COD_CALLE1 == ii)
      
      ff = auxP %>% st_join(auxC, 
                  join=nngeo::st_nn, maxdist= Inf,k=1) %>% 
        st_set_geometry(NULL) %>% suppressMessages()
      
      
    filas = unique(c(ff$ID_calle - 1,ff$ID_calle,ff$ID_calle+1))
    
    puntoPar = auxC[filas,] %>% cargaCoords() %>% st_set_geometry(NULL) %>%
      mutate(ID_punto = paste(X,Y,sep = '_')) %>% left_join(ff[,c('ID_calle','COD_UBIC_P')]) %>%
      mutate(COD_UBIC_P = ifelse(is.na(COD_UBIC_P),0,COD_UBIC_P),
             NOM_ESQ = ifelse(COD_UBIC_P > 0,'Par','Int'),
             COD_ESQ = NA) %>%
      arrange(ID_calle) %>% suppressMessages()
      
    puntoParList[[length(puntoParList) + 1]] = puntoPar
}


puntoPar = as.data.frame(data.table::rbindlist(puntoParList))

####################################
##### VECINOS MAS CERCANOS A LAS INTERSECCIONES

puntosIntUniq = intCallesDF %>% cargaCoords(.) %>% 
  mutate(ID_punto = paste(X,Y,sep = '_')) %>% 
  dplyr::select(-NOM_ESQ,-COD_ESQ,-NOM_CALLE,-COD_NOMBRE) %>%
  group_by(ID_punto) %>% slice_head() %>% ungroup()

dim(puntosIntUniq)

##########################
##### Homgeinizo estos puntos




uniqInt = colapsaPuntosCercanos(datPuntos = puntosIntUniq,
                      kvecino = 100,
                      maxdist = 16)

puntosIntUniq = uniqInt[['datos']]
puntosIntUniq = puntosIntUniq %>% st_as_sf(.,coords = c('X','Y'),crs = 32721) %>%
  cargaCoords(.)
##################################


prueba = CallesEsqDF %>% st_join(puntosIntUniq, 
                            join=nngeo::st_nn, maxdist= 30,k=1)




sum(is.na(prueba$ID_punto))
sum(is.na(prueba$ID_row))

faltan = prueba[is.na(prueba$X),]
sonInter = prueba[!is.na(prueba$X),]


puntosIntUniqDep = puntosIntUniq %>% st_set_geometry(NULL) %>% 
  group_by(ID_row) %>% slice_head() %>% ungroup() %>% data.frame()

sonInter = sonInter %>% dplyr::select(-ID_punto,-X,-Y) %>% left_join(puntosIntUniqDep)


uniqfaltan = colapsaPuntosCercanos(datPuntos = faltan,
                                      kvecino = 100,
                                      maxdist = 30)

faltan = uniqfaltan[['datos']]
resnndFinAdapt = uniqfaltan[['refPos']]


rownames(faltan) = 1:nrow(faltan)
faltan[resnndFinAdapt$ID_pos,'ID_row'] = resnndFinAdapt$ID_row



###### Hay algunos puntos que solo aparecen asociados a un punto que ya fue asignado,
###### esto genera que que el ID_pos del punto que parece en el ID_row, 
###### lo cual genera que ese punto se le asigna un ID_row y ademas juega como su posicion
###### por ejemplo, con el ID_row = 50, aparece asignado el ID_pos 51, pero a su vez,
###### En ID_row 51 no se borra porque el ID_pos 4065 solo esta asociado al ID_row 51,
###### EL ID_row 51 no deveria aparecer ya que habia siddo asignado previamentes

puntoFaltsUniq = faltan[unique(resnndFinAdapt$ID_row),] %>% cargaCoords(.) %>%
  mutate(ID_punto = paste(X,Y,sep = '_')) %>% 
  dplyr::select(-NOM_ESQ,-COD_ESQ,-NOM_CALLE,-COD_NOMBRE,-ID_calle,-dirIni,-dirFin) 


faltanFin = faltan %>% dplyr::select(-ID_punto,-X,-Y)  %>% 
  left_join(puntoFaltsUniq %>% st_set_geometry(NULL) %>% 
                                   dplyr::select(ID_punto,ID_row,X,Y))


puntosIntUniqDep = puntosIntUniqDep %>% st_as_sf(.,coords = c('X','Y'),crs = 32721) %>%
  cargaCoords(.)

puntoUniAll = rbind(puntosIntUniqDep,puntoFaltsUniq[,names(puntosIntUniq)])



asignaAll = sonInter  %>% st_set_geometry(NULL) %>%
  rbind(.,faltanFin[,names(sonInter)] %>% st_set_geometry(NULL)) %>% 
  arrange(COD_NOMBRE,ID_calle) %>% mutate(COD_UBIC_P = -1)

asignaAll0 = rbind(asignaAll[,colnames(puntoPar)],puntoPar) %>% 
  group_by(COD_NOMBRE,ID_calle) %>%
  slice_max(COD_UBIC_P) %>% ungroup() %>%
  group_by(COD_NOMBRE,ID_calle,COD_UBIC_P) %>%
  slice_head()

# xx = rbind(asignaAll[,colnames(puntoPar)],puntoPar) %>% 
#   group_by(COD_NOMBRE,ID_calle) %>%
#   slice_head()
# 
# dim(unique(allCal[,c('COD_NOMBRE','ID_calle')]))

##### ARMA LOS SEGEMNTOS DE CADA CALLE

segmCalles = asignaAll0 %>% group_by(COD_NOMBRE,ID_punto) %>% slice_head() %>%
  ungroup() %>%
  group_by(COD_NOMBRE) %>%
  arrange(ID_calle) %>%
  mutate(Xf = lead(X),
         Yf = lead(Y),
         numSeg = row_number(),
         ID_segm = paste(COD_NOMBRE,numSeg,sep = '_')) %>%
  filter(!is.na(Xf)) %>% 
  mutate(geometry = paste('LINESTRING (',X,' ',Y,',',Xf,' ',Yf,')')) %>%
  ungroup() %>%
  st_as_sf(.,wkt = c('geometry'),crs = 32721) %>%
  st_transform(x = .,crs = 4326)
  
  


segmCallesCentrid = st_centroid(segmCalles) %>% st_transform(x = .,crs = 32721)
segmCallesCentrid_lf = segmCallesCentrid %>% st_transform(x = .,crs = 4326)



tab = table(res$NOM_CALLE)  
calleRef =  names(tab)[tab >= 6] 

resb = res %>% filter(NOM_CALLE %in% calleRef)

vias_sf = vias  %>%  st_transform(x = ., crs = 4326)







ff = puntoUniAll %>%  st_transform(x = ., crs = 4326)

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  # addCircles(data = pruebaInt) %>%
  # addCircles(data = bb,
  #            popup=~content) %>%
  addCircles(data = segmCallesCentrid_lf,
             color = 'orange',
             weight = 0.5) %>%
  # addCircles(data = ff,
  #            color = 'orange',
  #            weight = 1) %>%
  # addCircles(data = pruebaIntV,color = 'green') %>%
  # addPolylines(data = lineasDF_lf,
  #              color = 'red',
  #              weight = 0.3) %>%
  addPolylines(data = segmCalles,
               color = 'red',
               weight = 1)

# %>%
#   addCircles(data = ccalle)
pp

###################################################
####### Asigna lineas a calles

### Identificar que calles
num = 500
df = data.frame(st_line_sample(lineasNewRef[num,], density = 1/10) %>% st_cast("POINT")) %>% 
  st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>% mutate(ID = row_number())


ggplot() + geom_sf() +
  geom_sf(data = lineasNewRef[num,])  +
  geom_sf(data = df,aes(colour = ID))



##### Joint con los puntos (vecinos mas cercanos)


varMaximal = unique(lineasNewRef$cod_variante_maximal)
varMaximal = varMaximal[!is.na(varMaximal)]


seqCallesVar = list()
for(ii in varMaximal) {

  
  df = data.frame(st_line_sample(subset(lineasNewRef,cod_variante == ii), density = 1/10) %>% st_cast("POINT")) %>% 
    st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>% mutate(ID = row_number())
  
  intPunt = df %>% st_join(puntosCalles, 
                                join=nngeo::st_nn, maxdist= 30,k=1) %>%
    st_set_geometry(NULL) %>%
    filter(!is.na(COD_NOMBRE)) %>%
    mutate(COD_lag = lag(COD_NOMBRE),
           calleD = ifelse((COD_NOMBRE != COD_lag) | is.na(COD_lag),1,0),
           seqCalle = cumsum(calleD)) %>%
    group_by(NOM_CALLE,COD_NOMBRE,seqCalle) %>%
    summarise(count = n())  %>%
  ungroup() %>%
    filter(count > 6) %>%
    arrange(seqCalle) %>%
    mutate(COD_lag = lag(COD_NOMBRE),
           calleD = ifelse((COD_NOMBRE != COD_lag) | is.na(COD_lag),1,0),
           seqCalle = cumsum(calleD)) %>%
    group_by(NOM_CALLE,COD_NOMBRE,seqCalle) %>%
    summarise(count = sum(count))  %>%
    ungroup() %>%
    arrange(seqCalle) %>% suppressMessages() %>% suppressWarnings()
  
  seqCallesVar[[length(seqCallesVar) + 1]] = intPunt
  
}

names(seqCallesVar) = as.character(varMaximal)
  

######################################################
######## ENcuentra secuencia de segmentos


hh = 8552
useCalle = seqCallesVar[[as.character(hh)]]


df = data.frame(st_line_sample(subset(lineasNewRef,cod_variante == hh), density = 1/10) %>% st_cast("POINT")) %>% 
  st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>% mutate(ID_lin = row_number(),
                                                                 cod_variante = hh)

head(segmCallesCentrid)

match = do.call(rbind,
                sapply(1:nrow(useCalle),function(xx) {
                  # for(xx in 1:nrow(useCalle)) {
                  codCalle = as.character(useCalle[xx,'COD_NOMBRE'])
                  NumCalle = as.numeric(useCalle[xx,'seqCalle'])
                  aux = subset(segmCallesCentrid,COD_NOMBRE == codCalle)
                  
                  # linAux = subset(linaVar,COD_CALLE1 == codCalle)
                  
                  matchAux <- df %>% 
                    st_join(aux, join=nngeo::st_nn, maxdist= 30,k=1) %>% 
                    filter(!is.na(COD_NOMBRE)) %>%
                    suppressMessages()
                  
                  matchAux$NumCalle = NumCalle
                  # } 
                  list(matchAux)
                })
)

match = match %>% arrange(ID_lin)


AV = nomCalles[grep('av |BV |RBLA ',nomCalles$NOM_CALLE,ignore.case = T),]

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  # addPolylines(data = segmCalles %>% filter(ID_segm %in% match$ID_segm),
  #              color = 'red',
  #              weight = 3) %>%
  addPolylines(data = segmCalles,
               color = 'red',
               weight = 3) %>%
  addPolylines(data = segmCalles %>% filter(COD_NOMBRE %in% c(AV$COD_NOMBRE)),
               color = 'green',
               weight = 3) %>%
  addPolylines(data = vias_sf %>% filter(COD_NOMBRE %in% c(AV$COD_NOMBRE)),
               color = 'orange',
               weight = 1) %>%
  # addCircles(data = segmCallesCentrid %>%  
  #              st_transform(x = ., crs = 4326)) %>%
  addPolylines(data = subset(lineasNew_sf,cod_variante == hh),
               color = 'blue',
               weight = 1)

# %>%
#   addCircles(data = ccalle)
pp


####################################################
####################################################

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
addPolylines(data = lineasNew_sf[num,],
             color = 'red',
             weight = 1) %>%
  # addCircles(data = puntosCalles %>% filter(NOM_CALLE == 'TURQUIA') %>% 
  #              st_transform(x = ., crs = 4326)) %>%
  addPolylines(data = vias_sf %>% filter(COD_NOMBRE == 7008),
               color = 'red',
               weight = 1)

# %>%
#   addCircles(data = ccalle)
pp

