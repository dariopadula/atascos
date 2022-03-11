
load('Resultados/001_segmProy.RData')
segm = segmProy %>% st_as_sf(.,wkt = 'geometry',crs = 32721)

segm_sf = st_transform(x = segm, crs = 4326)



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

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = segm_sf,
               color = 'blue',
               weight = 0.7,
               popup=~content) %>%
  addPolylines(data = lineasDF_lf,
               color = 'orange',
               weight = 0.7)

# %>%
#   addPolylines(data = lineasNew_sf,
#                color = 'red',
#                weight = 0.3)
pp



lineasNew = read.table('SHP/v_uptu_lsv_todas.csv',sep = ',',header = T)

lineasNew_sf = st_as_sf(lineasNew,wkt = 'the_geom',crs = 4326)

lineasNewRef = st_transform(x = lineasNew_sf, crs = 32721)


load('Resultados/001_colleSimpPuntos.RData')
puntosCalles = as.data.frame(data.table::rbindlist(puntList))

puntosCalles = st_as_sf(puntosCalles,sf_column_name = 'geometry',crs = 32721)
#### Intercepto una linea

res = st_intersection(lineasNewRef[1,]  %>% st_buffer(.,10) %>% select(the_geom),
                      puntosCalles) 
  suppressMessages() %>% suppressWarnings()

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


#hh = "AV LIB BRIG GRAL LAVALLEJA"
hh = "AV GRAL GARIBALDI"
pruebaInt =  st_intersection(subset(lineasDF,NOM_CALLE == hh) %>% select(geometry),
                             subset(lineasDF,NOM_CALLE != hh)) %>% st_transform(x = ., crs = 4326) %>%
  suppressMessages() %>% suppressWarnings()


pruebaIntB = st_intersection(subset(lineasDF,NOM_CALLE == hh) %>% select(geometry) %>% st_buffer(.,20),
                             subset(lineasDF,NOM_CALLE != hh)) %>% 
  st_centroid() %>% 
suppressMessages() %>% suppressWarnings()


aa = pruebaIntB %>% st_join(subset(puntosCalles,NOM_CALLE %in% c(hh)), 
                                   join=nngeo::st_nn, maxdist= Inf,k=1) %>%
  arrange(ID_calle)



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

# hh = 'AV DE LAS LEYES'

pruebaIntV =  st_intersection(subset(vias,NOM_CALLE == hh) %>% select(geometry),
                              subset(vias,NOM_CALLE != hh)) %>% 
  st_cast(.,'POINT') %>% 
  cargaCoords(.) %>% 
  st_set_geometry(NULL) %>%
  group_by(COD_NOMBRE) %>%
  summarise(NOM_CALLE = first(NOM_CALLE),
            X = mean(X),
            Y = mean(Y)) %>% st_as_sf(.,coords = c('X','Y'),crs = 32721) %>%
  st_transform(x = ., crs = 4326) %>%
  suppressMessages() %>% suppressWarnings()
  


####################################
##### VECINOS MAS CERCANOS A LAS INTERSECCIONES

puntosIntUniq = intCallesDF %>% cargaCoords(.) %>% 
  mutate(ID_punto = paste(X,Y,sep = '_')) %>% 
  dplyr::select(-NOM_ESQ,-COD_ESQ,-NOM_CALLE,-COD_NOMBRE) %>%
  group_by(ID_punto) %>% slice_head() %>% ungroup()

dim(puntosIntUniq)

##########################
##### Homgeinizo estos puntos

resInt = st_nn(
  x = puntosIntUniq,
  y = puntosIntUniq,
  sparse = TRUE,
  k = 100,
  maxdist = 16,
  returnDist = T,
  progress = F,
  parallel = 1
)


resnnInt = do.call(rbind,lapply(resInt$nn,function(xx) {
  aux = data.frame(ID_row = xx[1],ID_pos = xx)
  return(aux)
}) )

resddInt = do.call(rbind,lapply(resInt$dist,function(xx) {
  aux = data.frame(dist = xx)
  return(aux)
}) )

resnndInt = data.frame(resnnInt,resddInt) %>% filter(dist < 16)
resnndFinInt = resnndInt[!duplicated(resnndInt$ID_pos),]

nnInt = resnndFinInt %>% group_by(ID_row) %>% 
  summarise(n = n(),
            dmin = min(dist),
            pp = first(ID_pos)) %>% filter(dmin > 0)


resnndFinIntAdapt = resnndFinInt

for(ii in nn$ID_row) {
  resnndFinIntAdapt[resnndFinIntAdapt$ID_pos == ii,c('ID_row','dist')] = c(ii,0)
}

resnndFinIntAdapt = resnndFinIntAdapt %>% arrange(ID_row,dist)


puntosIntUniq[resnndFinIntAdapt$ID_pos,'ID_row'] = resnndFinIntAdapt$ID_row

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



res = st_nn(
  x = faltan,
  y = faltan,
  sparse = TRUE,
  k = 100,
  maxdist = 30,
  returnDist = T,
  progress = F,
  parallel = 1
)

resnn = do.call(rbind,lapply(res$nn,function(xx) {
  aux = data.frame(ID_row = xx[1],ID_pos = xx)
  return(aux)
}) )

resdd = do.call(rbind,lapply(res$dist,function(xx) {
  aux = data.frame(dist = xx)
  return(aux)
}) )

resnnd = data.frame(resnn,resdd) %>% filter(dist < 30)
resnndFin = resnnd[!duplicated(resnnd$ID_pos),]


nn = resnndFin %>% group_by(ID_row) %>% 
  summarise(n = n(),
            dmin = min(dist),
            pp = first(ID_pos)) %>% filter(dmin > 0)


resnndFinAdapt = resnndFin

for(ii in nn$ID_row) {
  resnndFinAdapt[resnndFinAdapt$ID_pos == ii,c('ID_row','dist')] = c(ii,0)
}

resnndFinAdapt = resnndFinAdapt %>% arrange(ID_row,dist)

sort(faltan[nn$ID_row,'ID_row']$ID_row)

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


# asignaAll = prueba %>% filter(!is.na(ID_punto)) %>% st_set_geometry(NULL) %>%
#   rbind(.,faltanFin[,names(prueba)] %>% st_set_geometry(NULL)) %>% 
#   arrange(COD_NOMBRE,ID_calle)

asignaAll = sonInter  %>% st_set_geometry(NULL) %>%
  rbind(.,faltanFin[,names(sonInter)] %>% st_set_geometry(NULL)) %>% 
  arrange(COD_NOMBRE,ID_calle)

nomCalles[grep('18 de julio',nomCalles$NOM_CALLE,ignore.case = T),]


ccalle = asignaAll %>% filter(COD_NOMBRE %in% c(2354)) %>% st_as_sf(.,coords = c('X','Y'),crs = 32721) %>%
  st_transform(x = .,crs = 4326)

# pruebaIntV =  st_intersection(subset(vias,NOM_CALLE == hh) %>% select(geometry),
#                              subset(vias,NOM_CALLE != hh)) %>% st_transform(x = ., crs = 4326) %>%
#   st_cast(.,'POINT') %>%
#   suppressMessages() %>% suppressWarnings()

##### ARMA LOS SEGEMNTOS DE CADA CALLE

segmCalles = asignaAll %>% group_by(COD_NOMBRE,ID_punto) %>% slice_head() %>%
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
  
  






tab = table(res$NOM_CALLE)  
calleRef =  names(tab)[tab >= 6] 

resb = res %>% filter(NOM_CALLE %in% calleRef)

vias_sf = vias  %>%  st_transform(x = ., crs = 4326)

bb = CallesEsqDF   %>%  st_transform(x = ., crs = 4326) %>%
  mutate(content = paste(sep = "<br/>",
                         paste("<b><a href='http://www.samurainoodle.com'>Calle</a></b>:",NOM_CALLE),
                         paste("<b><a href='http://www.samurainoodle.com'>Esquina</a></b>:",NOM_ESQ)))


cc = intCallesDF %>%  st_transform(x = ., crs = 4326) %>%
  mutate(content = paste(sep = "<br/>",
                         paste("<b><a href='http://www.samurainoodle.com'>Calle</a></b>:",NOM_CALLE),
                         paste("<b><a href='http://www.samurainoodle.com'>Esquina</a></b>:",NOM_ESQ)))


dd = prueba %>% st_transform(x = ., crs = 4326)


ee = faltan %>%  st_transform(x = ., crs = 4326) %>%
  mutate(content = paste(sep = "<br/>",
                         paste("<b><a href='http://www.samurainoodle.com'>Calle</a></b>:",NOM_CALLE),
                         paste("<b><a href='http://www.samurainoodle.com'>Esquina</a></b>:",NOM_ESQ)))

ff = puntoUniAll %>%  st_transform(x = ., crs = 4326)

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  # addCircles(data = pruebaInt) %>%
  # addCircles(data = bb,
  #            popup=~content) %>%
  # addCircles(data = cc,
  #            color = 'orange',
  #            weight = 0.5,
  #            popup=~content) %>%
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


