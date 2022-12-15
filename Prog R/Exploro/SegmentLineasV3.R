

# Librerias ---------------------------------------------------------------



library(tidyverse)
library(here)
library(data.table)
library(sf)
library(htmlwidgets)
library(leaflet)
library(lubridate)
library(parallel)
library(nngeo)
library(viridis)

library(geojsonsf)
library(rgdal)



# Carga Funciones ---------------------------------------------------------



fun = dir('Funciones/')
fun = fun[grep('\\.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))

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


# Carga SHP y datos -------------------------------------------------------


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
         paste("<b><aCalle</a></b>:",NOM_CALLE),
         paste("<b><aCalle esq</a></b>:",NOM_CALLE_ESQ),
         paste("<b><aCalle Fin</a></b>:",NOM_CALLE_ESQ_f)))




lineasNew = read.table('SHP/v_uptu_lsv_todas.csv',sep = ',',header = T)
lineasNew_sf = st_as_sf(lineasNew,wkt = 'the_geom',crs = 4326)
lineasNewRef = st_transform(x = lineasNew_sf, crs = 32721)


load('Resultados/001_colleSimpPuntos.RData')
puntosCalles = as.data.frame(data.table::rbindlist(puntList))
puntosCalles = st_as_sf(puntosCalles,sf_column_name = 'geometry',crs = 32721)


# Intecepto calles con el resto de las calles -----------------------------


#### Intercepto una una calle con el resto de las calles

allCalles = unique(lineasDF$COD_NOMBRE) 
nomCalles = unique(lineasDF[c('NOM_CALLE','COD_NOMBRE')]) %>% st_set_geometry(NULL) 
nomCalles[grep('leyes',nomCalles$NOM_CALLE,ignore.case = T),]

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


save(intCallesDF,file = 'Prog R/Exploro/BasesIntermedias/intCallesDF.RData')
# load('Prog R/Exploro/BasesIntermedias/intCallesDF.RData')


# Lista Calles con esquinas -----------------------------------------------



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


save(CallesEsqDF,file = 'Prog R/Exploro/BasesIntermedias/CallesEsqDF.RData')
# load('Prog R/Exploro/BasesIntermedias/CallesEsqDF.RData')

# Asigna paradas a puntos -------------------------------------------------


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


save(puntoPar,file = 'Prog R/Exploro/BasesIntermedias/puntoPar.RData')
# load('Prog R/Exploro/BasesIntermedias/puntoPar.RData')


# Homogeiniza puntos ------------------------------------------------------


##### VECINOS MAS CERCANOS A LAS INTERSECCIONES

puntosIntUniq = intCallesDF %>% cargaCoords(.) %>% 
  mutate(ID_punto = paste(X,Y,sep = '_')) %>% 
  dplyr::select(-NOM_ESQ,-COD_ESQ,-NOM_CALLE,-COD_NOMBRE) %>%
  group_by(ID_punto) %>% slice_head() %>% ungroup()

dim(puntosIntUniq)

##### Homgeinizo estos puntos

uniqInt = colapsaPuntosCercanos(datPuntos = puntosIntUniq,
                      kvecino = 100,
                      maxdist = 25) # 16

puntosIntUniq = uniqInt[['datos']]
puntosIntUniq = puntosIntUniq %>% st_as_sf(.,coords = c('X','Y'),crs = 32721) %>%
  cargaCoords(.)



prueba = CallesEsqDF %>% st_join(puntosIntUniq, 
                            join=nngeo::st_nn, maxdist= 30,k=1)




sum(is.na(prueba$ID_punto))
sum(is.na(prueba$ID_row))


sonInter = prueba[!is.na(prueba$X),]


puntosIntUniqDep = puntosIntUniq %>% st_set_geometry(NULL) %>% 
  group_by(ID_row) %>% slice_head() %>% ungroup() %>% data.frame()

sonInter = sonInter %>% dplyr::select(-ID_punto,-X,-Y) %>% left_join(puntosIntUniqDep)


# Integro puntos que faltan -----------------------------------------------


faltan = prueba[is.na(prueba$X),]
uniqfaltan = colapsaPuntosCercanos(datPuntos = faltan,
                                      kvecino = 100,
                                      maxdist = 40) #30

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



# Junta todos los puntos unificados ---------------------------------------



puntosIntUniqDep = puntosIntUniqDep %>% st_as_sf(.,coords = c('X','Y'),crs = 32721) %>%
  cargaCoords(.)

puntoUniAll = rbind(puntosIntUniqDep,puntoFaltsUniq[,names(puntosIntUniq)])

save(puntoUniAll,file = 'Prog R/Exploro/BasesIntermedias/puntoUniAll.RData')
# load('Prog R/Exploro/BasesIntermedias/puntoUniAll.RData')

asignaAll = sonInter  %>% st_set_geometry(NULL) %>%
  rbind(.,faltanFin[,names(sonInter)] %>% st_set_geometry(NULL)) %>% 
  arrange(COD_NOMBRE,ID_calle) %>% mutate(COD_UBIC_P = -1)


save(asignaAll,file = 'Prog R/Exploro/BasesIntermedias/asignaAll.RData')
# load('Prog R/Exploro/BasesIntermedias/asignaAll.RData')

asignaAll0 = rbind(asignaAll[,colnames(puntoPar)],puntoPar) %>% 
  group_by(COD_NOMBRE,ID_calle) %>%
  slice_max(COD_UBIC_P) %>% ungroup() %>%
  group_by(COD_NOMBRE,ID_calle,COD_UBIC_P) %>%
  slice_head()


# Construye los segmentos -------------------------------------------------



##### ARMA LOS SEGEMNTOS DE CADA CALLE

# segmCalles = asignaAll0 %>% group_by(COD_NOMBRE,ID_punto) %>% slice_head() %>%
#   ungroup() %>%
#   group_by(COD_NOMBRE) %>%
#   arrange(ID_calle) %>%
#   mutate(Xf = lead(X),
#          Yf = lead(Y),
#          numSeg = row_number(),
#          ID_segm = paste(COD_NOMBRE,numSeg,sep = '_')) %>%
#   ungroup() %>%
#   mutate(Xs_lead = lead(X),
#          Xs_lag = lag(X)) %>%
#          SentChang )



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
  
save(segmCalles,file = 'Prog R/Exploro/BasesIntermedias/segmCalles.RData')
# load('Prog R/Exploro/BasesIntermedias/segmCalles.RData')

# Calcula centroides de los segmentos -------------------------------------


segmCallesCentrid = st_centroid(segmCalles) %>% st_transform(x = .,crs = 32721) %>%
  suppressWarnings()
segmCallesCentrid_lf = segmCallesCentrid %>% st_transform(x = .,crs = 4326)

segmCallesPuntos = segmCalles %>% st_cast(x = .,'POINT') %>% st_transform(x = .,crs = 32721)



# Hace mapa con los segmentos ---------------------------------------------

ff = puntoUniAll %>%  st_transform(x = ., crs = 4326)

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = segmCalles,
               color = 'red',
               weight = 1,
               popup = ~NOM_CALLE) 


pp


# Ecuentra la secuencia de calles de la linea ---------------------------

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
  
save(seqCallesVar,file = 'Prog R/Exploro/BasesIntermedias/seqCallesVar.RData')
# load('Prog R/Exploro/BasesIntermedias/seqCallesVar.RData')
# Prototipo para asignar lineas a los segmentos ---------------------------
######## ENcuentra secuencia de segmentos
names(seqCallesVar)
# hh = 8860
# hh = 340
#hh = 8123
# hh = 8121
# hh = 8316
# hh = 8277
# hh = 8767 # Ida y vuelta por la misma calle
# hh = 8862 # Ida y vuelta por la misma calle
hh = 8603
useCalle = seqCallesVar[[as.character(hh)]]


df = data.frame(st_line_sample(subset(lineasNewRef,cod_variante == hh), density = 1/5) %>% st_cast("POINT")) %>% 
  st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>% mutate(ID_lin = row_number(),
                                                                 cod_variante = hh)

head(segmCallesCentrid)

match = do.call(rbind,
                sapply(1:nrow(useCalle),function(xx) {
                # for(xx in 1:nrow(useCalle)) {  
                  
                  # for(xx in 1:nrow(useCalle)) {
                  codCalle = as.character(useCalle[xx,'COD_NOMBRE'])
                  NumCalle = as.numeric(useCalle[xx,'seqCalle'])
                  # aux = subset(segmCalles,COD_NOMBRE == codCalle) %>% 
                  #   st_transform(.,crs = 32721)
                  aux = subset(segmCallesCentrid,COD_NOMBRE == codCalle)
                  
                    callAnt = useCalle$COD_NOMBRE[xx-1]
                    callPos = useCalle$COD_NOMBRE[xx+1]
                    
                    posIni = which(aux$COD_ESQ == callAnt)
                    posFin = which(aux$COD_ESQ == callPos)
                  
                  if(length(posIni) == 0) posIni = 1 
                  if(length(posFin) == 0) posFin = nrow(aux)   
                  
                  # aux = aux[c(posIni:posFin),]  
                  # aux = subset(segmCallesPuntos,COD_NOMBRE == codCalle)
                  
                  # linAux = subset(linaVar,COD_CALLE1 == codCalle)
                  
                  distnn <- df %>% nngeo::st_nn(aux, maxdist= 30,k=1,sparse = T,returnDist = T) %>%
                    suppressMessages()
                   
                  distnn = do.call(c,lapply(distnn[[2]],function(xx) {
                    if(is_empty(xx)) {
                      return(NA)
                    } else {
                      return(xx)
                  }
                    })
                  )
                  
                  matchAux <- df %>% 
                    st_join(aux, join=nngeo::st_nn, maxdist= 100,k=1) %>% 
                    cbind(.,distnn) %>%
                    filter(!is.na(COD_NOMBRE)) %>%
                    suppressMessages()
                  
                  ###########
                  ## Descarta segmentos que no tiene al menos dos puntos
                  # segKeep = matchAux %>% group_by(ID_segm) %>%
                  #   summarise(n = n()) %>% ungroup() %>%
                  #   filter(n > 1)
                  # 
                  # matchAux = matchAux %>% filter(ID_segm %in% segKeep$ID_segm)
                  if(nrow(matchAux) > 0) {
                    matchAux$NumCalle = NumCalle
                  } else {
                    matchAux$NumCalle = character()  
                    }
                  
                  # }
                  list(matchAux)
                })
)

match = match %>% group_by(ID_lin) %>% 
  slice_min(distnn) %>%
  arrange(ID_lin)




# Mapa para ver como queda la linea representada con segmentos ------------


# AV = nomCalles[grep('av |BV |RBLA ',nomCalles$NOM_CALLE,ignore.case = T),]

segLinea = segmCalles %>% 
  inner_join(match %>% st_set_geometry(NULL) %>% dplyr::select(ID_segm,ID_lin)) %>%
  group_by(ID_calle,COD_NOMBRE,ID_segm) %>% slice_head() %>% ungroup() %>%
  arrange(ID_lin) %>%
  mutate(content = paste(sep = "<br/>",
                         paste("<b><a >Calle</a></b>:",NOM_CALLE),
                         paste("<b><a >X</a></b>:",round(X)),
                         paste("<b><a >Xf</a></b>:",round(Xf)),
                         paste("<b><a >Y</a></b>:",round(Y)),
                         paste("<b><a >Yf</a></b>:",round(Yf)),
                         paste("<b><a >ID segm</a></b>:",ID_segm),
                         paste("<b><a >ID Calle</a></b>:",ID_calle),
                         paste("<b><a >ID_lin</a></b>:",ID_lin)))


segLineaRep = segLinea %>% 
  group_by(X,Xf,Y,Yf) %>% slice_head() %>% ungroup() %>%
  select(ID_calle,COD_NOMBRE,COD_ESQ,X,Xf,Y,Yf,ID_segm,ID_lin) %>% 
  mutate(Xs_lead = lead(X),
         Xs_lag = lag(X)) %>%
  rowwise() %>%
  rowwise() %>%
  mutate(inter = sum(c(segLinea$X,segLinea$Xf) %in% X) - 1,
         interF = sum(c(segLinea$X,segLinea$Xf) %in% Xf) - 1,
         cambSent_lead = as.numeric(X == Xs_lead),
         cambSent_lag = as.numeric(X == Xs_lag)) %>%
  ungroup() %>% arrange(ID_lin) %>%
  st_set_geometry(NULL)

# View(segLineaRep)

segLineaRep0 = segLinea %>%
  group_by(X,Xf,Y,Yf) %>% slice_head() %>% ungroup() %>%
  select(ID_calle,COD_NOMBRE,COD_ESQ,COD_UBIC_P,X,Xf,Y,Yf,ID_segm,ID_lin,content) %>%
  mutate(Xs_lead = lead(X),
         Xs_lag = lag(X)) %>%
  rowwise() %>%
  mutate(inter = sum(c(segLinea$X,segLinea$Xf) %in% X) - 1,
         interF = sum(c(segLinea$X,segLinea$Xf) %in% Xf) - 1,
         cambSent_lead = as.numeric(X == Xs_lead),
         cambSent_lag = as.numeric(X == Xs_lag)) %>%
  ungroup() %>%
  # filter((inter > 0 & interF > 0) | ID_lin %in% c(min(segLinea$ID_lin),max(segLinea$ID_lin)))
  filter(!((inter >= 2 & interF == 0) | (interF >= 2 & inter == 0)) | ID_lin %in% c(min(segLinea$ID_lin),max(segLinea$ID_lin)))
# filter((inter > 0 & interF > 0) | cambSent_lead == 1) # | cambSent_lag == 1



segLineaRep00 = segLinea %>% 
  group_by(X,Xf,Y,Yf) %>% slice_head() %>% ungroup() %>%
  select(ID_calle,COD_NOMBRE,COD_ESQ,COD_UBIC_P,X,Xf,Y,Yf,ID_segm,ID_lin,content) %>% 
  group_by(ID_lin) %>% slice_head() %>% ungroup() %>%
  rowwise() %>%
  mutate(inter = sum(c(segLinea$X,segLinea$Xf) %in% X) - 1,
         interF = sum(c(segLinea$X,segLinea$Xf) %in% Xf) - 1) %>%
  ungroup() %>%
  # filter((inter > 0 & interF > 0) | ID_lin %in% c(min(segLinea$ID_lin),max(segLinea$ID_lin)))
  filter(!((inter >= 2 & interF == 0) | (interF >= 2 & inter == 0)) | ID_lin %in% c(min(segLinea$ID_lin),max(segLinea$ID_lin))) %>%
  arrange(ID_lin) %>%
  mutate(Xs_lag = lag(X),
         Xsf_lag = lag(Xf),
         Xs_lead = lead(X),
         Xsf_lead = lead(Xf),
         Ys_lag = lag(Y),
         Ysf_lag = lag(Yf),
         Ys_lead = lead(Y),
         Ysf_lead = lead(Yf)) %>%
  rowwise() %>%
  mutate(IntNext = as.numeric(X %in% c(Xs_lead,Xsf_lead)),
         IntPrev = as.numeric(X %in% c(Xs_lag,Xsf_lag)),
         IntNextf = as.numeric(Xf %in% c(Xs_lead,Xsf_lead)),
         IntPrevf = as.numeric(Xf %in% c(Xs_lag,Xsf_lag))) %>%
  ungroup()

#### DETECTA REPETIDOS



# segLineaRep00$indRep = apply(segLineaRep00[,c('X','Xf','Y','Yf')],1, function(yy) {
#   sum(apply(segLineaRep00[,c('X','Xf','Y','Yf')],1, function(hh) {
#     sum(yy %in% hh)}) == 4)
#     })

  

##### Une puntos desconexos

# puntDesc = segLineaRep00 %>% filter(IntNext == 0 & IntPrev == 0 & !is.na(Xs_lead) & !is.na(Xs_lag))
puntDesc = segLineaRep00 %>% filter(((IntNext == 0 & IntNextf == 0) | (IntPrev == 0 & IntPrevf == 0)) & !is.na(Xs_lead) & !is.na(Xs_lag))

segmAdd = list()

for(xx in 1:nrow(puntDesc)) {
  fila = puntDesc[xx,] %>% st_set_geometry(NULL)
  Xis = colnames(fila)[grep('^X',colnames(fila))]
  Yis = colnames(fila)[grep('^Y',colnames(fila))]
  
  fila_df = fila %>% data.frame()
  puntos = data.frame(X = do.call(c,as.vector(fila_df[1,Xis])),
                      Y = do.call(c,as.vector(fila_df[1,Yis])))
  rownames(puntos) = gsub('X','',rownames(puntos))
  
  
  # dd = apply(puntos,1,function(xx) {
  #   sqrt((xx[1] - puntos$X)^2 + (xx[2] - puntos$Y)^2)
  # })
  # 
  # dd[dd == 0] = Inf
  # dd[upper.tri(dd)] = Inf
  # 
  # pos = which(dd == min(dd))/nrow(dd)
  # posFila = floor(pos) + 1
  # posCol = which(dd == min(dd)) %% nrow(dd)
  
  # posCol = floor(pos)
  
  # posFilaSel = posFila[posFila <= 2][1]
  # posColSel = posCol[posFila <= 2][1]
  
  # posFilaSel = posFila[1]
  # posColSel = posCol[1]
  # 
  # if(posColSel == 0) posColSel = pos[1]
  
  
  # Xini = fila[,Xis[posFilaSel]]
  # Xfin = fila[,Xis[posColSel]]
  # Yini = fila[,Yis[posFilaSel]]
  # Yfin = fila[,Yis[posColSel]]
  
  Xini = fila$X
  Xfin = fila$Xs_lead
  Yini = fila$Y
  Yfin = fila$Ys_lead
  
  
  indRep = max(apply(segLineaRep00[,c('X','Xf','Y','Yf')],1, function(yy) {
    sum(c(Xini,Xfin,Yini,Yfin) %in% yy)
  })) < 4
  ##### Verifico que el segmento no exista
  
  if(indRep) {
    fila = fila %>% mutate(geometry = paste('LINESTRING (',Xini,' ',Yini,',',Xfin,' ',Yfin,')'),
                        X = Xini,
                        Xf = Xfin,
                        Y = Yini,
                        Yf = Yfin,
                         ID_lin = ID_lin + 1,
                         ID_segm = paste0(ID_segm,'_add')) %>%
    st_as_sf(.,wkt = c('geometry'),crs = 32721) %>%
  st_transform(x = .,crs = 4326)
  
  # mutate(geometry = paste('LINESTRING (',X,' ',Y,',',Xf,' ',Yf,')')) %>%
  #   ungroup() %>%
  #   st_as_sf(.,wkt = c('geometry'),crs = 32721) %>%
  #   st_transform(x = .,crs = 4326)
  

  segmAdd[[length(segmAdd) + 1]] = fila
  }
  
}

segmADD = do.call(rbind,segmAdd)

segLineaRep000 = rbind(segLineaRep00,segmADD) %>% arrange(ID_lin) %>%
  mutate(content = paste(sep = "<br/>",
                         paste("<b><a >ID segm</a></b>:",ID_segm),
                         paste("<b><a >ID Calle</a></b>:",ID_calle),
                         paste("<b><a >ID_lin</a></b>:",ID_lin)))
  
###### Saca segmentos repetidos

segRep = segLineaRep000 %>% select(X,Xf,Y,Yf,ID_segm) %>% st_set_geometry(NULL) %>%
  mutate(posID = row_number()) %>%
  rbind(.,segLineaRep000 %>% select(X,Xf,Y,Yf,ID_segm) %>%
          mutate(X0 = X,
                 Xf0 = Xf,
                 Y0 = Y,
                 Yf0 = Yf,
                 X = Xf0,
                 Xf = X0,
                 Y = Yf0,
                 Yf = Y0,
                 posID = row_number()) %>%  
          st_set_geometry(NULL) %>% 
          select(-c(X0,Y0,Xf0,Yf0))) %>%
  group_by(X,Xf,Y,Yf) %>% slice_head() %>% ungroup() %>%
  arrange(ID_segm) %>%
  group_by(posID) %>% summarise(n = n())


# aa = segRep %>% filter(ID_segm %in% c('2949_39','597_140')) %>% arrange(X)
# cc = segRep %>% filter(ID_segm %in% c('2949_39','597_140')) %>% arrange(X)
# bb = segRep %>% filter(posID %in% c(32:34))

posUnique = segRep$posID[segRep$n == 2]
table(segRep$n)
# nomCalles[grep('rivera',nomCalles$NOM_CALLE,ignore.case = T),]

# codCalle = 6051

pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = segLinea,
               color = 'red',
               weight = 4,popup = ~content,group = 'Seg inicial') %>%
  addPolylines(data = segLineaRep0,
               color = 'green',
               weight = 4,popup = ~content,group = 'Seg dep') %>%  
  addPolylines(data = segmADD,
               color = 'orange',
               weight = 4,popup = ~content,group = 'Seg ADD') %>%
  addPolylines(data = segLineaRep000[posUnique,],#[posUnique,]
               color = 'blue',
               weight = 4,popup = ~content,group = 'Seg dep + ADD') %>%
  addPolylines(data = subset(lineasNew_sf,cod_variante == hh),
               color = 'pink',
               weight = 1,group = 'Linea') %>%
  addLayersControl(
    baseGroups = c("CartoDB.Positron","OSM"), 
    overlayGroups = c('Seg inicial','Seg dep','Seg ADD','Seg dep + ADD','Linea'),
    options = layersControlOptions(collapsed = T))

# %>%
#   addCircles(data = ccalle)
pp


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addCircles(data = df %>% st_transform(.,crs = 4326),
               color = 'red',
               weight = 1) %>%
  addCircles(data = aux %>% st_transform(.,crs = 4326),
             color = 'blue',
             weight = 1)
pp

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


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = segmCalles,
               color = 'red',
               weight = 4) %>%
  addPolylines(data = lineasNew_sf %>% filter(cod_variante %in% varMaximal),
               color = 'blue',
               weight = 2,
               popup = ~cod_variante) %>%
  addCircles(data = paradasUni %>% st_transform(x = .,crs = 4326),
             color = 'orange',
             weight = 1)
pp
