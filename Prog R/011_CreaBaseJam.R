
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
# datos = read.table('Datos/etwazetrafficjam_202112061701.csv',sep = ',',header = T)
datos = read.table('Datos/etwazetrafficjam_202112071333.csv',sep = ',',header = T)

#############################################
#### Me quedo solo con los atascos
datJam = subset(datos,delay >= 0)

######################################################
#### Nomenclator IM
vias = st_read('SHP/v_mdg_vias')

#############################################
#### Paso las fecas de character a dates

datJam = datJam %>% mutate(datecreated = as.POSIXct(strptime(datecreated, "%Y-%m-%d %H:%M:%S")),
                           datemodified = as.POSIXct(strptime(datemodified, "%Y-%m-%d %H:%M:%S")),
                           datepublished = as.POSIXct(strptime(datepublished, "%Y-%m-%d %H:%M:%S"))) %>%
  filter(datepublished >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))

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



########################################################
########################################################
##############################################
######## Agrega codigos de calle, de calle de inicio y calle fin




######################################################
#### Calles a buscar
calleBusDF = data.frame(calleOrig = c(datJam$street,datJam$endnode)) %>% group_by(calleOrig) %>%
  summarise(cont = n()) %>% ungroup() %>% filter(calleOrig != '') %>%
  mutate(callesBuscar = arreglaCaracteres(calleOrig),
         NOM_CALLE = NA,
         COD_NOMBRE = NA) %>% data.frame()

rownames(calleBusDF) = calleBusDF[,'calleOrig']


###################################################
###### NOMENCLATOR IM

nomCalles = vias %>% st_set_geometry(NULL) %>% dplyr::select(NOM_CALLE,COD_NOMBRE) %>%
  group_by(COD_NOMBRE) %>% slice_head() %>% ungroup() %>%
  mutate(NOM_CALLE = arreglaCaracteres(NOM_CALLE))


###########################################
##### Encuentra las que estan igual

for(ii in rownames(calleBusDF)) {
  buscar = calleBusDF[ii,'callesBuscar']
  expr = paste0('^',buscar,'$')
  aux = nomCalles[grep(expr,nomCalles$NOM_CALLE,ignore.case = T),]
  
  if(nrow(aux) > 0) {
    calleBusDF[ii,c('NOM_CALLE','COD_NOMBRE')] = aux[1,c('NOM_CALLE','COD_NOMBRE')]
  }
}

#######################################
###### Agrego codigos a datJam

datJam[,'NOM_CALLE'] =  calleBusDF[as.character(datJam$street),'NOM_CALLE']
datJam[,'COD_NOMBRE'] =  calleBusDF[as.character(datJam$street),'COD_NOMBRE']

############################################
###### INTENTO UN JOIN ESPACIAL

vias_bf = st_buffer(vias,10)



datVacios = subset(datJam,street == '' & is.na(COD_NOMBRE))
datNoVacios = subset(datJam,street != '' & is.na(COD_NOMBRE)) %>% 
  group_by(street) %>% slice_head()

datMerge = datNoVacios %>% rbind(datVacios) %>% select(-COD_NOMBRE,-NOM_CALLE)

datMerge_bf = st_buffer(datMerge,1)

datMerge_bf_cod = datMerge_bf %>% st_join(vias_bf[,c('NOM_CALLE','COD_NOMBRE')],
                                          join = st_intersects,largest = T)


resMerge_bf_cod = datMerge_bf_cod %>% st_set_geometry(NULL) %>% 
  dplyr::select(ID_Base,street,NOM_CALLE,COD_NOMBRE) %>% 
  mutate(Vacia = ifelse(street == '',1,0),
         street = ifelse(Vacia == 1,as.character(ID_Base),street)) %>%
  dplyr::select(-ID_Base) %>%
  rename('calleOrig' = 'street') %>% data.frame()


rownames(resMerge_bf_cod) = as.character(resMerge_bf_cod$calleOrig)
calleBusDF$Vacia = 0
codCallesAll = rbind(calleBusDF[!is.na(calleBusDF$NOM_CALLE),colnames(resMerge_bf_cod)],
                     resMerge_bf_cod)

rownames(codCallesAll) = as.character(codCallesAll$calleOrig)


datJam$ID_CODCALLE = ifelse(datJam$street == '',as.character(datJam$ID_Base),datJam$street)


datJam[,'NOM_CALLE'] =  codCallesAll[as.character(datJam$ID_CODCALLE),'NOM_CALLE']
datJam[,'COD_NOMBRE'] =  codCallesAll[as.character(datJam$ID_CODCALLE),'COD_NOMBRE']

datJam[,'NOM_CALLE_END'] = codCallesAll[as.character(datJam$endnode),'NOM_CALLE'] 
datJam[,'COD_NOMBRE_END'] = codCallesAll[as.character(datJam$endnode),'COD_NOMBRE'] 

############################################
##### Encuentro la calle de inicio

puntosIni = datJam %>% dplyr::select(ID_Base,COD_NOMBRE) %>% 
  st_cast(.,'POINT') %>%
  group_by(ID_Base) %>%
  slice_head()

puntosFin = datJam %>% dplyr::select(ID_Base,COD_NOMBRE) %>% 
  st_cast(.,'POINT') %>%
  group_by(ID_Base) %>%
  slice_tail()


#################################################
####### GUARDO LA BASE CON LOS CODIGOS

load('Resultados/001_segmProy.RData')

segmProyPoint = segmProy %>% 
  st_as_sf(.,wkt = 'geometry',crs = 32721) %>% 
  st_cast(.,'POINT')




codCalles = unique(puntosIni$COD_NOMBRE)

direcCalles = unique(segmProy[,c("NOM_CALLE","COD_NOMBRE","dirIni","dirFin")])


results = sapply(codCalles, function(xx) {
  
  auxIni = subset(puntosIni,COD_NOMBRE == xx)
  auxFin = subset(puntosFin,COD_NOMBRE == xx) %>% dplyr::select(-COD_NOMBRE)
  auxAdd = subset(segmProyPoint,COD_NOMBRE == xx)
  
  if(nrow(auxAdd) > 0) {
    
    resAuxIni = auxIni %>% st_join(auxAdd[,c('ID_calle','NOM_CALLE_ESQ','COD_NOMBRE_ESQ')], 
                                   join=nngeo::st_nn, maxdist= Inf,k=1) %>%
      
      st_set_geometry(NULL) %>%
      rename('ID_calle_Ini' = 'ID_calle') %>% data.frame(.) %>% suppressMessages()
    
    resAuxFin = auxFin %>% st_join(auxAdd[,c('ID_calle','NOM_CALLE_ESQ_f','COD_NOMBRE_ESQ_f')], 
                                   join=nngeo::st_nn, maxdist= Inf,k=1) %>%
      
      st_set_geometry(NULL) %>%
      rename('ID_calle_Fin' = 'ID_calle') %>% data.frame(.) %>% suppressMessages()
    
    
    resAux = resAuxIni %>% left_join(resAuxFin, by = 'ID_Base')
    
    
  } else {
    resAux = list()  
  }
  return(resAux)
})


### Detecta vacias
vacias = do.call(c,lapply(results,is_empty))
### Se queda con las no vacias
res = results[!vacias]

puntosAll = as.data.frame(data.table::rbindlist(res)) 

#### Agrego las direcciones
puntosAll = puntosAll %>% left_join(direcCalles[,c('COD_NOMBRE','dirIni','dirFin')],
                                    by = 'COD_NOMBRE')

#### Pego las direcciones y la info del inicio y fin a datJam

datJamDir = datJam %>% left_join(puntosAll %>% dplyr::select(-COD_NOMBRE), by = 'ID_Base') %>%
  mutate(IndShift = ifelse(ID_calle_Ini <= ID_calle_Fin,0,1),
         direction = ifelse(IndShift == 1,paste(dirFin,dirIni,sep = '_'),
                            paste(dirIni,dirFin,sep = '_'))) 
# %>%
#   filter(!is.na(ID_calle_Ini))


head(datJamDir[datJamDir$IndShift == 1,])


aux = st_geometry(datJamDir)

# names(table(datJamDir$direction,exclude = NULL))


# calles no modificar
allCalles = unique(datJamDir$NOM_CALLE)
allCalles[grep('argentinos',allCalles,ignore.case = F)]

cNoModif = c("av de las leyes","bv gral artigas")


cond = datJamDir$direction == "ESTE_OESTE" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
aux[cond] = aux[cond] + c(0,10)

cond = datJamDir$direction == "NORTE_SUR" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
aux[cond] =   aux[cond] + c(10,0)

cond = datJamDir$direction == "OESTE_ESTE" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
aux[cond] = aux[cond] + c(0,-10)

cond = datJamDir$direction == "SUR_NORTE" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
aux[cond] = aux[cond] + c(-10,0)


st_geometry(datJamDir) = aux



########################################
####### GUarda las bases con y sin direccion
save(datJam,file = 'BasesR/011_datJam.RData')
save(datJamDir,file = 'Resultados/011_datJamDir.RData')




