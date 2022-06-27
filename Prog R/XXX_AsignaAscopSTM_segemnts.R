
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

library(fst)


######################################################
###### FUnciones
fun = dir('Funciones/')
fun = fun[grep('\\.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))

###################################
##### FUncion Arregla caracteres
arreglaCaracteres = function(x,tolower = T,sacPunto = T) {
  x = gsub('Ã\u0091','N',x)
  x = gsub('ã\u0081','A',x)
  x = gsub('Ã\u0081','A',x)
  
  x = gsub('ã¼','u',x)
  x = gsub('Ã¼','u',x)
  
  x = gsub('Ã‘','N',x)
  
  
  if(sacPunto) x = gsub('\\.','',x)
  if(tolower) x = tolower(x)
  
  return(x)
}


###########################################################
######## CARGA INSUMOS

load('Shiny/Insumos/XXX_segmentUnic.RData')

load('Shiny/Insumos/XXX_puntSTMUnic.RData')

load('Shiny/Insumos/XXX_DatosAScopShiny.RData')


datJamSegm_df = read.fst('Shiny/Insumos/datJamSegm_df.fst')



###### Base con segmentos ID calle y direction
idSegmCalle = datJamSegm_df %>% group_by(ID_segmento) %>%
  slice_head() %>% ungroup() %>%
  select(ID_segmento,COD_NOMBRE,direction) %>%
  filter(!is.na(COD_NOMBRE))


#### La pasa a objeto espacial
idSegmCalle_sf = idSegmCalle %>% left_join(segmentUnic %>% select(-ID_coord)) %>%
  st_as_sf(x = .,sf_column_name = 'geometry') %>%
  st_centroid(.)
############################################
###### Asigna cada parada STM a un segmento

### Convierto el objeto en sf y cambio las coordenadas a metros ,crs = 32721
### y le asigo el segmento máscercano a la parada
paradaSegm = puntSTMUnic %>% st_as_sf(x = .,coords = c('long','lat'),crs = 4326) %>%
  st_transform(.,crs = 32721) %>%   
  st_join(segmentUnic,join=nngeo::st_nn, maxdist= 100,k=1)


##### Le pega el ID calle y calcula el centroide para cada segmento con parada
paradaSegm_centroid = paradaSegm %>% left_join(idSegmCalle) %>% st_set_geometry(NULL) %>%
  left_join(segmentUnic %>% select(-ID_coord)) %>% st_as_sf(x = .,sf_column_name = 'geometry') %>%
  st_centroid(.)


##### Identifica calles y sentidos que tienen paradas con segmentos
idCalles = aaaux %>% st_set_geometry(NULL) %>% 
  group_by(COD_NOMBRE,direction) %>%
  slice_head() %>%
  filter(!is.na(COD_NOMBRE)) %>%
  ungroup() %>% select(-ID_coord)

idCalles = idCalles[!is.na(idCalles$COD_NOMBRE),]

###### A cada calle direccion encuentra los segmentos que están a menos de xx metros para asociarle el dato de la parada

trials = 1:nrow(idCalles)

funPar = function(jj) {
  # ref = idCalles$COD_NOMBRE[jj]
  ref = idCalles[jj,c('COD_NOMBRE','direction')]
  
  subCParCalle = aaaux %>% inner_join(ref, by = c('COD_NOMBRE','direction'))

  subCSegmCalle = idSegmCalle_sf %>% inner_join(ref, by = c('COD_NOMBRE','direction'))
  
  res = subCSegmCalle %>%  st_join(subCParCalle %>% select(ID_parada),join=nngeo::st_nn, maxdist= 350,k=1) %>%
    suppressWarnings() %>% suppressMessages()


  return(res)
}



#### Detecta cores
cl <- makeCluster(detectCores())
#### Carga librerias e insumos
clusterExport(cl, c("idSegmCalle_sf","idCalles","funPar","aaaux"))
clusterEvalQ(cl, {
  library(tidyverse)
  library(sf)})

### Corre en paralelo
system.time({
  results <- parallel::parLapply(cl,trials,funPar)
})

### Cierra los clusters
stopCluster(cl)

names(results) = as.character(codCalles)

sum(do.call(c,lapply(results,function(xx) nrow(xx) == 0)))
# 
# 
# ### Detecta vacias
# vacias = do.call(c,lapply(results,is_empty))
# ### Se queda con las no vacias
# res = results[!vacias]


# ### Genera el data frame con todos los segmentos que tienen asociado alguna parada
segmParadasAsigna = as.data.frame(data.table::rbindlist(results))

save(segmParadasAsigna,file = 'Prog R/Descriptivos/Shiny/Insumos/XXX_segmParadasAsigna.RData')
###




