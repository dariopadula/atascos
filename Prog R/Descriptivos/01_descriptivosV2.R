
#########################################################
####### INSTALA LIBRERIAS QUE SE NECESITAN

libNecesarias = c('shiny','tidyverse','ggplot2','dplyr','ggrepel','shinydashboard','DT',
                  'formattable','gtools','MASS','sf','nngeo',
                  'mgcv','ggpubr','gridExtra','leaflet.extras','viridis','ggExtra','plotly',
                  'fst')



pkgInstall = installed.packages()[,'Package']

pkgAux = libNecesarias[!libNecesarias %in% pkgInstall]

if(length(pkgAux) > 0) install.packages(pkgAux)

########################################################
##### LIBRERIAS
for(ii in libNecesarias) eval(bquote(library(.(ii))))

###################################################
### FUNCIONES
fun = dir('Funciones/')
fun = fun[grep('.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))


##################################################
#################################################
# Base con los datos de Jam
# load('Shiny/Insumos/XXX_DatosShiny.RData')
datJamSegm_df = read.fst("Shiny/Insumos/datJamSegm_df.fst")
datJam_df = read.fst("Shiny/Insumos/datJam_df.fst")

load('Shiny/Insumos/XXX_segmentUnic.RData')

# Tabla con segmentos y paradas asociadas
load('Prog R/Descriptivos/Shiny/Insumos/XXX_segmParadasAsigna.RData')

# Base con los datos de Cortes
# load('Shiny/Insumos/XXX_DatosShinyCortes.RData')
# Base con los datos de AutoScop
# load('Shiny/Insumos/XXX_DatosAScopShiny.RData')
# Tabla de dias, anio, meses para filtrar los días
load('Shiny/Insumos/XXX_dsma.RData')
# Lista con shapes de servicios a incluir
load('Resultados/002_listSHPServicios.RData')
# Base para hacer el resumen
# load('Shiny/Insumos/XXX_datJam_resum.RData')
# datJam_resum = read.fst("Shiny/Insumos/datJam_resum.fst")
#################################################
#################################################

vv = st_read('SHP/v_mdg_vias') %>% st_set_geometry(NULL)


vv %>% filter(COD_NOMBRE == 2940) %>% slice_head()

vv %>% filter(COD_NOMBRE == 3258) %>% slice_head()


vv %>% filter(COD_NOMBRE == 2922) %>% slice_head()



################################
#### Por segmentos





# segmUsar = datJamSegm_df %>% group_by(ID_segmento) %>% summarise(n = n()) %>% filter(n > 200)


# segmUsar = datJamSegm_df %>% group_by(ID_segmento,hora) %>% summarise(n = n())
# 
# segmUsar %>% filter(ID_segmento == 26596 & hora == 17)
####################################################
####################################################
####################################################
##### REARMO TODO

datJamSegm_df = datJamSegm_df %>% mutate(COD_NOMBRE_END_F = ifelse(is.na(COD_NOMBRE_END),COD_NOMBRE_ESQ_f,COD_NOMBRE_END)) %>%
  dplyr::select(-diaSem) %>% left_join(dsma %>% dplyr::select(diaStr,diaSem))


####################################
###### Homogeinizo las calles en funsion del tramo que mas se repite para cada segmento

zonaCalles = datJamSegm_df %>% 
  group_by(ID_segmento,COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F) %>% 
  summarise(TotMax = n(),
            direction = first(direction)) %>% 
  ungroup() %>%
  filter(!is.na(COD_NOMBRE_ESQ)) %>%
  group_by(ID_segmento)  %>%
  mutate(TotalVeces = sum(TotMax)) %>%
  slice_max(TotMax) %>%
  slice_head() %>%
ungroup() %>%
  rename(c('COD_NOMBRE_REF' = 'COD_NOMBRE','ESQ_REF' = 'COD_NOMBRE_ESQ','ESQ_END_REF' = 'COD_NOMBRE_END_F'))

datJamSegm_df = datJamSegm_df %>% dplyr::select(-direction) %>% left_join(zonaCalles)

pruebaS = datJamSegm_df %>% #filter(ID_segmento %in% segmUsar$ID_segmento) %>%
  group_by(ID_segmento,COD_NOMBRE_REF,ESQ_REF,ESQ_END_REF,diaStr,diaSem,finDeSem,hora,direction) %>% 
  mutate(N3 = as.numeric(level == 3),
         N4 = as.numeric(level == 4)) %>% 
  summarise(n = n(),
            N3 = sum(N3),
            N4 = sum(N4),
            N34 = N3 + N4,
            TotalVeces = first(TotalVeces),
            lengthM = mean(length,na.rm = T)) %>%
  ungroup() %>% filter(!is.na(COD_NOMBRE_REF)) %>%
  arrange(desc(N34)) %>% 
  mutate(porcDH = round(100*n/31),
         porcDH_N34 = round(100*N34/31)) 




pruebaAggS= pruebaS %>% 
  group_by(ID_segmento,COD_NOMBRE_REF,ESQ_REF,ESQ_END_REF,diaSem,finDeSem,hora,direction) %>%
  summarise(
    nEventos = sum(n),
    nEventosAll = first(TotalVeces),
    nEventosMedio = mean(n),
    nDias = n(),
    N4medio = mean(N4),
    N34medio = mean(N34),
    N4Sum = sum(N4),
    N34Sum = sum(N34),
    lengthM = mean(lengthM,na.rm = T)) %>%
  ungroup() %>%
  left_join(dsma %>% group_by(diaSem) %>% summarise(TotalDias = n()))  %>%
  mutate(nEventosMedio_All = nEventos/TotalDias,
         N4medio_All = round(N4Sum/TotalDias,2),
         N34medio_All = round(N34Sum/TotalDias,2),
         difN34 = N34medio - N34medio_All)  %>%
  arrange(desc(N34medio_All))





#####################################################
#####################################################
### Segmentacion

datKm = pruebaAggS %>% #select(-c(COD_NOMBRE_REF:ESQ_END_REF,finDeSem)) %>% 
  filter((nEventosAll > 800 & N34Sum > 5) | N34medio > 10) %>%
  mutate(porcDias = round(100*nDias/TotalDias,2))

hhS = datKm

k = 15

# c("nEventos","nEventosAll","porcDias","N4medio","N34medio","N4medio_All","N34medio_All")
varsKM = c("porcDias","nEventosMedio","N4medio","N34medio","nEventosMedio_All","N4medio_All","N34medio_All")

media = datKm %>% group_by() %>% 
  summarise(across(all_of(varsKM),mean)) %>%
  uncount(k)

SD = datKm %>% group_by() %>% 
  summarise(across(all_of(varsKM),sd)) %>%
  uncount(k)
  




varStand = function(x) {
  y = (x - mean(x))/sd(x)
  return(y)
}

datKm = datKm %>% mutate(across(all_of(varsKM),varStand))


set.seed(1234)
fitkm = kmeans(datKm[,varsKM], centers = k, iter.max = 30, nstart = 10)

cc = round((fitkm$centers*SD + media))

table(fitkm$cluster)

cc[,'n'] = table(fitkm$cluster)
cc[,'cluster'] = 1:k

clusterInfo = cc %>% arrange(desc(N34medio_All)) %>%
  mutate(ordenSev = row_number())


clusterInfo$period = cut(clusterInfo$porcDias,breaks = c(-Inf,20,40,60,Inf),labels = c('Poco frecuente','Algo frecuente','Frecuente','Muy frecuente'))
clusterInfo$severidad = ifelse(clusterInfo$N4medio > 20 | clusterInfo$N34medio_All >= 10, 'Severo',
                               ifelse(clusterInfo$N34medio_All < 10 & clusterInfo$N34medio_All >= 7, 'Poco Severo','Nada severo'))


hhS = hhS %>% mutate(cluster = fitkm$cluster,
                     diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo'))) %>%
  left_join(clusterInfo %>% dplyr::select(cluster,ordenSev))
  


# hhS = hhS  %>% mutate(clusters = fitkm$cluster) %>% filter(clusters == 15 & hora %in% c(17:19) & diaSem == 'viernes')

pruebaSS = pruebaS %>% 
  mutate(diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo'))) %>% 
  left_join(hhS %>% dplyr::select(ID_segmento,diaSem,hora,cluster,ordenSev), by = c('ID_segmento','diaSem','hora')) %>%
  left_join(segmParadasAsigna %>% select(ID_segmento,ID_parada))

save(pruebaSS,pruebaAggS,hhS,clusterInfo,file = "Prog R/Descriptivos/Shiny/Insumos/XXX_DatosShiny.RData")




resumClust0 = hhS %>% filter(cluster %in% c(1,8))

table(resumClust0$hora,resumClust0$diaSem)

resumClust = hhS %>% filter(cluster %in% c(1)) %>% 
  group_by(ID_segmento,finDeSem,hora) %>%
  summarise(n = n()) %>%
  ungroup()


table(resumClust$hora,resumClust$n,resumClust$finDeSem)


# test = hhS %>% filter(cluster %in% c(3) & ID_segmento == 24667 & hora == 19)





subSegm = hhS %>% 
  group_by(ID_segmento) %>%
  summarize(n = n(),
            N34medio_All = mean(N34medio_All),
            N4medio_All = mean(N4medio_All)) %>%
  ungroup()
  

# segmSelect = segmentUnic %>% filter(ID_segmento %in% subSegm$ID_segmento) %>% st_transform(x = .,crs = 4326)
segmSelect = segmentUnic %>% inner_join(subSegm) %>% st_transform(x = .,crs = 4326)

######################################################
######################################################



pruebaAggS0 = pruebaAggS %>%
    left_join(dsma %>% group_by(diaSem) %>% summarise(TotalDias = n())) %>%
    mutate(porcDias = round(100*nDias/TotalDias))


ss = pruebaAggS %>% filter(ID_segmento == 32883 & hora == 17 & finDeSem == 'Lunes a viernes') %>%
  arrange(desc(N34Sum))


# 32883

pruebaAggS2 = pruebaAggS %>%
  group_by(COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaSem,finDeSem,hora,direction) %>%
  mutate(Total = sum(nEventos)) %>% # Total de eventos generados por distintos segemntos entre las calles
  arrange(desc(nEventos)) %>%
  slice_head() %>% # me quedo con el segmento que genero la mayor cantidad de eventos
  ungroup() %>% 
  group_by(ID_segmento,diaSem,hora,direction) %>%
  arrange(desc(Total)) %>%
  slice_head() %>% # Me quedo con las secuencia de calles que contienen al segmento que genero la mayor cantida de eventos
  ungroup() %>%
  left_join(dsma %>% group_by(diaSem) %>% summarise(TotalDias = n())) %>%
  mutate(porcDias = round(100*nDias/TotalDias)) %>%
  arrange(desc(porcDias),desc(N34medio))



pruebaAggS2_trim = pruebaAggS2 %>% filter(porcDias > 20 & N34medio > 5)

hhS = pruebaAggS2 %>% filter(porcDias > 50 & nEventos > 100 & N34medio > 5)


segmSelect = segmentUnic %>% filter(ID_segmento %in% hhS$ID_segmento) %>% st_transform(x = .,crs = 4326)


pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
  addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
  addPolylines(data = segmSelect,
               color = 'orange',
               weight = 4,
               layerId = ~ID_segmento) 




pruebaAggS3 = pruebaAggS2 %>%
  group_by(ID_segmento,finDeSem) %>%
  mutate(nFilas = n(),
         nDiasSum = sum(nDias),
         nDiasMax = max(nDias),
         nEventosSum = sum(nEventos),
         nEventosMax = max(nEventos)) %>%
  arrange(desc(N34medio)) %>%
  slice_head() %>%
  ungroup() %>%
  arrange(desc(round(N34medio)),desc(nDiasMax))
  
  
