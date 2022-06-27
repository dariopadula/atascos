
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
datJam_resum = read.fst("Shiny/Insumos/datJam_resum.fst")
#################################################
#################################################

vv = st_read('SHP/v_mdg_vias') %>% st_set_geometry(NULL)


vv %>% filter(COD_NOMBRE == 1917) %>% slice_head()

vv %>% filter(COD_NOMBRE == 7050) %>% slice_head()


vv %>% filter(COD_NOMBRE == 4029) %>% slice_head()


aa = datJam_df %>% mutate(COD_NOMBRE_END_F = ifelse(is.na(COD_NOMBRE_END),COD_NOMBRE_ESQ_f,COD_NOMBRE_END)) %>%
  group_by(COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F) %>% 
  summarise(n = n(),
            lengthM = mean(length,na.rm = T),
            lengthSD = sd(length,na.rm = T)) %>%
  ungroup() %>%
  mutate(CV = lengthSD/lengthM) %>%
  arrange(desc(n)) 




bb = aa %>% group_by(COD_NOMBRE,COD_NOMBRE_END_F) %>%
  mutate(COD_NOMBRE_ESQ = COD_NOMBRE_ESQ,
         Total = sum(n)) %>%  
  slice_max(n) %>%
  ungroup() %>% 
  arrange(desc(Total))


prueba = datJam_df %>% mutate(COD_NOMBRE_END_F = ifelse(is.na(COD_NOMBRE_END),COD_NOMBRE_ESQ_f,COD_NOMBRE_END)) %>%
  group_by(COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaStr,diaSem,finDeSem,hora,direction) %>% 
  mutate(N3 = as.numeric(level == 3),
         N4 = as.numeric(level == 4)) %>% 
  summarise(n = n(),
            N3 = sum(N3),
            N4 = sum(N4),
            N34 = N3 + N4,
            lengthM = mean(length,na.rm = T),
            lengthSD = sd(length,na.rm = T)) %>%
  ungroup() %>% filter(!is.na(COD_NOMBRE_ESQ)) %>%
  mutate(CV = lengthSD/lengthM) %>%
  arrange(desc(N34)) 


pruebaAgg = prueba %>% 
  group_by(COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaSem,finDeSem,hora,direction) %>%
  summarise(
            nT = sum(n),
            nAgg = n(),
            N3medio = mean(N3),
            N4medio = mean(N4),
            N34medio = mean(N34),
            lengthM = mean(lengthM,na.rm = T),
            lengthSD = max(lengthSD,na.rm = T))


pp = prueba %>% filter(COD_NOMBRE == 918 & diaSem == 'viernes' & hora == 17 & substr(diaStr,6,7) == '04') %>% arrange(diaStr,COD_NOMBRE_END_F)

hh = pruebaAgg %>% filter(nAgg > 20)


################################
#### Por segmentos





segmUsar = datJamSegm_df %>% group_by(ID_segmento) %>% summarise(n = n()) %>% filter(n > 200)


datJamSegm_df = datJamSegm_df %>% mutate(COD_NOMBRE_END_F = ifelse(is.na(COD_NOMBRE_END),COD_NOMBRE_ESQ_f,COD_NOMBRE_END))

pruebaS = datJamSegm_df %>% #filter(ID_segmento %in% segmUsar$ID_segmento) %>%
  group_by(ID_segmento,COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaStr,diaSem,finDeSem,hora,direction) %>% 
  mutate(N3 = as.numeric(level == 3),
         N4 = as.numeric(level == 4)) %>% 
  summarise(n = n(),
            N3 = sum(N3),
            N4 = sum(N4),
            N34 = N3 + N4,
            lengthM = mean(length,na.rm = T),
            lengthSD = sd(length,na.rm = T)) %>%
  ungroup() %>% filter(!is.na(COD_NOMBRE_ESQ)) %>%
  mutate(CV = lengthSD/lengthM) %>%
  arrange(desc(N34)) 



 pruebaAggS= pruebaS %>% 
  group_by(ID_segmento,COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaSem,finDeSem,hora,direction) %>%
  summarise(
    nT = sum(n),
    nAgg = n(),
    N3medio = mean(N3),
    N4medio = mean(N4),
    N34medio = mean(N34),
    lengthM = mean(lengthM,na.rm = T),
    lengthSD = max(lengthSD,na.rm = T)) %>%
  arrange(ID_segmento)

 
 
pruebaAggS2 = pruebaAggS %>% ungroup() %>%
   group_by(COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaSem,finDeSem,hora,direction) %>%
     mutate(Total = sum(nT)) %>% 
  arrange(desc(nT)) %>%
     slice_head() %>%
     ungroup() %>% 
  arrange(Total)  %>%
  group_by(ID_segmento,diaSem,hora,direction) %>%
  mutate(TotalT = sum(Total)) %>% 
  arrange(desc(Total)) %>%
  slice_head() %>%
  ungroup() %>%
  arrange(desc(TotalT))

hhS = pruebaAggS %>% filter(nAgg > 10)

hhS = pruebaAggS %>% filter(nAgg > 10 & nT > 200 & N34medio > 5)


hhSAgg = hhS %>% group_by(ID_segmento,COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F) %>% slice_max(N34medio) %>%
  arrange(ID_segmento)


####################################################
####################################################
####################################################
##### REARMO TODO


pruebaS = datJamSegm_df %>% #filter(ID_segmento %in% segmUsar$ID_segmento) %>%
  group_by(ID_segmento,COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaStr,diaSem,finDeSem,hora,direction) %>% 
  mutate(N3 = as.numeric(level == 3),
         N4 = as.numeric(level == 4)) %>% 
  summarise(n = n(),
            N3 = sum(N3),
            N4 = sum(N4),
            N34 = N3 + N4,
            lengthM = mean(length,na.rm = T)) %>%
  ungroup() %>% filter(!is.na(COD_NOMBRE_ESQ)) %>%
  arrange(desc(N34)) 



pruebaAggS= pruebaS %>% 
  group_by(ID_segmento,COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaSem,finDeSem,hora,direction) %>%
  summarise(
    nEventos = sum(n),
    nDias = n(),
    N3medio = mean(N3),
    N4medio = mean(N4),
    N34medio = mean(N34),
    lengthM = mean(lengthM,na.rm = T)) %>%
  arrange(ID_segmento)



pruebaAggS2 = pruebaAggS %>% ungroup() %>%
  group_by(COD_NOMBRE,COD_NOMBRE_ESQ,COD_NOMBRE_END_F,diaSem,finDeSem,hora,direction) %>%
  mutate(Total = sum(nEventos)) %>% # Total de eventos generados por distintos segemntos entre las calles
  arrange(desc(nEventos)) %>%
  slice_head() %>% # me quedo con el segmento que genero la mayor cantidad de eventos
  ungroup() %>% 
  group_by(ID_segmento,diaSem,hora,direction) %>%
  arrange(desc(Total)) %>%
  slice_head() %>% # Me quedo con las secuencia de calles que contienen al segmento que genero la mayor cantida de eventos
  ungroup() %>%
  arrange(desc(Total))

