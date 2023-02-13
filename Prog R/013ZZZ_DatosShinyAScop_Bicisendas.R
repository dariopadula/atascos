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
library(conflicted)

library(fst)

######################################################
###### FUnciones
fun = dir('Funciones/')
fun = fun[grep('\\.R',fun,ignore.case = T)]
for(ii in fun) source(paste0('Funciones/',ii))


### Conflictos
conflict_prefer("hour", "lubridate")
conflict_prefer("minute", "lubridate")
conflict_prefer("first", "dplyr")
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

######################################################
###### LEE base
dir('Datos/Conteo/')

basesAll = c("202112","202201","202202","202203","202204",
             "202205","062022","072022","202208","202209",
             "202210","202211","202212","202301") #,"202203"

basesUnir = list()  
puntosUnir = list()

indCambios = FALSE
AScopBaseNamesBici = character()

for(bb in basesAll) {
  
  nameBase = bb

  ######################################
  ##### Registra las bases que se van poniendo
  ##### e indica si hay que calcular o no
  IndAgg = FALSE
  IndIni = FALSE
  if(file.exists('BasesActualiza/AScopBaseNamesBici.RData')) {
    load('BasesActualiza/AScopBaseNamesBici.RData')
    
    IndAgg = !nameBase %in% AScopBaseNamesBici
    AScopBaseNamesBici = unique(c(AScopBaseNamesBici,nameBase))
    
    ###### Cargo las bases previas
    if(IndAgg) {
      if(length(basesUnir) == 0) {
        #load('Shiny/Insumos/XXX_DatosAScopShinyBici.RData')
        load('Shiny/Insumos/XXX_puntAScoptUnic.RData')
        basesUnir[[1]] = 0
        puntosUnir[[1]] = puntAScoptUnic
      }
    }
  } else {
    IndIni = TRUE
    AScopBaseNamesBici = c(AScopBaseNamesBici,nameBase)
  }
  

  ###################################
  ##### Si ya esta ingresada no se calcula
  

  
  if((IndAgg | IndIni)) {  
  
    indCambios = TRUE
  
    ### Carga datos
    print(paste0(Sys.time(),' :Lee datos de la bases ',nameBase))
    datos = read.table(paste0('Datos/Conteo/carriles_',nameBase,'.csv'),sep = ',',header = T)
    print(paste0(Sys.time(),' :Final del proceso'))
    ## Quedarse solo con los SP
    # usar como unico dsc_avenida, dsc_int_anterior y dsc_int_siguiente, con la base de totales
    
    colnames(datos) = gsub('info_detector_carriles.','',names(datos))
    
    ############## Datos dep
    # Solo sensores SP
    
    print(paste0(Sys.time(),' :Depura datos y selecciona variables de la bases ',nameBase))
    datos = subset(datos,tipo_sensor == 'SP')
    
    # Variables a quedarse
    varSelect = c("dsc_avenida","dsc_int_anterior","dsc_int_siguiente",'cod_sentido',"latitud",
                  "longitud","fecha_hora","volume",
                  "classacount","classbcount","classccount","classdcount","classecount",
                  "spacemeanspeed","cod_nivel_servicio")
    
    # Saca las filas sin coordenadas
    datAScop_df = datos %>% filter(!is.na(latitud) & !is.na(longitud)) %>%
      dplyr::select_at(vars(dplyr::one_of(varSelect))) %>%
      mutate(ID_posSens = paste(dsc_avenida,dsc_int_anterior,dsc_int_siguiente,sep = '_'))
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    
    print(paste0(Sys.time(),' :Encuentra puntos de conteo unicos de la base ',nameBase))
    #########################################
    ############# Coordenadas unicas
    puntAScoptUnic = datAScop_df %>% 
      dplyr::select(latitud,longitud,dsc_avenida,dsc_int_anterior,dsc_int_siguiente,cod_sentido) %>% 
      group_by(latitud,longitud,dsc_avenida,dsc_int_anterior,dsc_int_siguiente,cod_sentido) %>% slice_head() %>%
      mutate(ID_posSens = paste(dsc_avenida,dsc_int_anterior,dsc_int_siguiente,sep = '_')) %>% data.frame()
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    
    
    print(paste0(Sys.time(),' :Genera nuevas variables en datAScop_df de la base ',nameBase))
    # Genera nuevas variables
    datAScop_df = datAScop_df %>% 
      mutate(datemodified = as.POSIXct(strptime(fecha_hora, "%Y-%m-%d %H:%M:%S")),
             diaStr = substr(datemodified,1,10),
             hora = hour(datemodified),
             minuto = minute(datemodified),
             minutoHora = 60*hora + minuto,
             minutoHora = minutoHora - minutoHora %% 5,
             ID_posSens = paste(dsc_avenida,dsc_int_anterior,dsc_int_siguiente,sep = '_')) %>%
      dplyr::select(-latitud,-longitud,-dsc_avenida,-dsc_int_anterior,-dsc_int_siguiente)
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    print(paste0(Sys.time(),' :Agrega en todos los carriles datAScop_df de la base ',nameBase))
    # Agraga en todos los carriles
    datAScop_df = datAScop_df %>% 
      group_by(ID_posSens,diaStr,minutoHora) %>%
      summarise(count = n(),
                across(c("volume","classacount","classbcount","classccount","classdcount","classecount"), ~ sum(.x, na.rm = TRUE)),
                spacemeanspeed = mean(spacemeanspeed,na.rm = T),
                cod_nivel_servicio = first(cod_nivel_servicio))
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    print(paste0(Sys.time(),' :Carga en la lista el datAScop_df y puntAScoptUnic de la base ',nameBase))
    ### Agrega las posisiones a las ya existentes si ers que ya había gregadas
    ### y para los datos del conteo 
      puntosUnir[[length(puntosUnir) + 1]] = puntAScoptUnic
      basesUnir[[length(basesUnir) + 1]] = datAScop_df
      
      
    ### Guarda el vector de bases ingresadas  
      save(AScopBaseNamesBici,file = 'BasesActualiza/AScopBaseNamesBici.RData')
  } else {
    print(paste0('La base de nombre: ',nameBase,' ya fue ingresada'))
  }

}



############################################
###### Si hay cambios se generan las bases nuevas agregando

if(indCambios) {
  
  
  print(paste0(Sys.time(),' :Actualizo cambios'))
##### Genera los puntos unicos de conteo  
  if(length(puntosUnir) > 1) {
    puntAScoptUnic = do.call(rbind,puntosUnir)
    puntAScoptUnic = puntAScoptUnic %>% 
      group_by(ID_posSens) %>%
      slice_head()
  } else {
    puntAScoptUnic = puntosUnir[[1]]
  }

  
##### Genera el data frame con el conteo
  if(length(basesUnir) > 1) { 
    if(!IndIni) basesUnir[[1]] = read.fst("Shiny/Insumos/datAScopBiciSend_df.fst")
    datAScop_df = as_tibble(data.table::rbindlist(basesUnir))
  } else {
    datAScop_df = basesUnir[[1]]
  }
  
  print(paste0(Sys.time(),' :Final del proceso'))
  ##########################################
  ############# Guardo los datos para el shiny
  print(paste0(Sys.time(),' :Guarda bases actualizadas'))
  write.fst(datAScop_df, "Shiny/Insumos/datAScopBiciSend_df.fst")
  save(puntAScoptUnic, file = 'Shiny/Insumos/XXX_puntAScoptUnic.RData')
  print(paste0(Sys.time(),' :Final del proceso'))
} else {
  print('No hay cambios para adaptar')
}





