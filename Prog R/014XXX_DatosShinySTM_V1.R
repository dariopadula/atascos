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

library(fst)

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


arreglaDias = function(x) {
  x = gsub('Monday|lunes','Lunes',x)
  x = gsub('Tuesday|martes','Martes',x)
  x = gsub('Wednesday|miércoles|miercoles','Miércoles',x)
  x = gsub('Thursday|jueves','Jueves',x)
  x = gsub('Friday|viernes','Viernes',x)
  x = gsub('Saturday|sábado','Sábado',x)
  x = gsub('Sunday|domingo','Domingo',x)
  
  return(x)
}

######################################################
###### LEE base
dir('Datos/STM/')

basesAll = c("012022","022022","032022","042022","052022") #,

basesUnir = list()  
puntosUnir = list()

indCambios = FALSE
STMBaseNames = character()


####################################
####################################
## Hasta que vengan los coordenadas
# load(file = 'Shiny/Insumos/XXX_puntSTMUnic.RData')
####################################
####################################

for(bb in basesAll) {
  
  nameBase = bb

  ######################################
  ##### Registra las bases que se van poniendo
  ##### e indica si hay que calcular o no
  IndAgg = FALSE
  IndIni = FALSE
  if(file.exists('BasesActualiza/STMBaseNames.RData')) {
    load('BasesActualiza/STMBaseNames.RData')
    IndAgg = !nameBase %in% STMBaseNames
    STMBaseNames = unique(c(STMBaseNames,nameBase))
    
    ###### Cargo las bases previas
    if(IndAgg) {
      if(length(basesUnir) == 0) {
        # load('Shiny/Insumos/XXX_DatosSTMShiny.RData')
        # basesUnir[[1]] = datSTM_df
        basesUnir[[1]] = read.fst("Shiny/Insumos/datSTM_df.fst")
        load('Shiny/Insumos/XXX_puntSTMUnic.RData')
        puntosUnir[[1]] = puntSTMUnic
      }
    }
  } else {
    IndIni = TRUE
    STMBaseNames = c(STMBaseNames,nameBase)
  }
  

  ###################################
  ##### Si ya esta ingresada no se calcula
  

  
  if((IndAgg | IndIni)) {  
  
    indCambios = TRUE
  
    ### Carga datos
    print(paste0(Sys.time(),' :Lee datos de la bases ',nameBase))
    datos = read.table(paste0('Datos/STM/stm_',nameBase,'.csv'),sep = ',',header = T) %>%
      rename('ID_parada' = 'parad_codigo')
    print(paste0(Sys.time(),' :Final del proceso'))
    
############################################    
############################################    
    # datos = datos %>% left_join(puntSTMUnic)
############################################    
############################################    
    

    
    # Saca las filas sin coordenadas
    datSTM_df = datos %>% filter(!is.na(lat) & !is.na(long))
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    
    print(paste0(Sys.time(),' :Encuentra paradas unicas de la base ',nameBase))
    #########################################
    ############# Coordenadas unicas
    puntSTMUnic = datSTM_df %>% 
      dplyr::select(ID_parada,lat,long) %>% 
      group_by(ID_parada) %>% slice_head() %>% data.frame()
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    
    
    print(paste0(Sys.time(),' :Genera nuevas variables en datSTM_df de la base ',nameBase))
    # Genera nuevas variables
    datSTM_df = datSTM_df %>% 
      mutate(minutoHora = 60*hora + franja - 30,
             levelDia = ifelse(dia_semana == 1,7,dia_semana-1)) %>%
             dplyr::select(-c(lat,long,nombre_dia)) %>%
      rename('anio' = 'año')
    
    datSTM_df$nameBase = as.character(nameBase)
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    
    
    print(paste0(Sys.time(),' :Carga en la lista el datSTM_df y puntSTMUnic de la base ',nameBase))
    ### Agrega las posisiones a las ya existentes si ers que ya había gregadas
    ### y para los datos del conteo 
      puntosUnir[[length(puntosUnir) + 1]] = puntSTMUnic
      basesUnir[[length(basesUnir) + 1]] = datSTM_df
      
      
    ### Guarda el vector de bases ingresadas  
      save(STMBaseNames,file = 'BasesActualiza/STMBaseNames.RData')
  } else {
    print(paste0('La base de nombre: ',nameBase,' ya fue ingresada'))
  }

}



############################################
###### Si hay cambios se generan las bases nuebas agregando

if(indCambios) {
  
  
  print(paste0(Sys.time(),' :Actualizo cambios'))
##### Genera los puntos unicos de conteo  
  if(length(puntosUnir) > 1) {
    puntSTMUnic = do.call(rbind,puntosUnir)
    puntSTMUnic = puntSTMUnic %>% 
      group_by(ID_parada) %>%
      slice_head()
  } else {
    puntSTMUnic = puntosUnir[[1]]
  }

  
##### Genera el data frame con el conteo
  if(length(puntosUnir) > 1) {  
    datSTM_df = as_tibble(data.table::rbindlist(basesUnir))
  } else {
    datSTM_df = basesUnir[[1]]
  }
  
  print(paste0(Sys.time(),' :Final del proceso'))
  ##########################################
  ############# Guardo los datos para el shiny
  print(paste0(Sys.time(),' :Guarda bases actualizadas'))
  write.fst(datSTM_df, "Shiny/Insumos/datSTM_df.fst")
  save(puntSTMUnic, file = 'Shiny/Insumos/XXX_puntSTMUnic.RData')
  print(paste0(Sys.time(),' :Final del proceso'))
} else {
  print('No hay cambios para adaptar')
}





