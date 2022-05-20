
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


#### PARA ARREGLAR FECHAS
sacaFecha = function(xx) {
  yy = strsplit(xx,'_')[[1]]
  return(yy[2])
}


####### Genera fecha tope de maxia y minima
genLimFecha = function(xx) {
  
  aux = strsplit(xx,'_')[[1]]

  if(length(aux) == 2) {
    anio = as.numeric(aux[1])
    mes = as.numeric(aux[2])
    
    anioMax = anio
    mesMax = mes + 1
    if(mes == 12) {
      anioMax = anio + 1
      mesMax = 1
    }
    
    mesMax = as.character(ifelse(nchar(mesMax) == 1,paste0('0',mesMax),mesMax))
    mes = as.character(ifelse(nchar(mes) == 1,paste0('0',mes),mes))
    
    fechaMin = paste0(anio,'-',mes,'-01 00:00:01')
    fechaMax = paste0(anioMax,'-',mesMax,'-01 00:00:01')
    
    res = c(fechaMin,fechaMax)
    
    return(res)

  } else {
    return(NULL)
  }

}


######################################################
###### LEE base
dir('Datos/Cortes/')

basesAll = c("2021_12","2022_01","2022_02","2022_03","2022_04") #,"202203"

datCor_dfList = list()
datCorSegm_dfList = list()
segmentUnicList = list()

indCambios = FALSE
CortesBaseNames = character()

for(bb in basesAll) {
  
  nameBase = bb
  
  ######################################
  ##### Registra las bases que se van poniendo
  ##### e indica si hay que calcular o no
  IndAgg = FALSE
  IndIni = FALSE
  if(file.exists('BasesActualiza/CortesBaseNames.RData')) {
    load('BasesActualiza/CortesBaseNames.RData')
    IndAgg = !nameBase %in% CortesBaseNames
    CortesBaseNames = unique(c(CortesBaseNames,nameBase))
    
    ###### Cargo las bases previas
    if(IndAgg) {
      if(length(datCorSegm_dfList) == 0) {
        load('Shiny/Insumos/XXX_DatosShinyCortes.RData')
        datCor_dfList[[1]] = datCor_df
        datCorSegm_dfList[[1]] = datCorSegm_df %>% dplyr::select(-any_of('ID_segmento'))
        segmentUnicList[[1]] = segmentUnicCor %>% dplyr::select(-any_of('ID_segmento'))
      }
    }
  } else {
    IndIni = TRUE
    CortesBaseNames = c(CortesBaseNames,nameBase)
  }
  
  
  ###################################
  ##### Calcula el rango de fechas
  if(length(datCor_dfList) > 0) {
    maxDate = max(do.call(c,lapply(datCor_dfList, function(xx) max(xx$datemodified))))
    minDate = min(do.call(c,lapply(datCor_dfList, function(xx) min(xx$datemodified))))
  }
  
  
  ###################################
  ##### Si ya esta ingresada no se calcula
  
  if((IndAgg | IndIni)) {  
    

    indCambios = TRUE  
    ######################################################
    ######################################################
    ###### LEE base
    print(paste0(Sys.time(),' : Carga los datos de la base (',nameBase,')'))
    datos = read.table(paste0('Datos/Cortes/cortes',nameBase,'.csv'),sep = ',',header = T)
    print(paste0(Sys.time(),' : Fin del proceso'))
    
    #############################################
    #### Paso las fecas de character a dates
    
    
    print(paste0(Sys.time(),' : Crea la base datCor para la base (',nameBase,')'))
    
    datCor = datos %>% filter(city == 'Montevideo') %>% 
      rowwise() %>% mutate(datecreated = sacaFecha(id)) %>%
      ungroup() %>%
      mutate(datemodified = as.POSIXct(strptime(gsub('T',' ',max_datemodified), "%Y-%m-%d %H:%M:%S")),
             datecreated = as.POSIXct(strptime(gsub('T',' ',datecreated), "%Y-%m-%d %H:%M:%S"))) 
    
    
    refDate = genLimFecha(nameBase)
    
    if(!is.null(refDate)) {
      datCor = datCor %>% 
        filter(datemodified > as.POSIXct(strptime(refDate[1], "%Y-%m-%d %H:%M:%S")) & 
                 datemodified < as.POSIXct(strptime(refDate[2], "%Y-%m-%d %H:%M:%S")))
    }
   
    
    
    ##### Filtro de fecha
    
    if(IndIni) {
      datCor = datCor  %>%
        filter(datemodified >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))
    }
    
    if(IndAgg) {
      datCor = datCor  %>%
        filter(datemodified > maxDate | datemodified < minDate)
      
      datCor = datCor  %>%
        filter(datemodified >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))
      
      if(nrow(datCor) == 0) {
        stop("No hay eventos posteriores ni anteriores para agregar")
      }
    }
    
    
  ###### Calcula variables y tiempos por dia  
    datCor = datCor %>% 
      arrange(desc(id),desc(datemodified)) %>% 
      dplyr::select(-city,-location_centroid_lat,-location_centroid_lon) %>%
      mutate(hora = hour(datemodified),
             minuto = minute(datemodified),
             minutoHora = 60*hora + minuto) %>%
      group_by(id,diaStr) %>%
      slice_max(datemodified) %>%
      ungroup() %>%
      as.data.frame() %>%
      getLineGeomV2(.) %>%
      dplyr::select(-location) %>%
      group_by(id) %>%
      arrange(desc(datemodified)) %>%
      mutate(ID_EntityDia = row_number()) %>%
      ungroup() %>% 
      # rowwise() %>% mutate(datecreated = max(c(datecreated,datepublished))) %>%
      # ungroup() %>%
      arrange(id,desc(ID_EntityDia)) %>%
      st_as_sf(.,wkt = 'geometry',crs = 4326) %>%
      st_transform(.,crs = 32721) %>%
      mutate(ID_Base = row_number(),
             difCreate = as.numeric(difftime(datemodified,datecreated, units = 'mins')),
             minutoIniDia = 
               ifelse(difCreate < minutoHora,round(minutoHora - difCreate),0),
             minutoLastDia = minutoHora) %>%
      filter(!st_is_empty(st_geometry(.)))
    
    print(paste0(Sys.time(),' : Fin del proceso'))
    
    
    ######################################################
    ######################################################
    ######################################################
    
    ########################################
    
    ###############################################################
    ########### ARREGLO LOS DIAS 
    # disOrd = names(table(datCor$diaStr))
    # datCor = datCor %>% mutate(diaStr = factor(diaStr,levels = disOrd))
    
    
    print(paste0(Sys.time(),' : Segmentiza la base (',nameBase,')'))
    
    datCor_df = datCor %>% st_set_geometry(NULL)
    
    ###### Genera los segmentos
    datCorSegm = st_segments(datCor,progress = FALSE) %>% 
      rename('geometry' = 'result') %>% 
      mutate(ID_coord = as.character(geometry))
    
    print(paste0(Sys.time(),' : Fin del proceso'))
    
    ##### Segmentos unicos
    print(paste0(Sys.time(),' : Encuentra segemntos unicos (',nameBase,')'))
    
    segmentUnicList[[length(segmentUnicList) + 1]] = unique(datCorSegm[,c('geometry')]) %>% 
      mutate(ID_coord = as.character(geometry))
    

    print(paste0(Sys.time(),' : Fin del proceso'))    
    
    #################################
    ##### GUARDO LOS SEGMENTOS
    
    print(paste0(Sys.time(),' : Guarda las bases procesadas en las listas (',nameBase,')'))
    
    varsSacar = c('NOM_CALLE','ID_CODCALLE','NOM_CALLE_END',
                  'NOM_CALLE_ESQ_f',
                  'ID_CODCALLE','NOM_CALLE_END','ID_calle_Ini',
                  'NOM_CALLE_ESQ','ID_calle_Fin','endnode','jamtype') 
    
    datCorSegm_dfList[[length(datCorSegm_dfList) + 1]] = datCorSegm %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(-any_of(varsSacar))
    
    datCor_dfList[[length(datCor_dfList) + 1]] = datCor %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(-any_of(varsSacar))
    
    
    print(paste0(Sys.time(),' : Fin del proceso')) 
    ########### GUardo el vector de bases ingresadas
    print(paste0(Sys.time(),' :Guarda vector de bases cargadas'))
    save(CortesBaseNames,file = 'BasesActualiza/CortesBaseNames.RData')
    print(paste0(Sys.time(),' :Final del proceso'))
    
  } else {
    print(paste0('La base de nombre: ',nameBase,' ya fue ingresada'))
  }
}


###################################################################
###################################################################
###################################################################

############################################
###### Si hay cambios se generan las bases nuebas agregando
if(indCambios) {
  print(paste0(Sys.time(),' :Actualizo cambios'))
  ##### Genera los puntos unicos de conteo  
  if(length(segmentUnicList) > 1) {
    segmentUnicCor = do.call(rbind,segmentUnicList)
    
    segmentUnicCor = segmentUnicCor %>% 
      group_by(ID_coord) %>%
      slice_head() %>%
      ungroup()
  } else {
    segmentUnicCor = segmentUnicList[[1]]
  }
  
  segmentUnicCor = segmentUnicCor %>% mutate(ID_segmento = row_number())
  ####### Genero la base de Jams segmentizada
  if(length(datCorSegm_dfList) > 1) {
    datCorSegm_df = as_tibble(data.table::rbindlist(datCorSegm_dfList))
  } else {
    datCorSegm_df = as_tibble(datCorSegm_dfList[[1]])
  }
  
  datCorSegm_df = datCorSegm_df %>% left_join(segmentUnicCor %>% 
                                                st_set_geometry(NULL) %>% 
                                                dplyr::select(ID_coord,ID_segmento),
                                              by = 'ID_coord')
  
  disOrd = names(table(datCorSegm_df$diaStr))
  datCorSegm_df = datCorSegm_df %>% mutate(diaStr = factor(diaStr,levels = disOrd))
  
  
  ####### Genero la base de Jams sin segmentizar
  if(length(datCor_dfList) > 1) {
    datCor_df = as_tibble(data.table::rbindlist(datCor_dfList))
  } else {
    datCor_df = as_tibble(datCor_dfList[[1]])
  }
  

  print(paste0(Sys.time(),' :Final del proceso'))
  
  ####################################################
  ####################################################
  ####################################################
  
  print(paste0(Sys.time(),' :Guarda bases que son insumo para shiny y resumen'))
  save(datCor_df,datCorSegm_df,segmentUnicCor, file = 'Shiny/Insumos/XXX_DatosShinyCortes.RData')
  print(paste0(Sys.time(),' :Final del proceso'))
  
} else {
  print('No hay cambios para adaptar')
}




