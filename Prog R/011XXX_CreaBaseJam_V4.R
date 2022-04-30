
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

######################################################
#### Nomenclator IM
vias = st_read('SHP/v_mdg_vias')

######################################################
#### Proyeccion de clles en segmentos (para calpular el sentido)
load('Resultados/001_segmProy.RData')

######################################################
#### Identificador de bases
allBases = c('202112141520','202112231648','202201191112','202202071004','07_08_022022',
             '01_31_032022_P1','01_31_032022_P2','01_31_032022_P3','01_31_032022_P4',
             '01_25_042022_P1','01_25_042022_P2','01_25_042022_P3')

# allBases = c('202112141520','202112231648','202201191112','202202071004','07_08_022022')

datJam_dfList = list()
datJamSegm_dfList = list()
segmentUnicList = list()

indCambios = FALSE
jamBaseNames = character()
for(bb in allBases) {
  iniTime = Sys.time()
  
  ######################################################
  ###### LEE base
  nameBase = bb


  ######################################
  ##### Registra las bases que se van poniendo
  ##### e indica si hay que calcular o no
  IndAgg = FALSE
  IndIni = FALSE
  ID_BaseRef = 0
  
  if(file.exists('BasesActualiza/jamBaseNames.RData')) {
    load('BasesActualiza/jamBaseNames.RData')
    IndAgg = !nameBase %in% jamBaseNames
    jamBaseNames = unique(c(jamBaseNames,nameBase))
    
    ###### Cargo las bases previas
    if(IndAgg) {
      if(length(datJamSegm_dfList) == 0) {
        load('Shiny/Insumos/XXX_DatosShiny.RData')
        datJam_dfList[[1]] = datJam_df
        datJamSegm_dfList[[1]] = datJamSegm_df %>% dplyr::select(-any_of('ID_segmento'))
        segmentUnicList[[1]] = segmentUnic %>% dplyr::select(-any_of('ID_segmento'))
      }
    }
  } else {
    IndIni = TRUE
    jamBaseNames = c(jamBaseNames,nameBase)
  }
  
  
  
  if(length(datJam_dfList) > 0) {
    maxDate = max(do.call(c,lapply(datJam_dfList, function(xx) max(xx$datemodified))))
    minDate = min(do.call(c,lapply(datJam_dfList, function(xx) min(xx$datemodified))))
  }
  
  ###################################
  ##### Si ya esta ingresada no se calcula
  
  if((IndAgg | IndIni)) {
    
    indCambios = TRUE
    
    #### LEE BASE
    print(paste0(Sys.time(),' :Lee datos de la bases ',nameBase))
    datos = read.table(paste0('Datos/etwazetrafficjam_',nameBase,'.csv'),sep = ',',header = T)
    print(paste0(Sys.time(),' :Final del proceso'))
    
    print(paste0(Sys.time(),' :Arma base datJam'))
    #############################################
    #### Me quedo solo con los atascos
    datJam = subset(datos,delay >= 0 & city == 'Montevideo')
    

    #############################################
    #### Paso las fecas de character a dates
    
    datJam = datJam %>% mutate(datecreated = as.POSIXct(strptime(gsub('T',' ',datecreated), "%Y-%m-%d %H:%M:%S")),
                               datemodified = as.POSIXct(strptime(gsub('T',' ',datemodified), "%Y-%m-%d %H:%M:%S")),
                               datepublished = as.POSIXct(strptime(gsub('T',' ',datepublished), "%Y-%m-%d %H:%M:%S")))
    
    ##### Filtro de fecha
    
    if(IndIni) {
      datJam = datJam  %>%
        filter(datemodified >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))
    }
    
    if(IndAgg) {
      datJam = datJam  %>%
        filter(datemodified > maxDate | datemodified < minDate)
      
      datJam = datJam  %>%
        filter(datemodified >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))
      
      if(nrow(datJam) == 0) {
        stop("No hay eventos posteriores ni anteriores para agregar")
      }
    }
    #############################################
    ##### EXTRAE LAS COORDENADAS
    
    ### del Centroide y de los extremos del segemnto
    datJam = datJam %>% arrange(desc(entity_id),desc(datemodified)) %>% 
      getLineGeomV2(.) %>% 
      dplyr::select(-any_of(c('city','country','location_centroid','location','entity_type','fiware_servicepath','pubmillis',
                              'turntype',"location_centroid_lat","location_centroid_lon","blockingalertuuid"))) %>%
      mutate(diaStr = substr(datemodified,1,10),
             diaSem = weekdays(datemodified),
             finDeSem = ifelse(diaSem %in% c('sábado','domingo'),'Fin de semana','Lunes a viernes'),
             hora = hour(datemodified),
             minuto = minute(datemodified),
             minutoHora = 60*hora + minuto,
             diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo'))) %>%
      st_as_sf(.,wkt = 'geometry',crs = 4326) %>%
      st_transform(.,crs = 32721) %>%
      mutate(ID_Base = row_number() + ID_BaseRef)
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    ########################################################
    ########################################################
    ##############################################
    ######## Agrega codigos de calle, de calle de inicio y calle fin
    print(paste0(Sys.time(),' :Codifica calles con nomenclatura IM'))
    ######################################################
    #### Calles a buscar
    calleBusDF = data.frame(calleOrig = c(datJam$street,datJam$endnode)) %>% 
      group_by(calleOrig) %>%
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
    
    
    
    if(file.exists('Resultados/011_codCallesAllPrev.RData')) {
      load('Resultados/011_codCallesAllPrev.RData')
      
      
      codCallesAllPrev = codCallesAllPrev[!duplicated(codCallesAllPrev$calleOrig),]
      noEstan = calleBusDF[!rownames(calleBusDF) %in% rownames(codCallesAllPrev),]
      
      noEstan = noEstan %>% dplyr::select(-cont)
      
      codCallesAllPrev = codCallesAllPrev %>% mutate(callesBuscar = arreglaCaracteres(calleOrig))
      
      calleBusDF = rbind(codCallesAllPrev[,colnames(noEstan)],noEstan)
      
    }
    
    
    
    
    
    ###########################################
    ##### Encuentra las que estan igual
    
    for(ii in rownames(calleBusDF)[is.na(calleBusDF$NOM_CALLE)]) {
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
                                              join = st_intersects,largest = T) %>%
      suppressWarnings()
    
    
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
    
    
    datJam$ID_CODCALLE = ifelse(datJam$street == '',as.character(datJam$ID_Base),
                                datJam$street)
    
    
    datJam[,'NOM_CALLE'] =  codCallesAll[as.character(datJam$ID_CODCALLE),'NOM_CALLE']
    datJam[,'COD_NOMBRE'] =  codCallesAll[as.character(datJam$ID_CODCALLE),'COD_NOMBRE']
    
    datJam[,'NOM_CALLE_END'] = codCallesAll[as.character(datJam$endnode),'NOM_CALLE'] 
    datJam[,'COD_NOMBRE_END'] = codCallesAll[as.character(datJam$endnode),'COD_NOMBRE'] 
    
    
    ####### GUardo las calles encontradas hasta el momento
    codCallesAllPrev = codCallesAll[codCallesAll$Vacia == 0,]
    save(codCallesAllPrev,file = 'Resultados/011_codCallesAllPrev.RData')
    
    print(paste0(Sys.time(),' :Final del proceso'))
    ############################################
    ###### Agrega direccion
    ############################################
    ##### Encuentro la calle de inicio
    
    print(paste0(Sys.time(),' :Infiere la direccion de las calles'))
    
    puntosIni = datJam %>% dplyr::select(ID_Base,COD_NOMBRE) %>% 
      st_cast(.,'POINT') %>%
      group_by(ID_Base) %>%
      slice_head() %>% suppressWarnings()
    
    puntosFin = datJam %>% dplyr::select(ID_Base,COD_NOMBRE) %>% 
      st_cast(.,'POINT') %>%
      group_by(ID_Base) %>%
      slice_tail() %>% suppressWarnings()
    
    
    #################################################
    ####### Agrega direccion
    
    
    
    segmProyPoint = segmProy %>% 
      st_as_sf(.,wkt = 'geometry',crs = 32721) %>% 
      st_cast(.,'POINT') %>% suppressWarnings()
    
    
    
    
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
    
    datJam = datJam %>% left_join(puntosAll %>% dplyr::select(-COD_NOMBRE), by = 'ID_Base') %>%
      mutate(IndShift = ifelse(ID_calle_Ini <= ID_calle_Fin,0,1),
             direction = ifelse(IndShift == 1,paste(dirFin,dirIni,sep = '_'),
                                paste(dirIni,dirFin,sep = '_'))) 
    
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    aux = st_geometry(datJam)
    # calles no modificar
    
    print(paste0(Sys.time(),' :Hace shift de los segmentos de acuerdo al sentido'))
    
    cNoModif = c("av de las leyes","bv gral artigas")
    
    
    cond = datJam$direction == "ESTE_OESTE" & !is.na(datJam$direction) & !datJam$NOM_CALLE %in% cNoModif
    aux[cond] = aux[cond] + c(0,10)
    
    cond = datJam$direction == "NORTE_SUR" & !is.na(datJam$direction) & !datJam$NOM_CALLE %in% cNoModif
    aux[cond] =   aux[cond] + c(-10,0)
    
    cond = datJam$direction == "OESTE_ESTE" & !is.na(datJam$direction) & !datJam$NOM_CALLE %in% cNoModif
    aux[cond] = aux[cond] + c(0,-10)
    
    cond = datJam$direction == "SUR_NORTE" & !is.na(datJam$direction) & !datJam$NOM_CALLE %in% cNoModif
    aux[cond] = aux[cond] + c(10,0)
    
    
    st_geometry(datJam) = aux
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
 
###################################################    
###################################################
    
    ###### Genera los segmentos
    print(paste0(Sys.time(),' :Segmentiza las geometrias'))
    
    datJamSegm = st_segments(datJam,progress = FALSE) %>% #mutate(ID_fila = row_number() + ID_BaseSegRef) %>%
      rename('geometry' = 'result') %>% filter(!st_is_empty(.))  %>% 
      mutate(ID_coord = as.character(geometry)) %>% 
      suppressWarnings()
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    ##### Segmentos unicos
    print(paste0(Sys.time(),' :Encuentra segmentos unicos y los anexa a los ya existentes'))
    
    segmentUnicList[[length(segmentUnicList) + 1]] = unique(datJamSegm[,c('geometry','ID_coord')]) 
    
     #### PASO LA BASE DE SEGMENTOS A DATA FRAME
    
    print(paste0(Sys.time(),' :Saco variables y guardo en la listas'))
    
    varsSacar = c('NOM_CALLE','ID_CODCALLE','NOM_CALLE_END',
                  'NOM_CALLE_ESQ_f',
                  'ID_CODCALLE','NOM_CALLE_END','ID_calle_Ini',
                  'NOM_CALLE_ESQ','ID_calle_Fin','endnode','jamtype') 
    
    datJamSegm_dfList[[length(datJamSegm_dfList) + 1]] = datJamSegm %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(-any_of(varsSacar))
    
    datJam_dfList[[length(datJam_dfList) + 1]] = datJam %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(-any_of(varsSacar))
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    ### Borro las bases cargadas
    rm(list = c('datJam','datJamSegm'))
    #######################################

    print(paste0(Sys.time(),' :Final del proceso'))
    #########################################################
    
    ########### GUardo el vector de bases ingresadas
    print(paste0(Sys.time(),' :Guarda vector de bases cargadas'))
    save(jamBaseNames,file = 'BasesActualiza/jamBaseNames.RData')
    print(paste0(Sys.time(),' :Final del proceso'))

    
    finTime = Sys.time()
    totalTime = difftime(finTime,iniTime,units = 'mins')
    
    print(paste0(Sys.time(),' :FINAL DE TODO EL PROCESO PARA LA BASE: (',nameBase,') Y DURO ',round(as.numeric(totalTime),1),' MINUTOS'))

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
    segmentUnic = do.call(rbind,segmentUnicList)
    
    segmentUnic = segmentUnic %>% 
      group_by(ID_coord) %>%
      slice_head()
  } else {
    segmentUnic = segmentUnicList[[1]]
  }
  
  segmentUnic = segmentUnic %>% mutate(ID_segmento = row_number())
####### Genero la base de Jams segmentizada
  if(length(datJamSegm_dfList) > 1) {
    datJamSegm_df = as_tibble(data.table::rbindlist(datJamSegm_dfList))
  } else {
    datJamSegm_df = as_tibble(datJamSegm_dfList[[1]])
  }
  
  datJamSegm_df = datJamSegm_df %>% left_join(segmentUnic %>% 
                                                st_set_geometry(NULL) %>% 
                                        dplyr::select(ID_coord,ID_segmento),
                                        by = 'ID_coord')
  
  disOrd = names(table(datJamSegm_df$diaStr))
  datJamSegm_df = datJamSegm_df %>% mutate(diaStr = factor(diaStr,levels = disOrd))
  
  
####### Genero la base de Jams sin segmentizar
  if(length(datJam_dfList) > 1) {
    datJam_df = as_tibble(data.table::rbindlist(datJam_dfList))
  } else {
    datJam_df = as_tibble(datJam_dfList[[1]])
  }
  
  ##### Tabla con dias y meses y semana
  dsma = datJam_df %>% dplyr::select(diaStr,finDeSem,diaSem) %>% 
    group_by(diaStr) %>% 
    slice_head() %>%
    ungroup() %>%
    mutate(diaNum = as.numeric(substr(diaStr,9,10)),
           mes = substr(diaStr,6,7),
           anio = substr(diaStr,1,4),
           levelDia = as.numeric(diaSem),
           diaSem = as.character(diaSem)) %>%
    arrange(anio,mes,diaNum) %>% data.frame()
  
  rownames(dsma) = dsma$diaStr
  
  print(paste0(Sys.time(),' :Final del proceso'))
  ##############################################
  ##### Tabla para resumen de la base
  print(paste0(Sys.time(),' :Crea base resumen'))
  
  datJam_resum = datJam_df %>% 
    mutate(
      tiempoMP = as.numeric(difftime(datemodified,datepublished,units = 'mins')),
      diaMes = substr(diaStr,1,7)) %>% 
    group_by(entity_id,street) %>%
    summarise(count = n(),
              diaMes = last(diaMes),
              tiempoMP = max(tiempoMP),
              delay = mean(delay,na.rm = T),
              speedkmh = mean(speedkmh,na.rm = T),
              level = max(level,na.rm = T)) %>%
    ungroup() %>%
    mutate(duracionEv = ifelse(abs(tiempoMP - 2*count) > 2,2*count,tiempoMP)) %>%
    suppressMessages()
  
  print(paste0(Sys.time(),' :Final del proceso'))
  
  ####################################################
  ####################################################
  ####################################################
  
  print(paste0(Sys.time(),' :Guarda bases que son insumo para shiny y resumen'))
  
  save(datJam_df,datJamSegm_df,segmentUnic, file = 'Shiny/Insumos/XXX_DatosShiny.RData')
  save(dsma, file = 'Shiny/Insumos/XXX_dsma.RData')
  save(datJam_resum, file = 'Shiny/Insumos/XXX_datJam_resum.RData')
  
  print(paste0(Sys.time(),' :Final del proceso'))
  
} else {
  print('No hay cambios para adaptar')
}



