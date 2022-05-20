
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
#### Identificador de bases
allBases = c('202112141520','202112231648','202201191112','202202071004','07_08_022022',
             '01_31_032022_P1','01_31_032022_P2','01_31_032022_P3','01_31_032022_P4',
             '01_25_042022_P1','01_25_042022_P2','01_25_042022_P3')

# 
# if(file.exists('BasesActualiza/jamBaseNames.RData')) {
#   load('BasesActualiza/jamBaseNames.RData')
#   allBases = allBases[!allBases %in% jamBaseNames]
# }
# 
# 
# if(file.exists('BasesActualiza/jamDatosShiny.RData')) {
#   load('BasesActualiza/jamDatosShiny.RData')
# }


for(bb in allBases) {
  iniTime = Sys.time()
  
  # indAgregar = T
  # if(exists('jamBaseNames')) {
  #   indAgregar = !bb %in% jamBaseNames
  # }
  
  # indSegmentar = T
  # if(exists('jamDatosShiny')) {
  #   indSegmentar = !bb %in% jamDatosShiny
  # }
  ######################################################
  ###### LEE base
  nameBase = bb

  # if(indAgregar) {

  
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
      load('BasesR/011_datJam.RData')
      datJamPrev = datJam
      load('Resultados/011_datJamDir.RData')
      datJamDirPrev = datJamDir
      maxDate = max(datJamDir$datemodified)
      minDate = min(datJamDir$datemodified)
      ID_BaseRef = nrow(datJamPrev)
    }
  } else {
    IndIni = TRUE
    jamBaseNames = nameBase
  }
  
  
  ###################################
  ##### Si ya esta ingresada no se calcula
  
  if((IndAgg | IndIni)) {
    
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
        filter(datepublished >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))
    }
    
    if(IndAgg) {
      datJam = datJam  %>%
        filter(datepublished > maxDate | datepublished < minDate)
      
      datJam = datJam  %>%
        filter(datepublished >= as.POSIXct(strptime('2021-11-29 00:00:01', "%Y-%m-%d %H:%M:%S")))
      
      if(nrow(datJam) == 0) {
        stop("No hay eventos posteriores ni anteriores para agregar")
      }
    }
    #############################################
    ##### EXTRAE LAS COORDENADAS
    
    ### del Centroide y de los extremos del segemnto
    datJam = datJam %>% arrange(desc(entity_id),desc(datemodified)) %>% 
      getLineGeomV2(.) %>% 
      dplyr::select(-any_of(c('city','country','location_centroid','location','entity_type','fiware_servicepath','pubmillis','turntype'))) %>%
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
    
    load('Resultados/001_segmProy.RData')
    
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
    
    datJamDir = datJam %>% left_join(puntosAll %>% dplyr::select(-COD_NOMBRE), by = 'ID_Base') %>%
      mutate(IndShift = ifelse(ID_calle_Ini <= ID_calle_Fin,0,1),
             direction = ifelse(IndShift == 1,paste(dirFin,dirIni,sep = '_'),
                                paste(dirIni,dirFin,sep = '_'))) 
    
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    
    aux = st_geometry(datJamDir)
    # calles no modificar
    
    print(paste0(Sys.time(),' :Hace shift de los segmentos de acuerdo al sentido'))
    
    cNoModif = c("av de las leyes","bv gral artigas")
    
    
    cond = datJamDir$direction == "ESTE_OESTE" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
    aux[cond] = aux[cond] + c(0,10)
    
    cond = datJamDir$direction == "NORTE_SUR" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
    aux[cond] =   aux[cond] + c(-10,0)
    
    cond = datJamDir$direction == "OESTE_ESTE" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
    aux[cond] = aux[cond] + c(0,-10)
    
    cond = datJamDir$direction == "SUR_NORTE" & !is.na(datJamDir$direction) & !datJamDir$NOM_CALLE %in% cNoModif
    aux[cond] = aux[cond] + c(10,0)
    
    
    st_geometry(datJamDir) = aux
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    ###### Agrego el nombre de la base
    print(paste0(Sys.time(),' :Adjunta nueva base a las anteriores'))
    
    datJamDir$nomBase = nameBase
    datJam$nomBase = nameBase
    
    if(IndAgg) {
      
      datJam$street = arreglaCaracteres(datJam$street,F,F)
      nomDatJam = intersect(colnames(datJam),colnames(datJamPrev))
      datJam = rbind(datJamPrev[,nomDatJam],datJam[,nomDatJam])  
      
      datJamDir$street = arreglaCaracteres(datJamDir$street,F,F)
      nomDatJamDir = intersect(colnames(datJamDir),colnames(datJamDirPrev))
      datJamDir = rbind(datJamDirPrev[,nomDatJamDir],datJamDir[,nomDatJamDir])
      
    }
    
    print(paste0(Sys.time(),' :Final del proceso'))
    ########################################
    print(paste0(Sys.time(),' :Guardando las bases'))
    ####### GUarda las bases con y sin direccion
    save(datJam,file = 'BasesR/011_datJam.RData')
    #### GUARDO LA BASE CON LOS CODIGOS
    save(datJamDir,file = 'Resultados/011_datJamDir.RData')
    #### Actualizo las bases ingresadas
    save(jamBaseNames,file = 'BasesActualiza/jamBaseNames.RData')
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
  } else {
    print(paste0('La base de nombre: ',nameBase,' ya fue ingresada'))
  }
  
###################################################################
###################################################################
###################################################################
###### Geenra los datos para el shiny
  
  #######################################
  ###### PARCHE DIRECCION
  # if(!exists('datJamDir')) load('Resultados/011_datJamDir.RData')
  # if(!exists('datJam')) load('BasesR/011_datJam.RData')
  

  
  # datJam_new = datJamDir
  
  ######################################
  ##### Registra las bases que se van poniendo
  ##### e indica si hay que calcular o no
  # nameBaseIN = unique(datJam_new$nomBase)
  
  IndAgg = FALSE
  IndIni = FALSE
  ID_BaseSegRef = 0
  ID_segUnicRef = 0
  if(file.exists('BasesActualiza/jamDatosShiny.RData')) {
    load('BasesActualiza/jamDatosShiny.RData')
    IndAgg = sum(!nameBase %in% jamDatosShiny) > 0 
    nomBaseAgg = nameBase[!nameBase %in% jamDatosShiny]
    jamDatosShiny = unique(c(jamDatosShiny,nomBaseAgg))
    
    ###### Cargo las bases previas
    if(IndAgg) {
      load('Shiny/Insumos/XXX_DatosShiny.RData')
      datJamSegm_dfPrev = datJamSegm_df
      segmentUnicPrev = segmentUnic
      if(!exists('datJamDir')) load('Resultados/011_datJamDir.RData')
      
      datJamDir = datJamDir[datJamDir$nomBase %in% nomBaseAgg,]
      ID_BaseSegRef = nrow(datJamSegm_dfPrev)
      ID_segUnicRef = nrow(segmentUnicPrev)
    }
  } else {
    IndIni = TRUE
    jamDatosShiny = nameBase
  }
  
  
  
  if((IndAgg | IndIni)) {
    
    #############################################
    ##### data frame de la base datJamDir
    ########################################
    if(!exists('datJam')) load('BasesR/011_datJam.RData')
    datJam_df = datJam %>% st_set_geometry(NULL)
    rm(datJam)
    
    ###### Genera los segmentos
    print(paste0(Sys.time(),' :Segmentiza las geometrias'))
    
    datJamSegm = st_segments(datJamDir,progress = FALSE) %>% mutate(ID_fila = row_number() + ID_BaseSegRef) %>%
      rename('geometry' = 'result') %>% filter(!st_is_empty(.))  %>% 
      mutate(ID_coord = as.character(geometry)) %>% 
      suppressWarnings()
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    ##### Segmentos unicos
    print(paste0(Sys.time(),' :Encuentra segmentos unicos y los anexa a los ya existentes'))
    
    segmentUnic = unique(datJamSegm[,c('geometry','ID_coord')]) 
    
    #### Pregunta si es el inicio o si ya hay segemntos existentes
    
    if(IndIni) {
      segmentUnic = segmentUnic %>% mutate(ID_segmento = row_number())
    } else {
      segmentUnic = subset(segmentUnic,!ID_coord %in% segmentUnicPrev$ID_coord)
      if(nrow(segmentUnic) > 0) {
        segmentUnic = segmentUnic %>% mutate(ID_segmento = row_number() + ID_segUnicRef) 
        segmentUnic = rbind(segmentUnicPrev,segmentUnic)
      } else {
        segmentUnic = segmentUnicPrev  
      }
    }
    
    
    datJamSegm = datJamSegm %>% left_join(segmentUnic %>% 
                                            st_set_geometry(NULL) %>%
                                            dplyr::select(ID_coord,ID_segmento),
                                          by = 'ID_coord')
    
    
    #### PASO LA BASE DE SEGMENTOS A DATA FRAME
    datJamSegm_df = datJamSegm %>% st_set_geometry(NULL)
    
    
    
    print(paste0(Sys.time(),' :Final del proceso'))
    #######################################
    ####### Veo si agrego datos o no
    print(paste0(Sys.time(),' :Adjunta nueva base de datos segmentizados datJamSegm_df a los anteriores'))
    
    ##### BORRO BASES
    rm(datJamSegm)
    rm(datJamDir)
    
    if(IndAgg) {
      datJamSegm_dfPrev$diaStr = as.character(datJamSegm_dfPrev$diaStr)
      datJamSegm_df = datJamSegm_df %>% dplyr::select(-ID_coord)
      
      nomDatJamSeg = intersect(colnames(datJamSegm_df),colnames(datJamSegm_dfPrev))
      
      datJamSegm_df = rbind(datJamSegm_dfPrev[,nomDatJamSeg],datJamSegm_df[,nomDatJamSeg])
    }
    
    print(paste0(Sys.time(),' :Final del proceso'))
    #################################
    ##### GUARDO LOS SEGMENTOS
    # save(datJamSegm,file = 'Resultados/XXX_datJamSegm.RData')
    ###############################################################
    ########### ARREGLO LOS DIAS 
    
    print(paste0(Sys.time(),' :Crea base dsma'))
    
    disOrd = names(table(datJamSegm_df$diaStr))
    datJamSegm_df = datJamSegm_df %>% mutate(diaStr = factor(diaStr,levels = disOrd))

    
    
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
    #########################################################
    ####### ARREGLA CARACTERES
    print(paste0(Sys.time(),' :Elimina algunas variables que por ahora no se necesitan'))
    
    # varsSacar = c('NOM_CALLE','COD_NOMBRE','ID_CODCALLE','NOM_CALLE_END',
    #               'COD_NOMBRE_END','NOM_CALLE_ESQ_f','COD_NOMBRE_ESQ_f',
    #               'ID_CODCALLE','NOM_CALLE_END','COD_NOMBRE_END','ID_calle_Ini',
    #               'NOM_CALLE_ESQ','COD_NOMBRE_ESQ','ID_calle_Fin','endnode','jamtype') 
    # 

    varsSacar = c('NOM_CALLE','ID_CODCALLE','NOM_CALLE_END',
                  'NOM_CALLE_ESQ_f',
                  'ID_CODCALLE','NOM_CALLE_END','ID_calle_Ini',
                  'NOM_CALLE_ESQ','ID_calle_Fin','endnode','jamtype') 
    
        # datJam_df$street = arreglaCaracteres(datJam_df$street,F,F)
    datJam_df = datJam_df %>% dplyr::select(-any_of(varsSacar))

    # datJamSegm_df$street = arreglaCaracteres(datJamSegm_df$street,F,F)
    datJamSegm_df = datJamSegm_df %>% dplyr::select(-any_of(varsSacar))
    
    # datJam_resum$street = arreglaCaracteres(datJam_resum$street,F,F)
    
    print(paste0(Sys.time(),' :Final del proceso'))
    #########################################################
    ########### GUardo insumos shiny
    print(paste0(Sys.time(),' :Guarda bases que son insumo para shiny'))
    
    save(datJam_df,datJamSegm_df,segmentUnic, file = 'Shiny/Insumos/XXX_DatosShiny.RData')
    
    #### GUARDO COMO RDS
    # saveRDS(datJam_df,'Shiny/Insumos/datJam_df.RDS')
    # saveRDS(datJamSegm_df,'Shiny/Insumos/datJamSegm_df.RDS')
    # saveRDS(segmentUnic,'Shiny/Insumos/segmentUnic.RDS')
    ### Guardo la tabla de dias mes y anio
    
    save(dsma, file = 'Shiny/Insumos/XXX_dsma.RData')
    
    ### Guarda base para hacer el resumen de la base
    
    save(datJam_resum, file = 'Shiny/Insumos/XXX_datJam_resum.RData')
    
    #### Actualiaza datos cargados
    save(jamDatosShiny,file = 'BasesActualiza/jamDatosShiny.RData')
    
    print(paste0(Sys.time(),' :Final del proceso'))
    
    finTime = Sys.time()
    totalTime = difftime(finTime,iniTime,units = 'mins')
    
    print(paste0(Sys.time(),' :FINAL DE TODO EL PROCESO PARA LA BASE: (',nameBase,') Y DURO ',round(as.numeric(totalTime),1),' MINUTOS'))
    
    
  } else {
    print(paste0('La base de nombre: ',nameBase,' fue SEGMENTIZADA PREVIAMENTE'))
  }
  print(paste0('La base de nombre: ',nameBase,' INGRESADA'))
}



