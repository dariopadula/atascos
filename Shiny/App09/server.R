
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  

####################################
### Largo intervalo
  larInter  <- reactive({  
    aux = 2
    return(aux)
  })
  
#################################################
####### LISTA con tiempo de inicio y fin
intTiempo <- reactive({
  minHoraIni = input$horaIni*60 + input$minIni
  minHoraFin = input$horaFin*60 + input$minFin
  
  aux = list(minHoraIni = minHoraIni,minHoraFin = minHoraFin)
  
})   


#################################################
##### vactor con calle elegida

selCalle <- reactive({
  aux = input$varCalle
  aux
}) 


##################################################
##### Filtro los dias disponibles de dsma

diasIntercept <- reactive({
  ### Anio
  diasAnio = rownames(dsma)
  if(!('Todo' %in% input$varAnio  | is.null(input$varAnio))) {
    diasAnio = rownames(subset(dsma, anio %in% input$varAnio))
  }
  
  ### FIn de semana
  diasMes = rownames(dsma)
  if(!('Todo' %in% input$varMes  | is.null(input$varMes))) {
    diasMes = rownames(subset(dsma, mes %in% input$varMes))
  }
  
  ### FIn de semana
  diasFinde = rownames(dsma)
  if(!('Todo' %in% input$varFinde  | is.null(input$varFinde))) {
    diasFinde = rownames(subset(dsma, finDeSem %in% input$varFinde))
  }
  
  ### Dias de la semana
  diasDiaSem = rownames(dsma)
  if(!('Todo' %in% input$varDiaSem  | is.null(input$varDiaSem))) {
    diasDiaSem = rownames(subset(dsma, as.character(diaSem) %in% input$varDiaSem))
  }
  
  ### Dia calendario
  diasDiaEsp = rownames(dsma)
  if(!('Todo' %in% input$varDiaEsp  | is.null(input$varDiaEsp))) {
    diasDiaEsp = rownames(subset(dsma, as.character(diaStr) %in% input$varDiaEsp))
  }
  
  
  diasIntercept = Reduce(intersect, list(diasAnio,diasMes,diasFinde,diasDiaSem,diasDiaEsp))
  
  diasIntercept
})



##############################################
##############################################
#### Adapta filtros de los tiempos dinamicos


  observe({

    diasIntercept = diasIntercept()
    
    diaSemana = dsma[diasIntercept,] %>% arrange(levelDia) %>% 
      select(diaSem) %>% data.frame()
    
    choAnio<-c('Todo',unique(dsma[diasIntercept,'anio']))
    choMes<-c('Todo',unique(dsma[diasIntercept,'mes']))
    choFinde<-c('Todo',unique(dsma[diasIntercept,'finDeSem']))
    choDiaSem<-c('Todo',unique(diaSemana$diaSem))
    choDiaEsp<-c('Todo',unique(dsma[diasIntercept,'diaStr']))
    

    
    
    updateSelectInput(session,"varAnio",choices=choAnio,selected=input$varAnio)
    updateSelectInput(session,"varMes",choices=choMes,selected=input$varMes)
    updateSelectInput(session,"varFinde",choices=choFinde,selected=input$varFinde)
    updateSelectInput(session,"varDiaSem",choices=choDiaSem,selected=input$varDiaSem)
    updateSelectInput(session,"varDiaEsp",choices=choDiaEsp,selected=input$varDiaEsp)

})


###### Tabla de filtros para referencia
output$tablaFiltros = renderTable({
  
  anio = input$varAnio
  mes = input$varMes
  finde = input$varFinde
  diasSem = input$varDiaSem
  diasEsp = input$varDiaEsp
  Nivel = input$varNivel
  
  horaIni = input$horaIni
  horaFin = input$horaFin
  
  minIni = input$minIni
  minFin = input$minFin
  
  tini = ifelse(nchar(minIni) == 1,paste0(horaIni,':0',minIni),paste0(horaIni,':',minIni))
  tfin = ifelse(nchar(minFin) == 1,paste0(horaFin,':0',minFin),paste0(horaFin,':',minFin))
  
  anioP = paste(anio,collapse = '/')
  mesP = paste(mes,collapse = '/')
  diasSemP = paste(diasSem,collapse = '/')
  diasEspP = paste(diasEsp,collapse = '/')
  NivelP = paste(Nivel,collapse = '/')
  
  res = data.frame(Anio = anioP,Mes = mesP,finDeSem = finde,
                   diaSem = diasSemP,diaStr = diasEspP,Nivel = NivelP,
                   HoraIni = tini,HoraFin = tfin)
  
  colnames(res) = nomVarsDF_b[colnames(res),'nomShow']
  res
})


#### Tabla que resume la cantidad de dias considerados y el total de horas

output$tablaRangos = renderTable({
  
  # Rango horario
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  diasIntercept = diasIntercept()
  nDias = length(diasIntercept)
  
  horas = round(nDias*((minHoraFin - minHoraIni)/60),2)
  
  res = data.frame(nDias,horas)
  colnames(res) = c('# Días','Total Horas')
  
  res
  
})

##################################################
######### ARMA EL MAPA

##### Aplica filtros Jams

datFiltro = eventReactive(input$run, {
  
  # Rango horario
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  # Dias elegidos
  diasIntercept = diasIntercept()
  
  # Datos
  datFiltro = datJamSegm_df

  ### FIltro fechas todas
  IndFecha = length(diasIntercept) < nrow(dsma)
  if(IndFecha) {
    datFiltro = subset(datFiltro,diaStr %in% diasIntercept)
  }
  
  
  ### Calle
  if(!('Todo' %in% selCalle()  | is.null(selCalle()))) {
    datFiltro = subset(datFiltro, street %in% selCalle())
  }
  
  ### Nivel
  if(!('Todo' %in% input$varNivel  | is.null(input$varNivel))) {
    datFiltro = subset(datFiltro, as.character(level) %in% input$varNivel)
  }
  
  ### Filtro horario  
  IndDiff = (minHoraFin - minHoraIni) > 2
  if(IndDiff) {
    datFiltro = subset(datFiltro,(minutoHora >= minHoraIni & minutoHora <= minHoraFin))
  }
  
  return(datFiltro)
})




##### Aplica filtros Cortes

datCorFiltro = eventReactive(input$run, {
  
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  # Dias elegidos
  diasIntercept = diasIntercept()
  
  # Datos
  datFiltro = datCorSegm_df
  
  ### FIltro fechas todas
  IndFecha = length(diasIntercept) < nrow(dsma)
  if(IndFecha) {
    datFiltro = subset(datFiltro,diaStr %in% diasIntercept)
  }
  
  datFiltro = subset(datFiltro,
                     !((minutoLastDia < minHoraIni) |
                         (minutoIniDia > minHoraFin))) 
  
  largoInter = minHoraFin - minHoraIni
  
  datFiltro = datFiltro %>% mutate(refIni = ifelse(minutoIniDia <= minHoraIni,minHoraIni,minutoIniDia),
                                   refFin = ifelse(minutoLastDia >= minHoraFin,minHoraFin,minutoLastDia),
                                   Peso = (refFin - refIni)/largoInter)
  
  ### Calle
  # if(!('Todo' %in% selCalle()  | is.null(selCalle()))) {
  #   datFiltro = subset(datFiltro, street %in% selCalle())
  # }
  
  return(datFiltro)
})


##### Aplica filtros AutoScope

datAScopFiltro = eventReactive(input$run, {
  
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  # Dias elegidos
  diasIntercept = diasIntercept()
  
  # Datos
  datFiltro = datAScop_df
  
  ### FIltro fechas todas
  IndFecha = length(diasIntercept) < nrow(dsma)
  if(IndFecha) {
    datFiltro = subset(datFiltro,diaStr %in% diasIntercept)
  }
  
  datFiltro$posMin = datFiltro$minutoHora - minHoraIni
  datFiltro$posFin = minHoraFin - datFiltro$minutoHora
  
  datFiltro = datFiltro %>% filter(posMin >= - 4 & posFin >= - 4) %>%
    mutate(volume_aj = ifelse(posMin < 0,((5 + posMin)/5)*volume,
                              ifelse(posFin < 0,((5 + posFin)/5)*volume,volume)),
           IndCongest = ifelse(cod_nivel_servicio %in% c('E','F'),1,0))
  
  
  return(datFiltro)
})



##########################################
##### Agrega los datos

datAgg = eventReactive(input$run, {
  
  datFiltro = datFiltro()
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  minPorc = input$minPorc
  
  nDays = length(unique(as.character(datFiltro$diaStr)))
  larInter = larInter()
  nInter = ((minHoraFin - minHoraIni)/larInter + 1)*nDays
  
  
  
  ####### Datos para agregar
  varsGroup = c('ID_segmento','street','roadtype')
  # varsSum = c('delay','length','level','speed','speedkmh','minutoHora')
  
  
  datAgg = datFiltro %>% group_by_at(vars(dplyr::one_of(varsGroup))) %>%
    summarise(nJam = n(),
              porcTimeJamO = round(100*nJam/nInter,1),
              porcTimeJam = min(100,round(100*nJam/nInter,1)),
              delayMean = mean(delay,na.rm = T),
              levelMean = mean(level,na.rm = T),
              lengthMean = mean(length,na.rm = T),
              speedkmhMean = mean(speedkmh,na.rm = T),
              minutoHoraMean = mean(minutoHora,na.rm = T),
              direction = first(direction)) %>%
    ungroup() %>%
    filter(porcTimeJam > minPorc) %>%
    mutate(
      horaMedia = floor(minutoHoraMean/60),
      minMedio = round(60*(minutoHoraMean/60 - horaMedia)),
      timeStr = ifelse(nchar(minMedio) == 1,paste0(horaMedia,':0',minMedio),
                       paste0(horaMedia,':',minMedio)),
      content = paste(sep = "<br/>",
                      paste("<b><a>ID Segmentno</a></b>:",ID_segmento),
                      paste("<b><a>Sentido</a></b>:",direction),
                      paste("<b><a>Número de eventos de congestión</a></b>:",nJam),
                      paste("<b><a>% Tiempo congestión</a></b>:",porcTimeJam),
                      paste("<b><a>Demora media (segundos)</a></b>:",round(delayMean)),
                      paste("<b><a>Nivel de congestión medio</a></b>:",round(levelMean)),
                      paste("<b><a>Velocidad media en Kmh</a></b>:",round(speedkmhMean,2)),
                      paste("<b><a>Hora promedio</a></b>:",timeStr)),
      opacity = ifelse(porcTimeJam/100 < 0.1,0.1,porcTimeJam/100),
      weight = ifelse(porcTimeJam*0.1 < 0.3,0.3,porcTimeJam*0.1))  %>% 
    left_join(segmentUnic, by = 'ID_segmento') %>% st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>%
    st_transform(.,crs = 4326) 
  
  
  return(datAgg)
  
})


#############################
##### Agrega datos de cortes
datCorAgg = eventReactive(input$run, {
  
  datFiltro = datCorFiltro()
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  
  nDays = length(unique(as.character(datFiltro$diaStr)))

  ####### Datos para agregar
  varsGroup = c('ID_segmento','street','roadtype')

  datAgg = datFiltro %>% group_by_at(vars(dplyr::one_of(varsGroup))) %>%
    summarise(nCorte = n(),
              sumPeso = sum(Peso),
              porcTimeCorte = min(100,round(100*sumPeso/nDays,1)),
              lengthMean = mean(length,na.rm = T),
              DateMin = min(datecreated),
              DateMax = max(datemodified)) %>%
    ungroup() %>%
    mutate(content = paste(sep = "<br/>",
                      paste("<b><a>ID Segmentno</a></b>:",ID_segmento),
                      paste("<b><a>Total cortes</a></b>:",nCorte),
                      paste("<b><a>% Tiempo con corte</a></b>:",porcTimeCorte),
                      paste("<b><a>Fecha mínima</a></b>:",DateMin),
                      paste("<b><a>Fecha máxima</a></b>:",DateMax)),
      opacity = ifelse(porcTimeCorte/100 < 0.1,0.1,porcTimeCorte/100),
      weight = ifelse(porcTimeCorte*0.1 < 0.3,0.3,porcTimeCorte*0.05))  %>% 
    left_join(segmentUnicCor, by = 'ID_segmento') %>% st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>%
    st_transform(.,crs = 4326) 
  
  
  return(datAgg)
  
})


#############################
##### Agrega datos AutoScope
datAScopAgg = eventReactive(input$run, {
  
  datFiltro = datAScopFiltro()
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  

  ####### Datos para agregar
  varsGroup = c('ID_posSens')
  
  
  factorInterv = (minHoraFin - minHoraIni)/5
  
  datAgg =  datFiltro %>% group_by_at(vars(dplyr::one_of(varsGroup))) %>%
    summarise(count = n(),
              valume_mean = round(mean(volume_aj,na.rm = T)),
              valume_max = round(max(volume_aj,na.rm = T)),
              valume_min = round(min(volume_aj,na.rm = T)),
              valume_sd = round(sd(volume_aj,na.rm = T),1),
              valume_intTime = valume_mean*factorInterv,
              velocidad_mean = round(mean(spacemeanspeed,na.rm = T),1),
              PorcCongest = round(100*mean(IndCongest,na.rm = T),1)) %>% 
    ungroup() %>%
    left_join(puntAScoptUnic, by = 'ID_posSens') %>%
    mutate(avenida = dsc_avenida,
           desde = dsc_int_anterior,
           hacia = dsc_int_siguiente) %>%
    mutate(content = paste(sep = "<br/>",
                           paste("<b><a>Total reportes</a></b>:",count),
                           "<b><a>Sentido</a></b>:",
                           paste("<a>Calle</a>:",avenida),
                           paste("<a>Desde</a>:",desde),
                           paste("<a>Hacia</a>:",hacia),
                           "<em><b><a>Volumen</a></em></b></font>:",
                           paste("<a>Prom 5 minutos</a>:",valume_mean),
                           paste("<a>SD</a>:",valume_sd),
                           paste("<a>Máximo 5 minutos</a>:",valume_max),
                           paste("<a>Mínimo 5 minutos</a>:",valume_min),
                           paste("<a>Prom Intevalo</a>:",valume_intTime),
                           paste("<b><a>Velocidad Promedio</a></b>:",velocidad_mean),
                           paste("<b><a>% Reporte congestión</a></b>:",PorcCongest)))
    
    
    
  
  return(datAgg)
  
})


#########################################
#### HACE EL MAPA  
pp <- eventReactive(input$run, {
  
  datAgg = datAgg()
  datCorAgg = datCorAgg()
  datAScopAgg = datAScopAgg()
  
  usePalet = input$usePalet
  
  if(usePalet) {
    pal <- colorBin(
      bins = 9,
      palette = 'Reds',
      domain = seq(0,100,5))
  } else {
    pal <- colorBin(
      bins = 9,
      palette = 'Reds',
      domain = datAgg$porcTimeJam)
  }
  
  
  pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
    addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
    addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
    addPolylines(data = datAgg,
                 weight = ~weight,
                 color = ~pal(porcTimeJam),
                 fillColor = ~pal(porcTimeJam),
                 fillOpacity = ~opacity,
                 popup = ~content,
                 group = 'Congestion') %>%
    addLegend(position = 'topleft',pal = pal,values = datAgg$porcTimeJam,
              title = '% Tiempo con reporte',group = 'Congestion')
  
  
  capasAdd = NULL
  # if(addCortes) {
    
    palCor <- colorBin(
      bins = 9,
      palette = 'Blues',
      domain = datCorAgg$porcTimeCorte)
    
    pp = pp %>% 
      addPolylines(data = datCorAgg,
                   weight = ~weight,
                   color = ~palCor(porcTimeCorte),
                   fillColor = ~palCor(porcTimeCorte),
                   fillOpacity = ~opacity,
                   popup = ~content,
                   group = 'Corte') %>%
      addLegend(position = 'topleft',pal = palCor,values = datCorAgg$porcTimeCorte,
                title = '% Tiempo con Corte',group = 'Corte') %>%
      addCircleMarkers(data = datAScopAgg,
                       lng = ~longitud,lat = ~latitud,
                       radius = 3, 
                       opacity = 0.5, 
                       fillOpacity = 0.5, 
                       color = 'orange', 
                       fillColor = 1,
                       popup = ~content,
                       group = 'AutoScope') %>%
      addLegend(position = "topleft",labels = 'AutoScope', 
                colors = 'orange',
                title = NULL,
                opacity = 0.5,
                group = 'AutoScope')
      
    
    capasAdd = c(capasAdd,'AutoScope','Corte')
    
  # }
  
  ppp = pp
  
  1:length(listSHP) %>%
    purrr::walk(function(df) {
      ppp <<- ppp %>%
        addCircles(data = listSHP[[df]]$shp,
                   radius = 4, 
                   opacity = 0.5, 
                   fillOpacity = 0.5, 
                   color = listSHP[[df]]$color, 
                   fillColor = 0.5,
                   group = listSHP[[df]]$Group) %>%
        addLegend(position = "topleft",labels = listSHP[[df]]$Group, 
                  colors = listSHP[[df]]$color,
                  title = NULL,
                  opacity = 0.5,
                  group = listSHP[[df]]$Group)
    })
  
  
  gruposSHP = do.call(c,lapply(listSHP,function(xx) xx$Group))
  
  ppp = ppp %>%
    addLayersControl(
      baseGroups = c("CartoDB.Positron","OSM"),
      overlayGroups = c('Congestion',capasAdd,gruposSHP),
      options = layersControlOptions(collapsed = T)) %>%
    hideGroup(c(capasAdd,gruposSHP))
  
  ppp

})

output$distPlot <- renderLeaflet({
  pp()
})

###########################################################
###########################################################
######### DESCRIPTIVOS


###### Filtro para los descriptivos

dat_desc_Filtro = eventReactive(input$run, {
  
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  datFiltro = subset(datJam_df,(minutoHora >= minHoraIni & minutoHora <= minHoraFin))
  
  
  
  ### FIn de semana
  if(!('Todo' %in% input$varFinde  | is.null(input$varFinde))) {
    datFiltro = subset(datFiltro, finDeSem %in% input$varFinde)
  }
  
  ### Dia de semana
  if(!('Todo' %in% input$varDiaSem  | is.null(input$varDiaSem))) {
    datFiltro = subset(datFiltro, diaSem %in% input$varDiaSem)
  }
  
  ### Dia calendario
  if(!('Todo' %in% input$varDiaEsp  | is.null(input$varDiaEsp))) {
    datFiltro = subset(datFiltro, as.character(diaStr) %in% input$varDiaEsp)
  }
  
  ### Nivel
  if(!('Todo' %in% input$varNivel  | is.null(input$varNivel))) {
    datFiltro = subset(datFiltro, as.character(level) %in% input$varNivel)
  }
  
  return(datFiltro)
})


##### Tabla descriptivos
dat_desc_Agg = eventReactive(input$run, {
  
  datFiltro = dat_desc_Filtro()
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  nDays = length(unique(as.character(datFiltro$diaStr)))
  larInter = larInter()
  nInter = ((minHoraFin - minHoraIni)/larInter + 1)*nDays
  
  
  
  ####### Datos para agregar
  varsGroup = c('street','roadtype')
  # varsSum = c('delay','length','level','speed','speedkmh','minutoHora')
  
  
  summar = datFiltro %>% group_by(street) %>%
    summarise(count = n(),
              delayMean = round(mean(delay,na.rm = T)),
              levelMean = round(mean(level,na.rm = T),1),
              level345 = round(100*sum(level %in% c(3,4,5))/nInter),
              lengthMean = round(mean(length,na.rm = T)),
              speedkmhMean = round(mean(speedkmh,na.rm = T)),
              minutoHoraMean = mean(minutoHora,na.rm = T)) %>%
    ungroup() %>%
    mutate(horaMedia = floor(minutoHoraMean/60),
           minMedio = round(60*(minutoHoraMean/60 - horaMedia)),
           timeStr = ifelse(nchar(minMedio) == 1,paste0(horaMedia,':0',minMedio),
                            paste0(horaMedia,':',minMedio))) %>%
    dplyr::select(-horaMedia,-minMedio,-minutoHoraMean) %>%
    arrange(desc(count))
  
  
  
  return(summar)
  
})


output$descTable = DT::renderDataTable({
  aux = dat_desc_Agg()
  
  colnames(aux) = nomVarsDF_b[colnames(aux),'nomShow']

  return(aux)
  
})


#################################################
#################################################
########## HEAT MAP


##################################################
##### Filtro los dias disponibles de dsma

diasInterceptHeat <- reactive({
  ### Anio
  diasAnio = rownames(dsma)
  if(!('Todo' %in% input$varAnioHeat  | is.null(input$varAnioHeat))) {
    diasAnio = rownames(subset(dsma, anio %in% input$varAnioHeat))
  }
  
  ### FIn de semana
  diasMes = rownames(dsma)
  if(!('Todo' %in% input$varMesHeat  | is.null(input$varMesHeat))) {
    diasMes = rownames(subset(dsma, mes %in% input$varMesHeat))
  }
  
  ### FIn de semana
  diasFinde = rownames(dsma)
  if(!('Todo' %in% input$varFindeHeat  | is.null(input$varFindeHeat))) {
    diasFinde = rownames(subset(dsma, finDeSem %in% input$varFindeHeat))
  }
  
  ### Dias de la semana
  diasDiaSem = rownames(dsma)
  if(!('Todo' %in% input$varDiaSemHeat  | is.null(input$varDiaSemHeat))) {
    diasDiaSem = rownames(subset(dsma, as.character(diaSem) %in% input$varDiaSemHeat))
  }
  
  ### Dia calendario
  diasDiaEsp = rownames(dsma)
  if(!('Todo' %in% input$varDiaEspHeat  | is.null(input$varDiaEspHeat))) {
    diasDiaEsp = rownames(subset(dsma, as.character(diaStr) %in% input$varDiaEspHeat))
  }
  
  
  diasInterceptHeat = Reduce(intersect, list(diasAnio,diasMes,diasFinde,diasDiaSem,diasDiaEsp))
  
  diasInterceptHeat
})



##############################################
##############################################
#### Adapta filtros de los tiempos dinamicos


observe({
  
  diasIntercept = diasInterceptHeat()
  
  diaSemana = dsma[diasIntercept,] %>% arrange(levelDia) %>% 
    select(diaSem) %>% data.frame()
  
  
  choAnio<-c('Todo',unique(dsma[diasIntercept,'anio']))
  choMes<-c('Todo',unique(dsma[diasIntercept,'mes']))
  choFinde<-c('Todo',unique(dsma[diasIntercept,'finDeSem']))
  choDiaSem<-c('Todo',unique(diaSemana$diaSem))
  choDiaEsp<-c('Todo',unique(dsma[diasIntercept,'diaStr']))
  
  
  updateSelectInput(session,"varAnioHeat",choices=choAnio,selected=input$varAnioHeat)
  updateSelectInput(session,"varMesHeat",choices=choMes,selected=input$varMesHeat)
  updateSelectInput(session,"varFindeHeat",choices=choFinde,selected=input$varFindeHeat)
  updateSelectInput(session,"varDiaSemHeat",choices=choDiaSem,selected=input$varDiaSemHeat)
  updateSelectInput(session,"varDiaEspHeat",choices=choDiaEsp,selected=input$varDiaEspHeat)
  
})



##### Aplica filtros

datFiltro_heat = eventReactive(input$run2, {
  
  # Dias elegidos
  diasInterceptHeat = diasInterceptHeat()
  
  # Datos
  datFiltro = datJam_df
  
  ### FIltro fechas todas
  IndFecha = length(diasInterceptHeat) < nrow(dsma)
  if(IndFecha) {
    datFiltro = subset(datFiltro,diaStr %in% diasInterceptHeat)
  }

  ### Nivel
  if(!('Todo' %in% input$varNivelHeat  | is.null(input$varNivelHeat))) {
    datFiltro = subset(datFiltro, as.character(level) %in% input$varNivelHeat)
  }
  
  return(datFiltro)
})




datFull = eventReactive(input$run2, {

  varArrang = nomVarsDF_s[input$varArrang,'nomBase']
  largoInt = input$largoInt
  
  datosHeat = datFiltro_heat()
  
  datFull = datosHeat %>% mutate(interv = ceiling(minutoHora/largoInt)) %>% 
    group_by(street) %>%
    summarise(count = n(),
              delayMean = round(mean(delay,na.rm = T)),
              levelMean = mean(level,na.rm = T),
              level345 = sum(level %in% c(3,4,5)),
              lengthMean = round(mean(length,na.rm = T))) %>%
    ungroup() %>%
    arrange(desc(across(all_of(varArrang)))) %>% 
    mutate(acumulado = cumsum(count),
           porcCum = round(100*acumulado/sum(count),1),
           porcCount = round(100*count/sum(count),2),
           IDpos = row_number())

  return(datFull)

})


dataPlot = eventReactive(input$run2, {
  
  largoInt = input$largoInt
  nCalles = input$nCalles
  datFull = head(datFull(),nCalles)
  
  datosHeat = datFiltro_heat()
  

  interv = unique(ceiling(1:(23*60 + 60)/largoInt))
  
  tiempoHora = floor(interv*largoInt/60)
  tiempoMin = 60*(interv*largoInt/60 - tiempoHora)
  tiempo = ifelse(nchar(tiempoMin) == 1,paste0(tiempoHora,':0',tiempoMin),
                  paste0(tiempoHora,':',floor(tiempoMin)))
  
  
  calles = unique(datFull$street)
  ncalles = length(calles)
  
  dfInterv = data.frame(interv = rep(interv,ncalles),
                        street = rep(calles,each = length(interv)),
                        hora = rep(tiempo,ncalles))
  
  
  datFilter = subset(datosHeat,street %in% datFull$street)
  
  dataAux = datFilter %>% 
    mutate(interv = ceiling(minutoHora/largoInt)) %>% 
    group_by(street,interv) %>%
    summarise(count = n(),
              delayMean = round(mean(delay,na.rm = T)),
              levelMean = mean(level,na.rm = T),
              level345 = sum(level %in% c(3,4,5)),
              lengthMean = round(mean(length,na.rm = T))) %>%
    ungroup() %>%
    right_join(dfInterv,by = c('street','interv')) %>%
    replace(is.na(.), 0) %>%
    mutate(streetF = factor(street,levels = datFull$street),
           hora = factor(hora,levels = tiempo))
  
  
  return(dataAux)

})


##### Plot Heat


heatPlot = eventReactive(input$run2, {

  dataPlot = data.frame(dataPlot())
  varFill = nomVarsDF_s[input$varFill,'nomBase']
  
  
  dataPlot$varFill = dataPlot[,varFill]
  
  
  
  p <-ggplot(dataPlot,aes(hora,streetF,fill=varFill))+
    geom_tile(aes(text = sprintf(paste("<b><a>Calle</a></b>: %s",
                                       "<b><a>Hora</a></b>: %s",
                                       "<b><a>Número de eventos de congestión</a></b>: %s",
                                       "<b><a>Demora media (segundos)</a></b>: %s", 
                                       "<b><a>Total eventos con nivel 3 o +</a></b>: %s", 
                                        "<b><a>Largo Medio</a></b>: %s",sep = '\n'),
                                 streetF,hora,count,delayMean,level345,lengthMean))) + 
    scale_fill_gradient(input$varFill,low = "white",
                        high = "red") + 
    theme_minimal()
  
  
  pp <-p +
    theme(plot.title=element_text(size = 14))+
    theme(axis.text.y=element_text(size=6)) +
    theme(axis.title.y=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(axis.text.x=element_text(angle=90,size = 6)) +
    theme(strip.background = element_rect(colour="white"))+
    theme(axis.text=element_text(size=7))  + 
    theme(legend.position = "bottom")
  
  #ggplotly(p)
  
  return(pp)
})


###### Por Porcentaje

porcPlot = eventReactive(input$run2, {
  datFull = datFull()
  nCalles = input$nCalles
  
  textAdd = sprintf(paste('Total de eventos: %s',
                          'Total de eventos filtrados: %s',
                          'Porcentaje del total: %s',
                          sep = '\n'),
                    nrow(datJam_df),sum(datFull$count),round(100*sum(datFull$count)/nrow(datJam_df),1))
  
  pp = ggplot(data = datFull, aes(x = IDpos,y = porcCount))  + 
    theme_minimal() + 
    geom_point(aes(text = sprintf(paste("<b><a>Calle</a></b>: %s",
                                        "<b><a>Posición</a></b>: %s",
                                        "<b><a>Porcentaje</a></b>: %s",
                                        "<b><a>Porcentaje Acumulado</a></b>: %s",
                                        sep = '\n'),street,IDpos,porcCount,porcCum)), 
               colour = 'gray',alpha = 0.5) + ylab('% eventos') + xlab('Ranking de calles') +
    geom_point(data = datFull %>% filter(IDpos == nCalles),
               aes(text = sprintf(paste("<b><a>Calle</a></b>: %s",
                                        "<b><a>Posición</a></b>: %s",
                                        "<b><a>Porcentaje</a></b>: %s",
                                        "<b><a>Porcentaje Acumulado</a></b>: %s",
                                                    sep = '\n'),street,IDpos,porcCount,porcCum)), 
               colour = 'blue',alpha = 1) + 
    geom_text(label = textAdd,x = mean(datFull$IDpos),y = max(datFull$porcCount)-1,
              hjust = 0,vjust = 1,size = 4)
  # annotate(text = textAdd,x = Inf,y = 0,hjust = 1,vjust = 0)
  
return(pp)
})

output$heatPlot <- renderPlotly({
  ggplotly(heatPlot(),tooltip = c("text"))
})
  
output$porcPlot <- renderPlotly({
  ggplotly(porcPlot(),tooltip = c("text"))
})


output$textPeriodo <- renderPrint({
  diaFirst = dsma[1,'diaStr']
  diaLast = dsma[nrow(dsma),'diaStr']
  
  res = HTML(paste0('<p>La información esta disponible desde el ',"<b>",diaFirst,' al ',diaLast,"</b></p>"))

  res
  
})


#### Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste('DescApp_',Sys.time(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(dat_desc_Agg()[1:min(input$nFilas,nrow(dat_desc_Agg())),], file, row.names = FALSE)
  }
)


}) ### CIERRA SERVER



