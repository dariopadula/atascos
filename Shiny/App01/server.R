
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

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
######### ARMA EL MAPA

##### Aplica filtros
datFiltro = eventReactive(input$run, {

  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  datFiltro = subset(datJamSegm_df,(minutoHora >= minHoraIni & minutoHora <= minHoraFin))
  
  
  ### Calle
  if(!("ALL" %in% selCalle()  | is.null(selCalle()))) {
    datFiltro = subset(datFiltro, street %in% selCalle())
  }
  
  ### FIn de semana
  if(!("ALL" %in% input$varFinde  | is.null(input$varFinde))) {
    datFiltro = subset(datFiltro, finDeSem %in% input$varFinde)
  }
  
  ### Dia de semana
  if(!("ALL" %in% input$varDiaSem  | is.null(input$varDiaSem))) {
    datFiltro = subset(datFiltro, diaSem %in% input$varDiaSem)
  }
  
  ### Dia calendario
  if(!("ALL" %in% input$varDiaEsp  | is.null(input$varDiaEsp))) {
    datFiltro = subset(datFiltro, as.character(diaStr) %in% input$varDiaEsp)
  }
  
  ### Nivel
  if(!("ALL" %in% input$varNivel  | is.null(input$varNivel))) {
    datFiltro = subset(datFiltro, as.character(level) %in% input$varNivel)
  }
  
  return(datFiltro)
})

##########################################
##### Agrega los datos

datAgg = eventReactive(input$run, {
  
  datFiltro = datFiltro()
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
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
              minutoHoraMean = mean(minutoHora,na.rm = T)) %>%
    ungroup() %>%
    mutate(
      horaMedia = floor(minutoHoraMean/60),
      minMedio = round(60*(minutoHoraMean/60 - horaMedia)),
      timeStr = ifelse(nchar(minMedio) == 1,paste0(horaMedia,':0',minMedio),
                       paste0(horaMedia,':',minMedio)),
      content = paste(sep = "<br/>",
                      paste("<b><a href='http://www.samurainoodle.com'>ID Segmentno</a></b>:",ID_segmento),
                      paste("<b><a href='http://www.samurainoodle.com'>Total</a></b>:",nJam),
                      paste("<b><a href='http://www.samurainoodle.com'>% Tiempo</a></b>:",porcTimeJam),
                      paste("<b><a href='http://www.samurainoodle.com'>Delay</a></b>:",round(delayMean)),
                      paste("<b><a href='http://www.samurainoodle.com'>Level</a></b>:",round(levelMean)),
                      paste("<b><a href='http://www.samurainoodle.com'>Speed Kmh</a></b>:",round(speedkmhMean,2)),
                      paste("<b><a href='http://www.samurainoodle.com'>Hora promedio</a></b>:",timeStr)),
      opacity = porcTimeJam/100)  %>% 
    left_join(segmentUnic, by = 'ID_segmento') %>% st_as_sf(.,sf_column_name = 'geometry',crs = 32721) %>%
    st_transform(.,crs = 4326) 
  
  
  return(datAgg)
  
})

#########################################
#### HACE EL MAPA  
pp <- eventReactive(input$run, {
  
  datAgg = datAgg()
  
  pal <- colorBin(
    bins = 9,
    palette = 'Reds',
    domain = seq(0,100,5))
  
  
  pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
    addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
    addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
    addPolylines(data = datAgg,
                 weight = ~porcTimeJam*0.1,
                 color = ~pal(porcTimeJam),
                 fillColor = ~pal(porcTimeJam),
                 fillOpacity = ~opacity,
                 popup = ~content) %>%
    addLegend(position = 'topleft',pal = pal,values = datAgg$porcTimeJam,
              title = '% Tiempo con reporte',group = 'Flujo')
  
  
  pp = pp %>%
    addLayersControl(
      baseGroups = c("CartoDB.Positron","OSM"),
      # overlayGroups = overlayGroups,
      options = layersControlOptions(collapsed = T)) %>%
    hideGroup(NULL)
  
  pp

})

output$distPlot <- renderLeaflet({
  pp()
})


###########################################################
######### DESCRIPTIVOS


###### Filtro para los descriptivos

dat_desc_Filtro = eventReactive(input$run, {
  
  minHoraIni = intTiempo()[['minHoraIni']]
  minHoraFin = intTiempo()[['minHoraFin']]
  
  datFiltro = subset(datJam_df,(minutoHora >= minHoraIni & minutoHora <= minHoraFin))
  
  
  
  ### FIn de semana
  if(!("ALL" %in% input$varFinde  | is.null(input$varFinde))) {
    datFiltro = subset(datFiltro, finDeSem %in% input$varFinde)
  }
  
  ### Dia de semana
  if(!("ALL" %in% input$varDiaSem  | is.null(input$varDiaSem))) {
    datFiltro = subset(datFiltro, diaSem %in% input$varDiaSem)
  }
  
  ### Dia calendario
  if(!("ALL" %in% input$varDiaEsp  | is.null(input$varDiaEsp))) {
    datFiltro = subset(datFiltro, as.character(diaStr) %in% input$varDiaEsp)
  }
  
  ### Nivel
  if(!("ALL" %in% input$varNivel  | is.null(input$varNivel))) {
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
              level45 = round(100*sum(level %in% c(4,5))/nInter),
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
  dat_desc_Agg()
})


#################################################
#################################################
########## HEAT MAP

##### Aplica filtros
datFiltro_heat = eventReactive(input$run2, {
  
  datFiltro = datJam_df
  
  
  ### FIn de semana
  if(!("ALL" %in% input$varFindeHeat  | is.null(input$varFindeHeat))) {
    datFiltro = subset(datFiltro, finDeSem %in% input$varFindeHeat)
  }
  
  ### Dia de semana
  if(!("ALL" %in% input$varDiaSemHeat  | is.null(input$varDiaSemHeat))) {
    datFiltro = subset(datFiltro, diaSem %in% input$varDiaSemHeat)
  }
  
  ### Dia calendario
  if(!("ALL" %in% input$varDiaEspHeat  | is.null(input$varDiaEspHeat))) {
    datFiltro = subset(datFiltro, as.character(diaStr) %in% input$varDiaEspHeat)
  }
  
  ### Nivel
  if(!("ALL" %in% input$varNivelHeat  | is.null(input$varNivelHeat))) {
    datFiltro = subset(datFiltro, as.character(level) %in% input$varNivelHeat)
  }
  
  return(datFiltro)
})


datFull = eventReactive(input$run2, {

  varArrang = input$varArrang
  nCalles = input$nCalles
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
    arrange(desc(across(all_of(varArrang)))) %>% head(nCalles) 
  
  return(datFull)

})


dataPlot = eventReactive(input$run2, {
  
  largoInt = input$largoInt
  datFull = datFull()
  
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
##### Plot


heatPlot = eventReactive(input$run2, {

  dataPlot = data.frame(dataPlot())
  varFill = input$varFill
  
  
  dataPlot$varFill = dataPlot[,varFill]
  
  p <-ggplot(dataPlot,aes(hora,streetF,fill=varFill))+
    geom_tile() + 
    scale_fill_gradient(varFill,low = "white",
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

# output$heatPlot <- renderPlot({
#   heatPlot()
# })


output$heatPlot <- renderPlotly({
  ggplotly(heatPlot()) #%>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
})
  
}) ### CIERRA SERVER



