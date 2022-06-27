# Define server logic       
server <- function(input, output) {
  
  
  segmSelect = eventReactive(input$run, {
    
    
    datFiltro = hhS
    
    ### Dia de la semana
    if(!('Todo' %in% input$clustName  | is.null(input$clustName))) {
      datFiltro = subset(datFiltro, cluster %in% input$clustName)
    }
    
    ### Dia de la semana
    if(!('Todo' %in% input$varDiaSem  | is.null(input$varDiaSem))) {
      datFiltro = subset(datFiltro, diaSem %in% input$varDiaSem)
    }
    
    ### Hora 
    if(!('Todo' %in% input$horaSel  | is.null(input$horaSel))) {
      datFiltro = subset(datFiltro, hora %in% as.integer(input$horaSel))
    }
    
    
    
    subSegm = datFiltro %>% 
      group_by(ID_segmento) %>%
      summarize(n = n(),
                N34medio_All = mean(N34medio_All),
                N4medio_All = mean(N4medio_All)) %>%
      ungroup()
    
    
    # segmSelect = segmentUnic %>% filter(ID_segmento %in% subSegm$ID_segmento) %>% st_transform(x = .,crs = 4326)
    segmSelect = segmentUnic %>% inner_join(subSegm) %>% st_transform(x = .,crs = 4326)
    
    segmSelect
    
  })
  
  
  
  
  
  
  output$map <- renderLeaflet({
    
    
    segmSelect = segmSelect()
    
    pal <- colorBin(
      bins = 9,
      palette = 'Reds',
      domain = segmSelect$N34medio_All)
    
    pp = leaflet() %>% # ABRE LA VENTANA PARA HACER EL MAPA
      addTiles(group = "OSM") %>% # DEFINE UN FONDO (POR DEFECTO OSM)
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'CartoDB.Positron') %>%
      addPolylines(data = segmSelect,
                   color =  ~pal(N34medio_All),
                   weight = 4,
                   layerId = ~ID_segmento) %>%
      addLegend(position = 'topleft',pal = pal,values = segmSelect$N34medio_All,
                title = 'Nivel 3 y 4 promedio',group = 'Congestion')
    
    
    if(!is.null(input$map_shape_click)) {
      
      centroide = segmSelect %>% filter(ID_segmento == input$map_shape_click$id) %>%
        st_centroid() %>% cargaCoords(.)
      
      sementoEleg = segmSelect %>% filter(ID_segmento == input$map_shape_click$id)
      ###################################################
      ########## Arma buffer
      
      if(input$buffer) {
        
        buffer = sementoEleg %>% st_transform(.,crs = 32721) %>%
          st_buffer(input$tamBuffer)
        
      } else {
        buffer = NULL
      }
      
      ####################################################
      ####################################################
      pp = pp %>% addPolylines(data = sementoEleg,
                               color = 'green',
                               weight = 6,
                               layerId = ~ID_segmento) %>%
        setView(lng=centroide$X, lat=centroide$Y, zoom=15) 
      
      if(!is.null(buffer)) {
        
        segmInBuffer = segmentUnic %>% st_join(buffer,left = FALSE)
        
        pp = pp  %>% addPolygons(data = buffer %>% 
                                   st_transform(.,crs = 4326),fillOpacity = 0,weight = 1) %>%
          addPolylines(data = segmInBuffer %>% 
                         st_transform(.,crs = 4326),
                       weight = 2,fillOpacity = 0.5)
      }
      
    }
    
    pp
    
  })
  
  
  ######################################
  ##### Captura varios eventos
  
  # click on polygon
  observe({ 
    
    event <- input$map_shape_click
    
    
    if(!is.null(event)) {
      message <- hhS %>% filter(ID_segmento == event$id) %>% 
        arrange(diaSem,hora)
      
      Zona = data.frame(ID_segmento = character(),
                        Calle = character(),
                        Desde = character(),
                        Hasta = character(), 
                        TotalEventos = integer())
      
      Zona[1,'ID_segmento'] = unique(message$ID_segmento)
      Zona[1,'Calle'] = unique(vv[vv$COD_NOMBRE == message$COD_NOMBRE_REF,"NOM_CALLE"])[1]
      Zona[1,'Desde'] = unique(vv[vv$COD_NOMBRE == message$ESQ_REF,"NOM_CALLE"])[1]
      Zona[1,'Hasta'] = unique(vv[vv$COD_NOMBRE == message$ESQ_END_REF,"NOM_CALLE"])[1]
      Zona[1,'TotalEventos'] = unique(message$nEventosAll)
      
      
      message <- message %>% dplyr::select(-c(COD_NOMBRE_REF:ESQ_END_REF,finDeSem,nEventosAll,ID_segmento)) %>%
        mutate(across(c(nEventosMedio:porcDias),round)) %>% dplyr::select(-c(N4Sum,N34Sum,nEventosMedio_All,difN34))
      
      
      
      
      
      ####################################################
      ### Datos serie
      aa =  pruebaSS %>% filter(ID_segmento == event$id)
      # aa =  pruebaSS %>% filter(ID_segmento == event$id & !is.na(ordenSev))
      
      grillaDiaHora = dsma  %>% mutate(nhoras = 24) %>% uncount(nhoras) %>%
        group_by(diaStr) %>%
        mutate(hora = row_number() - 1) %>%
        ungroup() %>%
        arrange(diaStr,hora)
      
      
      putNa0 = function(x) {
        x[is.na(x)] = 0
        return(x)
      }
      
      data = grillaDiaHora %>% 
        left_join(aa %>% dplyr::select(diaStr,hora,n,N34,porcDH,porcDH_N34,ordenSev), by = c("diaStr","hora")) %>%
        mutate(across(c('n','N34','porcDH','porcDH_N34'),putNa0))
      
      
      
      data = data %>% mutate(date = ifelse(hora < 10,paste0(diaStr,' 0',hora,':00:00'),paste0(diaStr,' ',hora,':00:00')),
                             date = as.POSIXct(strptime(gsub('T',' ',date), "%Y-%m-%d %H:%M:%S")),
                             value = porcDH_N34,
                             diaSem = factor(diaSem,levels = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')))
      
      p <- data %>% #filter(finDeSem == 'Lunes a viernes' & diaSem == 'viernes') %>%
        ggplot( aes(x=date, y=value)) +
        # geom_area(fill="#69b3a2", alpha=0.5) +
        # geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2",size = 0.1) +
        geom_point(aes(color = diaSem),size = 1) + 
        ylab("% tiempo") +
        ylim(0,120) +
        theme_ipsum()
      
      
      # p <- data %>% #filter(finDeSem == 'Lunes a viernes' & diaSem == 'viernes') %>%
      #   ggplot( aes(x=date, y=value)) +
      #   # geom_area(fill="#69b3a2", alpha=0.5) +
      #   # geom_area(fill="#69b3a2", alpha=0.5) +
      #   geom_line(color="#69b3a2",size = 0.1) +
      #   # geom_point(aes(shape = diaSem)) + 
      #   geom_point(aes(color = 15-ordenSev)) + 
      #   ylab("% tiempo") +
      #   ylim(0,120) +
      #   theme_ipsum()
      
      # Turn it interactive with ggplotly
      p <- ggplotly(p)
      ####################################################
      
    } else {
      message = NULL
      Zona = NULL
      p = NULL
    }
    # message <- event$id
    
    output$widgets <- renderDataTable(message)
    
    output$Zona <- renderTable(Zona)
    
    output$plotSerie <- renderPlotly(p)
    
    output$table <- renderFormattable({formattable(clusterInfo, list(`N34medio_All` = color_tile("white", "red"),
                                                                     `N4medio_All` = color_tile("white", "red"),
                                                                     `porcDias` = color_tile("white", "red"),
                                                                     `N4medio` = color_tile("white", "red"),
                                                                     `N34medio` = color_tile("white", "red"),
                                                                     `n` = color_bar("lightblue")))})
    
  })
  # click on a marker
  
}