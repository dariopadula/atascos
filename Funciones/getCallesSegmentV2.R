getCallesSegmentV2 = function(lineasDF,puntList,calleInt,bufferSize = 30) {
  
  ### Linea a segementar
  lineaSegm = lineasDF %>% filter(COD_NOMBRE == calleInt)
  ### Resto de las lineas (calles)
  lineaResto = lineasDF %>% filter(COD_NOMBRE != calleInt)
  
  
  
  ### Encuentra lospuntos de interceccion con el buffer de la linea original
  res = st_intersection(lineaSegm  %>% st_buffer(.,bufferSize) %>% select(geometry),
                        lineaResto) %>% st_cast(.,'POINT') %>% 
    suppressMessages() %>% suppressWarnings()
  
  if(nrow(res) > 0) {
  
    #### Grillla d epuntos de la calle a segmentar
    puntCalles = puntList[[as.character(calleInt)]]
    
    ##### Para cada punto de intersección encuentra el ID_calle mas cercano
    matchAux <- res %>% 
      st_join(puntCalles[,'ID_calle'], join=nngeo::st_nn, maxdist= Inf,k=1) %>% 
      st_set_geometry(NULL) %>% group_by(NOM_CALLE,COD_NOMBRE) %>% 
      summarise(ID_calle = round(mean(ID_calle))) %>% ungroup() %>%
      mutate(ID_calle = ifelse(duplicated(ID_calle),ID_calle + 1,ID_calle)) %>%
      rename('NOM_CALLE_ESQ' = 'NOM_CALLE','COD_NOMBRE_ESQ' = 'COD_NOMBRE') %>%
      suppressMessages()
    
    
    #### Se queda con los puntos que tiene intersecctiones y hace una windows para 
    ### encontrar la coordenada de inicio y la de fin, asi como el nombre y la calles
    resP = cargaCoords(puntCalles) %>% st_set_geometry(NULL) %>%
      inner_join(matchAux) %>% arrange(ID_calle) %>%
      mutate(Xf = lead(X),
             Yf = lead(Y),
             NOM_CALLE_ESQ_f = lead(NOM_CALLE_ESQ),
             COD_NOMBRE_ESQ_f = lead(COD_NOMBRE_ESQ),
             ID_segm = row_number()) %>% suppressMessages()
    
    ### Saco la ultima fila porque no hay segmento para hacer
    resP = resP[-nrow(resP),]
    
    #### Construyo la geometria
    resP[,'geometry'] = apply(resP[,c('X','Y','Xf','Yf')],1,
                              function(xx) {
                                res = paste0('LINESTRING (',paste(xx[c(1,2)], collapse = ' '),' , ',
                                             paste(xx[c(3,4)], collapse = ' '),')')
                                return(res)
                              })
    
    ### Lo convierto en un objeto sf
    # resP_sf <- sf::st_as_sf(resP, wkt = "geometry",crs = 32721) %>%
    #   st_transform(.,crs = 4326)
  } else {
    resP = list()
  }
  
  return(resP)
}
