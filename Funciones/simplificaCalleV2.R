###################################################
####### FUNCION PARA GENERAR CALLES con estructura mas simple y ordenada

simplificaCalleV2 = function(auxCalle,cellsizeMax = 500,npart = 10,distSegm = 15,
                           varsKeep = c('NOM_CALLE','COD_NOMBRE')) {
  
  #### Determina la caja que contiene el shape
  box = st_bbox(auxCalle)
  box_df = data.frame(X = rep(c(box$xmin,box$xmax),2),Y = rep(c(box$ymin,box$ymax),each = 2)) %>% 
    st_as_sf(.,coords = c('X','Y'),crs = 32721)
  
  # 1 = OESTE/SUR
  # 2 = ESTE/SUR
  # 3 = OESTE/NORTE
  # 4 = ESTE/NORTE
  
  dfRef = data.frame(pbox = 1:4,
                     xRef = c('OESTE','ESTE','OESTE','ESTE'),
                     yRef = c('SUR','SUR','NORTE','NORTE'))
  
  ### Se queda con el nombre y el codigo de la calle
  datCalle = auxCalle %>% st_set_geometry(NULL) %>% dplyr::select(one_of(varsKeep)) %>% unique() %>%
    suppressMessages()
  
  ### Pasa la calle a un conjunto de puntos
  puntos = st_cast(auxCalle,'POINT') %>% suppressWarnings()
  
  ### Encuentra los puntos extremos para tomar como inicio y luego un posible fin
  distBox = st_distance(puntos,box_df)
  posInicio = which.max(rowSums(distBox == min(distBox)))[1]
  orintacion = dfRef[which.min(distBox[posInicio,]),]
  pinicio = puntos[posInicio,]
  # Para el punto de inicio, me quedo con el punto mas lejano
  distIni = st_distance(puntos,pinicio)
  posFin = which.max(distIni)
  pfin = puntos[posFin,]
  
  #### Arma la grilla
  # Determina el largo del cuadrado
  cellsize = min(as.numeric(max(st_distance(box_df,box_df)))/npart,cellsizeMax)
  
  ind = F
  while(!ind) {
    gridd = st_make_grid(
      x = auxCalle,
      cellsize = cellsize)
    
    cellsize = cellsize/2
    ind = length(gridd) > 25
  }
  
  
  

  
  # Transforma la grilla en sf
  gridd = st_as_sf(data.frame(gridd),sf_column_name = 'geometry')
  ##############################
  
  ### Captura las coordenadas
  coord = st_coordinates(puntos)
  #### Las pega al sf de puntos
  puntos = cbind(puntos,coord)
  #### Calcula el punto promedio dentro de cada cuadrante de la grilla
  puntosMedio = aggregate(puntos[,c('X','Y')], gridd, mean, 
                          join = function(x, y) st_is_within_distance(x, y, dist = 0))
  
  
  #### Saca los cuadrantes donde no hay puntos y construye un sf de puntos con los puntos promedios
  res = puntosMedio %>% filter(!is.na(X)) %>% st_set_geometry(NULL) %>%
    st_as_sf(.,coords = c('X','Y'),crs = 32721)
  
  
 
  ## Agrego a res los puntos de inicio y fin, simpre y cuando ya no esten en res
  IndIni = as.numeric(min(st_distance(pinicio,res))) > 0
  IndFin = as.numeric(min(st_distance(pfin,res))) > 0 
  
  ## Agrego los puntos de inicio y fin
  if(IndIni) res = rbind(pinicio[,colnames(res)],res)
  if(IndFin) res = rbind(res,pfin[,colnames(res)])
  ##### Ordena los puntos desde el primero haciendo la secuencia con los puntos más cercanos  
  dist = st_distance(res,res)
  diag(dist) = Inf
  
  pos = 1
  if(!IndIni)  pos = which.max(rowSums(st_distance(res,box_df) == min(st_distance(res,box_df))))[1]
  
  permut = pos
  dist[,pos] = Inf
  
  cont = 1
  while(cont < nrow(dist)) {
    pos = which.min(dist[pos,])
    permut = c(permut,pos)
    dist[,pos] = Inf
    cont = cont + 1
  }
  
  ### Ordena los puntos aplicando la permutacion
  res_ord = res[permut,]
  
  ############################
  ############################
  linea = res_ord %>% 
    group_by() %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING") %>% st_as_sf(.,sf_column_name = 'geometry',crs = 32721)
  
  
  linea = cbind(linea,datCalle)
  
  # plot(gridd)
  # plot(puntos,add = T)
  # plot(res,add = T,col = 'red')
  # plot(st_geometry(linea),add = T)
  ## Segmentiza la linea
  
  df = data.frame(st_line_sample(linea, density = 1/distSegm) %>% st_cast("POINT")) %>% 
    st_as_sf(.,sf_column_name = 'geometry',crs = 32721)
  
  if(nrow(df) <= 1) {
    df = res_ord
  } else {
  df = rbind(res_ord[1,],df,res_ord[nrow(res_ord),])
  }
  

  df = df %>% mutate(ID_calle = row_number())
  
  ## Arregla la oriencacion
  Var = prop.table(apply(st_coordinates(df),2,var))
  
  revert = F
  

  if((Var['X'] < Var['Y'])) {
    dirIni = 'NORTE'
    dirFin = 'SUR'
    if(orintacion['yRef'] != 'NORTE') revert = T
  }
  

  if((Var['X'] >= Var['Y'])) {
    dirIni = 'ESTE'
    dirFin = 'OESTE'
    if(orintacion['xRef'] != 'ESTE') revert = T
  }
  
  ##### REVIERTO EL ORDEN EN CASO QUE HAYA QUE HACERLO
  if(revert) {
    df = df %>% mutate(ID_calle = (nrow(df) + 1) - ID_calle) %>% arrange(ID_calle)
  }
  
  #############################################
  df = cbind(df,datCalle,dirIni,dirFin)
  
  return(list(linea,df))
}


# plot(st_geometry(df))
# plot(st_geometry(pinicio),add = T,col = 'red')
# plot(st_geometry(pfin),add = T,col = 'green')
