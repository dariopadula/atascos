colapsaPuntosCercanos = function(datPuntos,kvecino,maxdist) {
  
  ## REtorna una lista con hasta K vecinos mas cercanos de los  que 
  # estan a menos de maxdist metros  
  res = st_nn(
    x = datPuntos,
    y = datPuntos,
    sparse = TRUE,
    k = kvecino,
    maxdist = maxdist,
    returnDist = T,
    progress = F,
    parallel = 1
  )
  
  
  #### Genera un data frame con la posicion del punto y los vecinas mas cercanos que encontro
  
  resnn = do.call(rbind,lapply(res$nn,function(xx) {
    aux = data.frame(ID_row = xx[1],ID_pos = xx)
    return(aux)
  }) )
  
  #### Retorna la posicion con las distancia de los vecinos mas cercanos  
  resdd = do.call(rbind,lapply(res$dist,function(xx) {
    aux = data.frame(dist = xx)
    return(aux)
  }) )
  
  #### Junta todo en un solo data frame   
  resnnd = data.frame(resnn,resdd) %>% filter(dist < maxdist)
  resnndF = resnnd[!duplicated(resnnd$ID_pos),]
  
  #### Detecta puntos que fueron asigado a otros y que ademas son referentes de otros puntos   
  nn = resnndF %>% group_by(ID_row) %>% 
    summarise(n = n(),
              dmin = min(dist),
              pp = first(ID_pos)) %>% filter(dmin > 0)
  
  
  #### Arregla esto haciendo que estos puntos sean puntos de referencia    
  resnndFAdapt = resnndF
  for(ii in nn$ID_row) {
    resnndFAdapt[resnndFAdapt$ID_pos == ii,c('ID_row','dist')] = c(ii,0)
  }
  
  #### Aplica la recodificacion a os puntos de la base  
  resnndFAdapt = resnndFAdapt %>% arrange(ID_row,dist)
  datPuntos[resnndFAdapt$ID_pos,'ID_row'] = resnndFAdapt$ID_row
  
  return(list(datos = datPuntos,refPos = resnndFAdapt))
}
