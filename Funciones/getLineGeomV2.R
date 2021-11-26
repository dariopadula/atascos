getLineGeomV2 = function(dat,varName = 'location',nomGeom = 'geometry') {
  
  x = dat[,varName]
  
  coords = gsub('\\{coordinates=\\[\\[','',x)
  coords = gsub('\\]\\]\\, type=LineString\\}','',coords)
  coords = gsub(' ','',coords)
  coords = strsplit(coords,'\\],\\[')
  
  
  coordsCentro = do.call(c,lapply(coords,function(yy) {
    coordsAux = sapply(yy,function(xx) {
          res = numeric()
          aux = as.numeric(strsplit(xx,',')[[1]])
          res[1] = min(aux)
          res[2] = max(aux)
          
          
          # res = round(res,4)
          
          return(paste(res,collapse = ' '))
        })
    
    resgeom = paste0('LINESTRING (',paste(coordsAux,collapse = ', '),')')
    
  return(resgeom)
  })
  )
  

  

  dat[,nomGeom] = coordsCentro
  dat[,'nSegemnt'] = do.call(c,lapply(coords,function(xx) return(length(xx) - 1)))
  
  return(dat)
  
}
