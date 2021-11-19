
#### Cextrae las coordenas de los centroides

getCoordCent = function(dat,varName = 'location_centroid',nomX = 'Xc',nomY = 'Yc') {
  aux = datJam[,varName]
  res = strsplit(gsub('\\(|\\)','',aux),'\\,')
  
  dat[,nomX] = as.numeric(as.character(do.call(c,lapply(res,function(xx) xx[2]))))
  dat[,nomY] = as.numeric(as.character(do.call(c,lapply(res,function(xx) xx[1]))))
  
  
  return(dat)
}
