
#### Cextrae las coordenas de los centroides

getCoordCent = function(dat,varName = 'location_centroid',nomX = 'Xc',nomY = 'Yc') {
  aux = datJam[,varName]
  res = strsplit(gsub('\\(|\\)','',aux),'\\,')
  
  dat[,nomX] = do.call(c,lapply(res,function(xx) min(as.numeric(as.character(xx)))))
  dat[,nomY] = do.call(c,lapply(res,function(xx) max(as.numeric(as.character(xx)))))
  
  
  return(dat)
}
