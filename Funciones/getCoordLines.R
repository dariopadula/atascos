
# Extrael los extrmos del segmento

getCoordLines = function(dat,varName = 'location',addIni = '_lini',addFin = '_lfin') {
  
  aux = datJam[,varName]
  res = strsplit(gsub('\\{coordinates=\\[\\[|\\]\\]|\\[|\\]|','',aux),'\\,')
  
  dat[,paste0('X',addIni)] = as.numeric(as.character(do.call(c,lapply(res,function(xx) gsub(' ','',xx[2])))))
  dat[,paste0('Y',addIni)] = as.numeric(as.character(do.call(c,lapply(res,function(xx) gsub(' ','',xx[1])))))
  
  dat[,paste0('X',addFin)] = as.numeric(as.character(do.call(c,lapply(res,function(xx) gsub(' ','',xx[4])))))
  dat[,paste0('Y',addFin)] = as.numeric(as.character(do.call(c,lapply(res,function(xx) gsub(' ','',xx[3])))))
  
  return(dat)
}
