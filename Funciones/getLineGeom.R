getLineGeom = function(dat,varName = 'location',nomGeom = 'geometry') {
  
  x = dat[,varName]
  ddd = gsub('\\{coordinates=\\[\\[','LINESTRING (',x) 
  ddd = gsub('\\,','',ddd)
  ddd = gsub('\\] \\[',', ',ddd)
  ddd = gsub('\\]\\] type=LineString\\}',')',ddd)
  
  dat[,nomGeom] = ddd
  
  return(dat)
  
}
