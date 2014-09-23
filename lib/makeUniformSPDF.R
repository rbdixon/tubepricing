# Prefixes each slot with data frame name
# From http://gis.stackexchange.com/questions/32732/proper-way-to-rbind-spatialpolygonsdataframes-with-identical-polygon-ids
makeUniformSPDF<-function(SPDF){
  pref<-substitute(SPDF)  #just putting the file name in front.
  newSPDF<-spChFIDs(SPDF,as.character(paste(pref,rownames(as(SPDF,"data.frame")),sep="_")))
  return(newSPDF)
}