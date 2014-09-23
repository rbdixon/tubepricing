# Eat an SPDF derived a KML rectangle and convert to fence definition
kmlBBToFence = function(spdf) {
  bb = bbox(spdf)
  return(
    paste(paste(bb[,"min"], collapse=" "), paste(bb[,"max"], collapse=" "), sep=",")
    )
}

kmlPolygonToFence = function(spdf) {
  df = fortify(spdf)
  r=ddply(df, .(id), function(d) {
    data.frame(
      LONLATRAD=paste("POLYGON((", paste(d$long, d$lat, sep=" ", collapse=","), "))", sep=""),
      stringsAsFactors=FALSE
    )})
  return(r$LONLATRAD)
}

# print(kmlBBToFence(rectangle.spdf))
# print(kmlPolygonToFence(polygon.spdf))