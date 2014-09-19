factorcols = function(d, c) {
  d[,c] <- lapply(d[,c] , factor)
  return(d)
}