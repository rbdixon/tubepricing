reorderCols = function(D, cols) {
  firstcols = which(names(D) %in% cols)
  lastcols =  which(!(names(D) %in% cols))
  D[, c(firstcols, lastcols)]
}