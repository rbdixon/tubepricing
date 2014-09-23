dropCols = function(D, cols) {
  D[, -which(names(D) %in% cols)]
}