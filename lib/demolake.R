textToTable <- function(text, ...) {
  dfr <- read.table(tc <- textConnection(text), ...)
  close(tc)
  dfr
}

demolake <- function(q) {
  cmd=paste0("ssh demolake \"hive -e \\\"", q, "\\\"\"")
  result <- system(cmd, intern=TRUE)
  textToTable(result, sep="\t")
}