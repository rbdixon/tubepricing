read.csv.https <- function(url, ...) {
  csv <- getURL(url)
  read.csv(textConnection(csv), ...)
}