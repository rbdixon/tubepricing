export_csv = function(D, basename, method="na") {
  fn=sprintf("reports/%s_%s_%s%s%s_%s.csv", basename, method,
             year(now()), month(now()), day(now()),
             system("git describe --tags --dirty", intern=TRUE)
  )
  write.table( D, fn, row.names = FALSE, sep=",", quote = FALSE)
}