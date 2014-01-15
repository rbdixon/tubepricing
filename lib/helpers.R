# Load every function in the lib dir
load.helper.functions <- function() {
  files <- list.files('lib')
  files <- files[which(files != 'helpers.R')]
  lapply(files, function(f) {
    source(paste0('lib/', f))
  })
}

load.helper.functions()
rm(load.helper.functions)