env.pre <- ls()

files <- list.files("./03_output/01_BayesX/", ".prg", recursive = T, full.names = T)

mclapply(files, function(j) {
  gc()
  system(paste0(BXpath, " ", j), wait = TRUE)
}, mc.cores = cores)

rm(list = grep(paste(env.pre, collapse = "|"), ls(), invert = T, value = T))