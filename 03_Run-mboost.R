env.pre <- ls()
dat <- read.table("./02_data/nigeriaBXc.raw", header = T)

tau <-c(0.05, 0.1, 0.5)
## Test ######################
mapgra <- read.gra("./02_data/nigeria37BX.gra")

dat$DHSREGEN37 <- factor(dat$DHSREGEN37)

fmadd <- stunting ~ 
  bols(cage, intercept = FALSE) + bbs(cage, center=TRUE, df = 1) +
  bols(mbmi, intercept = FALSE) + bbs(mbmi, center=TRUE, df = 1) +
  bols(mage, intercept = FALSE) + bbs(mage, center=TRUE, df = 1) +
  bols(edupartner, intercept = FALSE) + bbs(edupartner, center=TRUE, df = 1) +
  bols(csexFemale) +
  bols(ctwintwin) +
  bols(cbirthorder1) +
  bols(cbirthorder2) +
  bols(cbirthorder3) +
  bols(cbirthorder4) +
  bols(cbirthorder5) +
  bols(munemployedunemployed) +
  bols(mresidenceUrban) +
  bols(electricityyes) +
  bols(radioyes) +
  bols(televisionyes) +
  bols(refrigeratoryes) +
  bols(bicycleyes) +
  bols(motorcycleyes) +
  bols(caryes) +
  bmrf(DHSREGEN37, by = NULL, index = NULL, bnd = mapgra, df = 4, lambda = NULL, center = "spectralDecomp")

set.seed(456321)
lapply(tau, function(j) {
  gc()
  model <- gamboost(fmadd,
                    data = dat,
                    family = QuantReg(tau = j),
                    control = boost_control(trace = T, nu = 0.25))
  cvm <- cvrisk(model,
                grid = seq(200, 10, -10) * 1000,
                mc.cores = ifelse(cluster, 5, 1))
  
  if (mstop(cvm) == 200000) {
    model <- gamboost(fmadd,
                      data = dat,
                      family = QuantReg(tau = j),
                      control = boost_control(nu = 0.25))
    cvm <- cvrisk(model,
                  grid = seq(200, 400, 10) * 1000,
                  mc.cores = ifelse(cluster, 5, 1))
  }
  model[mstop(cvm)]
  save(model, file = paste0("./03_output/02_mboost/03_Run-mboost_Boost_", sprintf("%02d", j*100) ,".Rdata"))
  
  MSTOP <- mstop(cvm)
  write.table(MSTOP, file = paste0("./03_output/02_mboost/03_Run-mboost_Boost-mstop_", sprintf("%02d", j*100) ,".txt"), row.names = F, col.names = F)
  save(MSTOP, file = paste0("./03_output/02_mboost/03_Run-mboost_Boost-mstop_", sprintf("%02d", j*100) ,".Rdata"))
  
  tmp <- coef(model, which = "")
  save(tmp, file = paste0("./03_output/02_mboost/03_Run-mboost_Boost-coef_", sprintf("%02d", j*100) ,".Rdata"))
})

rm(list = grep(paste(env.pre, collapse = "|"), ls(), invert = T, value = T))
