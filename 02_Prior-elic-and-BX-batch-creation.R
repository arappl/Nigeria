env.pre <- ls()

if (!require("BayesX")) {install.packages("BayesX")}
if (!require("sdPrior")) {install.packages("sdPrior")}
if (!require("MASS")) {install.packages("MASS")}
if (!require("parallel")) {install.packages("parallel")}

# read in neighbour matrix
grafiles <- list.files("./02_data", ".gra", full.names = T, recursive = F)
Kreg <- lapply(grafiles, read.gra)

tau <-c(0.05, 0.1, 0.5)
BI <- c(3000, 3000, 2000)
step <- c(224, 158, 53)
seed <- c(333, 123, 583) # 321 111 583

taudf <- data.frame("tau" = tau,
                    "iter" = BI + step * 1000,
                    "BI" = BI,
                    "step" = step,
                    "seed" = seed)

a <- 5
alpha <- c(0.1)
c <- c(0.1)
sdPinput <- expand.grid(alpha, c)

nlinvar <- c("cage",
             "mbmi",
             "mage",
             "edupartner")

dat <- read.table("./02_data/nigeriaBXc.raw", header = T)
relvar <- names(dat[, -grep("stunting|REGEN", names(dat))])
spatvar <- grep("REGEN", names(dat), value = T)

names(Kreg) <- spatvar

mclapply(1:nrow(sdPinput), function(i)
{
  gc()
  alpha0 <-  sdPinput[i, 1]
  c0 <- sdPinput[i, 2]
  
  
  if (!file.exists("./03_output/01_BayesX/02_Prior-elic-and-BX-batch-creation_Hyperparameter.RData")) {
      hyperpara <- mclapply(which(names(dat) %in% relvar), function(z) {
        # elicitate for linear components
        Z <- as.matrix(dat[,z])
        K <- 1
        Kinv <- ginv(K)
        mdflin <- hyperpar(Z, Kinv, a=a, c=c0, alpha1=alpha0, alpha2=alpha0, R=10000, myseed=123)
        
        # elicitate for non-linear components
        if (z %in% which(names(dat) %in% nlinvar)) {
          Z <- DesignM(dat[, z])$Z_B
          K <- DesignM(dat[, z])$Kmatrix
          Kinv <- ginv(K)
          mdfnlin <- hyperpar(Z, Kinv, a=a, c=c0, alpha1=alpha0, alpha2=alpha0, R=10000, myseed=123)
        } else {
          mdfnlin <- NULL
        }
        return(list("lin" = mdflin, "nlin" = mdfnlin))
      }, mc.cores = cores)
      
    # elicitate for spatial variables
      hyperparaspat <- lapply(spatvar, function(z) {
        Z <- t(sapply(dat[, z], FUN=function(x){1*(x==rownames(Kreg[[z]]))}))
        Kinv <- ginv(Kreg[[z]])
        mdf <- hyperpar(Z, Kinv, a=a, c=c0, alpha1=alpha0, alpha2=alpha0, R=10000, myseed=123)
      })
      
      hyperpara <- c(hyperpara, hyperparaspat)
      names(hyperpara) <- c(relvar, spatvar)

    save(hyperpara, file = "./03_output/01_BayesX/02_Prior-elic-and-BX-batch-creation_Hyperparameter.RData")
  } else {
    load("./03_output/01_BayesX/02_Prior-elic-and-BX-batch-creation_Hyperparameter.RData")
  }
  
  gc()
  options(scipen = 100, digits = 4)
  
  set.seed(123456)
  
  for(j in 1:nrow(taudf)){
    # create folder results per quantile are stored in
    sub_dir <- paste0("Results_", sprintf("%02d", taudf$tau[j]*100), "/")
    dir.create(file.path("./03_output/01_BayesX/", sub_dir))
    
    # create actual batch
    jmProg <- paste0("./03_output/01_BayesX/", sub_dir, "/02_Prior-elic-and-BX-batch-creation_batch", sprintf("%02d", taudf$tau[j]*100), ".prg")
    write(NULL, jmProg)
    write(paste("% usefile",jmProg),jmProg)
    write(" ",jmProg,append=T)
    write("delimiter = ;",jmProg,append=T)
    write(" ",jmProg,append=T)
    write("dataset d;",jmProg,append=T)
    write(paste0("d.infile using ./02_data/nigeriaBXc.raw;"),jmProg,append=T)
    write("map g;",jmProg,append=T)
    write(paste0("g.infile using ./02_data/nigeria37BX.bnd;"),jmProg,append=T)
    write("mcmcreg m;",jmProg,append=T)
    write(paste0("m.outfile = ./03_output/01_BayesX/", sub_dir, "/r",";"),jmProg,append=T)
    write(paste0("logopen, replace using ./03_output/01_BayesX/", sub_dir, "/log", sprintf("%02d", taudf$tau[j]*100), ".txt;"),jmProg,append=T)
    write("m.hregress stunting = const", jmProg,append=T)
    for(ii in relvar) {
      write(paste0("+ ", ii, "(ssvs, gig, v1 = ", a, ", v2 = ", hyperpara[[ii]]$lin$b, ", r = ", hyperpara[[ii]]$lin$r,")"), jmProg,append=T)
    }
    for(ii in nlinvar) {
      write(paste0("+ ", ii, "(pspline, prior=ssvs, v1 = ", a, ", v2 = ", hyperpara[[ii]]$nlin$b, ", r = ", hyperpara[[ii]]$nlin$r,", centermethod=nullspace, ssvsupdate=gibbs)"), jmProg,append=T)
    }
      write(paste0("+ DHSREGEN37(spatial, map=g, prior=ssvs, v1 = ", a, ", v2 = ", hyperpara$DHSREGEN37$b, ", r = ", hyperpara$DHSREGEN37$r,", ssvsupdate=gibbs)"), jmProg,append=T)
    write(paste0(",family=quantreg quantile=", tau[j] ," predict=light iterations=", taudf$iter[j], " burnin=", taudf$BI[j], " step=", taudf$step[j]," setseed=", taudf$seed[j]," using d;"), jmProg,append=T)
    write("m.getsample;", jmProg, append=T)
    write("m.autocor;", jmProg, append = T)
    write("logclose;",jmProg,append=T)
    write("drop d g m;",jmProg,append=T)
    write(" ",jmProg,append=T)
    write("delimiter = return;",jmProg,append=T)
  }
}, mc.cores = length(tau))


rm(list = grep(paste(env.pre, collapse = "|"), ls(), invert = T, value = T))
