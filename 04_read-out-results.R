env.pre <- ls()

if (!require("BayesX")) {install.packages("BayesX")}
if (!require("parallel")) {install.packages("parallel")}
if (!require("mboost")) {install.packages("mboost")}
if (!require("ggplot2")) {install.packages("ggplot2")}
if (!require("kableExtra")) {install.packages("kableExtra")}
if (!require("dplyr")) {install.packages("dplyr")}


### READ OUT BX ######################################################## ##
taudirs <- list.dirs("./03_output/01_BayesX", recursive = F)

# delta/ selection probability results
res <- lapply(taudirs, function(p){
  files <-list.files(p)
  
  if (length(files) == 0) {
    return(NULL)
  } else {
    
    files <- grep("delta.res", files, value = T)
    
    type <- character(length(files))
    type[grep("Linear", files)] <- "linear"
    type[grep("nonlinear", files)] <- "nonlinear"
    type[grep("spatial", files)] <- "spatial"
    
    attributes <- unlist(strsplit(p, split = "/"))
    attributes <- unlist(strsplit(attributes[length(attributes)], split = "_"))
    names(attributes) <- c("Results", "tau")
    attributes <- attributes[names(attributes) %in% c("tau")]
    
    eff <-  data.frame(type, t(attributes)[,,drop = F])
    
    res <-
      lapply(files, function(x) {
        dat <- read.table(paste0(p,"/", x), header = T)
      })
    
    res <-
      lapply(1:length(res), function(x) {
        data.frame(cbind(res[[x]], eff[x, ]))
      })
    
    res <- Reduce(rbind, res)
    return(res)
  }
})

res <- Reduce(rbind, res)
res$tau <- as.numeric(res$tau)/100
res$xlabels <- paste0(c("lin", "sm", "mrf")[match(res$type, unique(c(unique(res$type), "spatial")))],
                      "(", res$varname, ")")
res$xlabels <- factor(res$xlabels, levels = unique(res$xlabels))

deltares <- res
save(deltares, file = "./03_output/04_read-out-results_deltaresBX.Rdata")

# read out estimation results
res <- lapply(taudirs, function(p){
  files <-list.files(p, ".res")
  
  if (length(files) == 0) {
    return(NULL)
  } else {
    
    files <- files[-grep("delta|var|variance|scale|psi2|omega|prob|predict", files)]
    files <- files[-grep("basisR|param", files)]
    
    attributes <- unlist(strsplit(p, split = "/"))
    attributes <- unlist(strsplit(attributes[length(attributes)], split = "_"))
    names(attributes) <- c("Results", "tau")
    attributes <- attributes[names(attributes) %in% c("tau")]
    
    linres <- lapply(paste0(p, "/", grep("Linear", files, value = T)), read.table, header = T)
    names(linres) <- ifelse(grepl("ssvs", grep("Linear", files, value = T)), "ssvs", "const")
    
    nlinres <- lapply(paste0(p, "/", grep("nonlinear", files, value = T)), read.table, header = T)
    names(nlinres) <- Reduce(rbind, lapply(nlinres, names))[,2]
    relstat <- c("pmean", "pqu2p5", "pqu10", "pqu90", "pqu97p5")
    
    nlineffs <- grep("nonlinear", files, value = T)
    nlineffs <- gsub(".res", "", gsub("r_MAIN_mu_REGRESSION_stunting_nonlinear_pspline_effect_of_", "", nlineffs))
    
    toteff <- lapply(nlineffs, function(i) {
      smeff <- nlinres[[grep(i, names(nlinres))]]
      x <- smeff[, i]
      linID <- grep(paste0(i, "$"), linres$ssvs$varname)
      
      lineff <- as.matrix(x) %*% as.matrix(linres$ssvs[linID, relstat])
      lineff[, -1] <- t(apply(lineff[,-1], 1, sort)) # ensure consistency of HDI boundaries (LB is real lower and UB is real upper boundary)
      toteff0 <- lineff[, relstat] + smeff[, relstat]
      toteff <- toteff0
      for(j in relstat) {toteff[, j] <- toteff0[, j] + linres$const[, j]} # add constant
      toteff <- data.frame("x" = x, "varname" = i, toteff)
      
      return(toteff)
    })
    
    linres <- Reduce(rbind, linres)
    linres <- data.frame(linres, t(attributes))
    
    nlinres <- lapply(nlinres, function(x) cbind(x, "varname" = names(x)[2]))
    nlinres <- lapply(nlinres, function(x) {
      names(x)[grep(paste0(Reduce(rbind, lapply(nlinres, names))[,2], collapse = "|"), names(x))] <- "x"
      return(x)
    })
    nlinres <- Reduce(rbind, nlinres)
    nlinres <- data.frame(nlinres, t(attributes))
    
    toteff <- Reduce(rbind, toteff)
    toteff <- data.frame(toteff, t(attributes))
    
    return(list("linres" = linres, "nlinres" = nlinres, "toteff" = toteff))
  }
})
save(res, file = "./03_output/04_read-out-results_resBX.Rdata")


### READ OUT mboost ######################################################## ##
dat <- read.table("./02_data/nigeriaBXc.raw", header = T)

resboost <- lapply(c(0.05, 0.1, 0.5), function(j) {
  fls <- list.files("./03_output/02_mboost/", pattern = paste0("coef_", sprintf("%02d", j*100)), full.names = T )
  load(fls)
  spatial <- tmp[[grep("bmrf", names(tmp))]]
  tmp0 <- tmp[-grep("bmrf", names(tmp))]
  
  const <- attributes(tmp)$offset
  
  tmp <- lapply(1:length(tmp0), function(z){
    if (grepl("bbs", names(tmp0)[z])) {
      nam <- unlist(strsplit(names(tmp0)[z], split = "[[:punct:]]"))[2]
      spline <- mboost::bbs(dat[,nam], center = T)
      if (length(tmp0[[z]]) != 0) {
        effect <- as.numeric(extract(spline, "design") %*% tmp0[[z]])
      } else {
        effect <- as.numeric(extract(spline, "design") %*% rep(0, ncol(extract(spline, "design"))))
      }
      tab <- data.frame("varname" = nam, "x" = dat[, nam], "effect" = effect, "type" = "nonlinear")
      return(tab)
    } else {
      nam <-  names(tmp0[[z]])
      tab <- data.frame("varname" = nam, "effect" = tmp0[[z]], "type" = "linear")
      rownames(tab) <- paste0(nam[2], nam)
      return(tab)
    }
  })
  
  linID <- grep("bols", names(tmp0))
  nlinID <- grep("bbs", names(tmp0))
  
  linres <- Reduce(rbind, tmp[linID])
  nlinres <- tmp[nlinID]
  names(nlinres) <- Reduce(rbind, strsplit(names(tmp0)[nlinID], split = "[[:punct:]]"))[,2]
  
  linres <- rbind(linres, c("const", const, "linear"))
  linres$effect <- as.numeric(linres$effect)
  
  linres$varname[grep("ntercept", linres$varname)] <- "const"
  linres <- aggregate(effect ~ type + varname, linres, sum)
  
  toteff <- lapply(names(nlinres), function(i) {
    smeff <- nlinres[[grep(i, names(nlinres))]]
    x <- smeff[, "x"]
    linID <- grep(paste0(i, "$"), linres$varname)
    
    if(length(linID) == 0) {
      lineff <- as.matrix(x) %*% 0
    } else {
      lineff <- as.matrix(x) %*% as.matrix(linres$effect[linID])
    }
    
    toteff <- lineff + smeff$effect
    toteff <- toteff + linres$effect[linres$varname == "const"]
    toteff <- data.frame("x" = x, "varname" = i, "effect" = toteff)
    
    return(toteff)
  })
  
  linres <- data.frame(linres, "tau" = j)
  linres$selection <- ifelse(linres$effect != 0, 1, 0)
  
  nlinres <- lapply(nlinres, function(x) {
    if(all(x$effect == 0)) {
      x$selection <- 0
    } else {
      x$selection <- 1
    }
    return(x)
  })
  nlinres <- Reduce(rbind, nlinres)
  nlinres <- data.frame(nlinres, "tau" = j)
  
  toteff <- Reduce(rbind, toteff)
  toteff <- data.frame(toteff, "tau" = j)
  
  return(list("linres" = linres, "nlinres" = nlinres, "toteff" = toteff, "spatial" = spatial))
  
})
save(resboost, file = "./03_output/04_read-out-results_resBoost.Rdata")

rm(list = grep(paste(env.pre, collapse = "|"), ls(), invert = T, value = T))