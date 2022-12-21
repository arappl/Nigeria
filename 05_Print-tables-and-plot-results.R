env.pre <- ls()

if (!require("parallel")) {install.packages("parallel")}
if (!require("mboost")) {install.packages("mboost")}
if (!require("ggplot2")) {install.packages("ggplot2")}
if (!require("kableExtra")) {install.packages("kableExtra")}
if (!require("dplyr")) {install.packages("dplyr")}


# Code contains:
## 1. print tabular overview of data (Table 1) 
## 2. plot estimation results (Figures 8, 25, 9, 10)
## 3. print selection table (Table 2) 

## 1. print tabular overview of data (Table 1) #######################################################################
dat <- read.table("./02_data/nigeriaBX.raw", header = T)
dat <- dat[, c(1:7, ncol(dat), 8: (ncol(dat)-1))]
dat <- dat[, -grep("REGEN", names(dat))]
desc <- c("Degree of stunting", "child's age in months", "mother's bmi", "mother's age in years",
          "education of mother's partner in years", "child's gender", "child is a twin", "child is 1st child", 
          "child is 2nd child", "child is 3rd child", "child is 4th child", "child is 5th child",
          "mother's employment status", "residential area", "household has electricity",
          "household has a radio", "household has a television",  "household has a refrigerator",  
          "household has a bicycle", "household has a motorcycle", "household has a car")

catvar <- names(dat)[6:length(names(dat))]
special <- c("mresidenceUrban", "munemployedunemployed", "ctwintwin", "csexFemale")
catvar <- catvar[!catvar %in% special]
dat[, catvar] <- apply(dat[, catvar], 2, function(x) ifelse(x == 0, "no = 0", "yes = 1"))

dat$mresidenceUrban <- ifelse(dat$mresidenceUrban == 1, "urban = 1", "rural = 0")
dat$munemployedunemployed <- ifelse(dat$munemployedunemployed == 1, "unemployed = 1", "employed = 0")
dat$ctwintwin <- ifelse(dat$ctwintwin == 1, "twin = 1", "single birth = 0")
dat$csexFemale <- ifelse(dat$csexFemale == 1, "female = 1", "male = 0")

ndat0 <- t(apply(dat[, 1:5], 2, function(x) c(mean(x, na.rm = T), sd(x, na.rm = T), range(x, na.rm = T))))
ndat0 <- round(ndat0, 2)
rnam <- rownames(ndat0)
ndat0 <- cbind(paste0(ndat0[, 1], " (", ndat0[,2], ")"),
               paste0(sprintf("%.02f", ndat0[, 3]), " - ", sprintf("%.02f", ndat0[, 4])))
ndat0 <- rbind(c("Mean (Std.Dev.)", "Min - Max"),
               ndat0)
colnames(ndat0) <- c("x1", "x2")
rownames(ndat0) <- c("Variable", rnam)

ndat1 <- lapply(dat[, 6:ncol(dat)], function(x) round(prop.table(table(x))*100, 2))
nam <- names(ndat1)
ndat1 <- data.frame(cbind(Reduce(rbind, lapply(ndat1, function(x) paste(names(x), collapse = " / "))),
                          Reduce(rbind, lapply(ndat1, function(x) paste(x, collapse = " / ")))
))
# add labels better readable for table
namdf <- data.frame("modelname" = nam, "label" = c("csex", "ctwin",  "cbirthorder1", "cbirthorder2", "cbirthorder3", "cbirthorder4", "cbirthorder5", "munemployed", "mresidence", "electricity", "radio", "television", "refrigerator", "bicycle", "motorcycle", "car"))
nam <- namdf$label[match(nam, namdf$modelname)]

ndat1 <- rbind(c("", "Frequency in %"),
               ndat1)
names(ndat1) <- c("x1", "x2")
rownames(ndat1) <- c("", nam)

ndat <- data.frame(cbind(rbind(ndat0, ndat1), "X3" = c("Description", desc[1:5], "", desc[6:length(desc)])))
ndat <- ndat[, c(3, 1, 2)]
names(ndat) <- rep("", 3)

df <- 
ndat %>%
  kbl(booktabs = T, 
      format = "html", 
      align = c("l", "l", "c", "c"),
      caption = "Candidate variables in the Nigeria childhood malnutrition data set") %>%
  # kable_paper(full_width = F, latex_options = c("striped", "hold_position")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  row_spec(1, bold=T) %>%
  pack_rows("Continuous/ Discrete", 2, 6) %>%
  row_spec(7, bold=T) %>%
  pack_rows("Binary", 7, nrow(ndat)) 
if(cluster == T) {
  writeLines(df, "./04_figures-and-tables/05_Print-tables-and-plot-results_Overview-data-Table-1.txt")
} else {
  save_kable(df, "./04_figures-and-tables/05_Print-tables-and-plot-results_Overview-data-Table-1.pdf")
}

rm(df)

## 2. plot estimation results (Figures 8, 25, 9, 10) #######################################################################
# load BX results
load("./03_output/04_read-out-results_resBX.Rdata")
load("./03_output/04_read-out-results_deltaresBX.Rdata")

linres <- Reduce(rbind, lapply(res, function(x) x$linres))
nlinres <- Reduce(rbind, lapply(res, function(x) x$nlinres))
totres <- Reduce(rbind, lapply(res, function(x) x$toteff))

totres$tau <- factor(as.numeric(totres$tau)/100)
linres$tau <- factor(as.numeric(linres$tau)/100)
nlinres$tau <- factor(as.numeric(nlinres$tau)/100)

## combine effect results with selection results
totres <- merge(totres, deltares[deltares$type == "nonlinear", c("varname", "tau", "pmed")], all.x = T, sort = F)
names(totres)[names(totres) == "pmed"] <- "selection"
nlinres <- merge(nlinres, deltares[deltares$type == "nonlinear", c("varname", "tau", "pmed")], 
                 by.x = c("varname", "tau"), by.y = c("varname", "tau"), all.x = T, sort = F)
names(nlinres)[names(nlinres) == "pmed.y"] <- "selection"
linres <- merge(linres, deltares[deltares$type == "linear", c("varname", "tau", "pmed")], 
                by.x = c("varname", "tau"), by.y = c("varname", "tau"), all.x = T, sort = F)
names(linres)[names(linres) == "pmed.y"] <- "selection"
linres$selection[linres$varname == "const"] <- 1

alllevels <- c('cage','csexFemale','mbmi','mage','edupartner','munemployedunemployed','ctwintwin','cbirthorder1','cbirthorder2','cbirthorder3','cbirthorder4','cbirthorder5','mresidenceUrban','electricityyes','radioyes','televisionyes','refrigeratoryes','bicycleyes','motorcycleyes','caryes','const')
lev <-  unique(linres$varname)[match(alllevels, unique(linres$varname))]
lev <- lev[length(lev):1]
linres$varname <- factor(linres$varname, levels = lev)

# load boosted results
load("./03_output/04_read-out-results_resBoost.Rdata")

linresboost <- Reduce(rbind, lapply(resboost, function(x) x$linres))
nlinresboost <- Reduce(rbind, lapply(resboost, function(x) x$nlinres))
totresboost <- Reduce(rbind, lapply(resboost, function(x) x$toteff))
spatresboost <- Reduce(cbind, lapply(resboost, function(x) x$spatial))
colnames(spatresboost) <- c(0.05, 0.1, 0.5)

totresboost$tau <- factor(totresboost$tau)
linresboost$tau <- factor(linresboost$tau)
nlinresboost$tau <- factor(nlinresboost$tau)

totresboost$selection <- nlinresboost$selection

lev <-  unique(linresboost$varname)[match(alllevels, unique(linresboost$varname))]
lev <- lev[length(lev):1]
linresboost$varname <- factor(linresboost$varname, levels = lev)
rm(alllevels, lev)

# plot non-linear effects (Figure 8 and Appendix Figure 25) ----------------------------------------
var.labels <- c("child age", "education partner", "mother age", "mother BMI")
names(var.labels) <- unique(totres$varname)

tau.labels <- c("5%", "10%", "50%")
names(tau.labels) <- unique(linres$tau)

ggplot(nlinres, aes(x = x, y = pmean, group = tau)) +
  geom_line(col = ifelse(nlinres$selection == 1, "black", "grey")) +
  geom_ribbon(aes(ymin = pqu2p5, ymax = pqu97p5, group = tau), alpha = 0.1, colour = NA) +
  facet_grid(tau ~ varname, scales = "free", 
             labeller = labeller(varname = var.labels, tau = tau.labels)) +
  scale_linetype_discrete(name = "Quantiles", labels = c("5%", "10%", "50%")) +
  labs(x = "", y = "", title = "Smooth effects only") +
  geom_line(data = nlinresboost, aes(x = x, y = effect, group = tau), linetype = 2,
            col = ifelse(nlinresboost$selection == 1, "black", "grey"))
ggsave(filename = "./04_figures-and-tables/05_Print-tables-and-plot-results_Non-linear-effects-Figure-8.jpeg")

ggplot(totres, aes(x = x, y = pmean, group = tau)) +
  geom_line(col = ifelse(totres$selection == 1, "black", "grey")) +
  geom_ribbon(aes(ymin = pqu2p5, ymax = pqu97p5, group = tau), alpha = 0.1, colour = NA) +
  facet_grid(tau ~ varname, scales = "free",
             labeller = labeller(varname = var.labels, tau = tau.labels)) +
  scale_linetype_discrete(name = "Quantiles", labels = c("5%", "10%", "50%")) +
  labs(x = "", y = "", title = "") +
  geom_line(data = totresboost, aes(x = x, y = effect, group = tau), linetype = 2,
            col = ifelse(totresboost$selection == 1, "black", "grey"))
ggsave(filename = "./04_figures-and-tables/05_Print-tables-and-plot-results_Non-linear-effects-with-linear-component-Appendix-Figure-25.jpeg")


# plot linear effects (Figure 9) ----------------------------------------
lin.var.labels <- c("const", "car", "motorcycle", "bicycle", "refrigerator", "television", "radio", "electricity", "mresidence",
                    "cbirthorder5", "cbirthorder4", "cbirthorder3", "cbirthorder2", "cbirthorder1", "ctwin", "munemployed",
                    "edupartner", "mage", "mbmi", "csex", "cage")
names(lin.var.labels) <- levels(linres$varname)

ggplot(subset(linres, varname != "const"), aes(x = tau, y = pmean)) +
  geom_hline(yintercept = 0, col = "darkgrey") +
  geom_errorbar(aes(ymin = pqu2p5, ymax = pqu97p5), width=0.5, alpha = 0.5, col= "blue") +
  geom_point(col = ifelse(linres$selection[linres$varname != "const"] == 1, "blue", "white"), shape = 19) + # placeholders for circles, which are being overlayed further down
  geom_point(shape = ifelse(linres$selection[linres$varname != "const"] == 1, 19, 1), col= "blue",  stroke = 1) + # actual circle with actual colouring
  facet_grid(varname ~ ., switch = "y",
             labeller = labeller(varname = lin.var.labels)) +
  coord_flip() +
  scale_x_discrete(name = "", labels = c("5%", "10%", "50%")) +
  labs(x = "", y = "",
       title = "Linear effects by quantile and method") +
  theme(legend.position = "bottom",
        strip.text.y.left = element_text(angle = 0)) +
  geom_point(data = subset(linresboost, varname != "const"), aes(x = tau, y = effect),
             col = ifelse(linresboost$selection[linresboost$varname != "const"] == 1, "black", "white"), shape = 19) + # placeholders for circles, which are being overlayed further down
  geom_point(data = subset(linresboost, varname != "const"), aes(x = tau, y = effect), col = "black",
             shape = ifelse(linresboost$selection[linresboost$varname != "const"] == 1, 19, 1),  stroke = 1)  # actual circle with actual colouring
ggsave(filename = "./04_figures-and-tables/05_Print-tables-and-plot-results_Linear-effects-Figure-9.jpeg", width = 6, height = 9)

# plot spatial effect/ map (Figure 10) ----------------------------------------
map <- read.bnd("./02_data/nigeria37BX.bnd")
mrfres <- list.files("./03_output/01_BayesX", "DHSREGEN37.res", recursive = T, full.names =T)
mrfres <- mrfres[-grep("variance", mrfres)]
tauBX <- substr(Reduce(rbind, strsplit(mrfres, "(.*)Results_"))[,2], start= 0, stop = 2)
tauBX <- as.numeric(tauBX)/100
mrfres <- lapply(mrfres, read.table, header = T)
names(mrfres) <- tauBX

tau <- c(0.05, 0.1, 0.5)
mrfres <- mrfres[sort(match(tauBX, tau))]

mrfresboost <- lapply(tau, function(j) {
  fls <- list.files("./03_output/02_mboost/", pattern = paste0("coef_", sprintf("%02d", j*100)), full.names = T )
  load(fls)
  tmp0 <- tmp
  tmp <- tmp[[length(tmp)]]
  tmp <- data.frame(as.numeric(names(tmp)), tmp)
  return(tmp)
})
names(mrfresboost) <- tau

# add intercept
mrfres2 <- mrfres
mrfres2boost <- mrfresboost

mrfres2 <- lapply(tau, function(x) {
  mrfres2[[as.character(x)]]$pmean <- mrfres2[[as.character(x)]]$pmean + linres$pmean[linres$varname == "const" & linres$tau == x]
  return(mrfres2[[as.character(x)]])
})
mrfres2boost <- lapply(tau, function(x) {
  mrfres2boost[[as.character(x)]]$tmp <- mrfres2boost[[as.character(x)]]$tmp + linresboost$effect[linresboost$varname == "const" & linresboost$tau == x]
  return(mrfres2boost[[as.character(x)]])
})

# set quantile specific color range
qlims <- lapply(1:3, function(x) range(c(mrfres2[[x]]$pmean, mrfres2boost[[x]]$tmp)))

# actual plot
pdf("./04_figures-and-tables/05_Print-tables-and-plot-results_Spatial-effect-Figure-10.pdf")
par(mfcol = c(3,2), oma = c(1,1,4,1))
for (i in 1:length(mrfres2)) {
  drawmap(data = mrfres2[[i]], map = map, main = paste0("Quantile = ", tau[i]*100, "%"), limits = qlims[[i]], cols = "grey")
  if (i == 1) mtext("Bayesian effect selection", side = 3, padj = -5)
}

for (i in 1:length(mrfres2boost)) {
  drawmap(data = mrfres2boost[[i]], map = map,  regionvar = 1,  plotvar = 2, 
          main = paste0("Quantile = ", tau[i]*100, "%"), limits = qlims[[i]], cols = "grey")
  if (i == 1) mtext("Gradient boosting ", side = 3, padj = -5)
}
dev.off()

## 3. print selection table (Table 2) #######################################################################
# prepare table by restructuring results
tab <- with(nlinresboost, tapply(selection, INDEX = list(varname, tau), sum))
tab[tab != 0] <- 1
tab <- data.frame(tab)
names(tab) <- gsub("X", "Selection_boost_", names(tab))
tab$xlabels <- paste0("sm(", rownames(tab), ")")

tabl <- with(linresboost, tapply(selection, INDEX = list(varname, tau), sum))
tabl <- data.frame(tabl)
names(tabl) <- gsub("X", "Selection_boost_", names(tabl))
tabl$xlabels <- paste0("lin(", rownames(tabl), ")")

tabs <- colSums(spatresboost != 0)
tabs[tabs != 0] <- 1
tabs <-  data.frame(t(tabs))
names(tabs) <- gsub("X", "Selection_boost_", names(tabs))
tabs$xlabels <- "mrf(region)"

deltaboost <- rbind(tab, tabl, tabs)
rm(tab, tabl, tabs)

tab <- with(deltares, tapply(pmean, list(xlabels, tau), sum))
tab <- data.frame(tab)
names(tab) <- gsub("X", "Selection_freq_", names(tab))
tabs <- with(deltares, tapply(pmed, list(xlabels, tau), sum))
tabs <- data.frame(tabs)
names(tabs) <- gsub("X", "Selection_", names(tabs))

deltaBX <- cbind(tab, tabs)

deltaBX <- deltaBX[c(grep("0.05", names(deltaBX)), grep("0.1", names(deltaBX)), grep("0.5", names(deltaBX)))]

deltaBX0 <- data.frame(cbind(paste0(deltaBX[, 2], " (", sprintf("%.03f",deltaBX[,1]), ")"), 
                             paste0(deltaBX[, 4], " (", sprintf("%.03f",deltaBX[,3]), ")"),
                             paste0(deltaBX[, 6], " (", sprintf("%.03f",deltaBX[,5]), ")")))
names(deltaBX0) <- names(tab)
deltaBX0$xlabels <- rownames(deltaBX)
deltaBX0$xlabels[grep("mrf", deltaBX0$xlabels)] <- "mrf(region)"
deltaBX <- deltaBX0

selection <- merge(deltaBX, deltaboost)

selection <- selection[, c(1, grep("0.05", names(selection)), grep("0.1", names(selection)), grep("0.5", names(selection)))]

for (i in 1:nrow(namdf)) {selection$xlabels <- gsub(namdf[i,1], namdf[i,2], selection$xlabels)}

pos <- grep("mrf", selection$xlabels)
selection <- selection[c(1:(pos-1), ((pos + 1):nrow(selection)), pos), ]

rownames(selection) <- selection[,1]
selection <- selection[, -grep("xlabels", names(selection))]

nam <- gsub("Selection_freq_.*", "NBPSS", names(selection))
nam <- gsub("Selection_boost_.*", "Boosting", nam)
names(selection) <- nam

df <- 
selection %>%
  kbl(booktabs = T, 
      align = c("l", "c", "c", "c", "c", "c", "c"),
      caption = "Selection of variables by the NBPSS prior (including the selection frequency in brackets) and gradient boosting. Boosting seems to be more conservative in variable selection.") %>%
  # kable_paper(full_width = F) %>%
  # kable_styling(latex_options = "scale_down") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(" " = 1, "tau = 0.05" = 2, "tau = 0.1" = 2, "tau = 0.5" = 2)) 
if(cluster == T) {
  writeLines(df, "./04_figures-and-tables/05_Print-tables-and-plot-results_Selection-Table-2.txt")
} else {
  save_kable(df, "./04_figures-and-tables/05_Print-tables-and-plot-results_Selection-Table-2.pdf")
}

rm(list = grep(paste(env.pre, collapse = "|"), ls(), invert = T, value = T))