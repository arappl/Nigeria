env.pre <- ls()
## Prepare data for estimation and add spatial information
dat <- read.table("./02_data/nigeria.raw", header = T)

# ## Variable subset used in Klein et al. 2020
relvar <- c("cage",
            "mbmi", 
            "mage",  
            "edupartner", 
            "csex", 
            "ctwin", 
            "cbirthorder", 
            "munemployed",
            "mresidence",  
            "electricity", 
            "radio", 
            "television", 
            "refrigerator", 
            "bicycle", 
            "motorcycle", 
            "car", 
            "DHSREGEN37")

nlinvar <- c("cage",
             "mbmi",
             "mage",
             "edupartner")

dat <- dat[, c("stunting", relvar)]
dat$cbirthorder <- as.character(dat$cbirthorder)

# turn region into a number for BX to be able to read it
shpName <- sub(pattern="(.*)\\.dbf", replacement="\\1",
               x="./01_raw-data/sdr_subnational_boundaries.dbf")
map37 <- shp2bnd(shpname=shpName, 
                     regionnames="DHSREGEN")
names(map37) <- match(names(map37), unique(dat$DHSREGEN37))
write.bnd(map37, file = paste0("./02_data/nigeria37BX.bnd"))
write.gra(bnd2gra(map37), file = "./02_data/nigeria37BX.gra")

dat$DHSREGEN37 <- match(dat$DHSREGEN37, unique(dat$DHSREGEN37))

## unscaled dataset
datBX <- data.frame(cbind("stunting" = dat$stunting, model.matrix(stunting ~ . -1 , data = dat)))
datBX <- datBX[, -grep("csexMale", names(datBX))]

datBX$cbirthorder1 <- ifelse(rowSums(datBX[, grep("cbirthorder", names(datBX))]) == 0, 1, 0)
write.table(datBX, file = "./02_data/nigeriaBX.raw", col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)

## scaled numerical variables
datBXc <- datBX

datBXc[, which(names(datBXc) %in% nlinvar)] <- scale(datBXc[, which(names(datBXc) %in% nlinvar)])
write.table(datBXc, file = "./02_data/nigeriaBXc.raw", col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)

rm(list = grep(paste(env.pre, collapse = "|"), ls(), invert = T, value = T))
