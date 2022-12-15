env.pre <- ls()
# 2013 Data
nigeriaraw <- read.dta("./01_raw-data/NGKR6AFL.DTA") # 2013 Data
vars <- character(0)
nams <- character(0)

# Response related variables:
# hw2      "Weight in kilograms (1 decimal)"
# hw3      "Height in centimeters (1 decimal)"
# hw70     "Ht/A Standard deviations (according to WHO)" = Stunting
# hw71     "Wt/A Standard deviations (according to WHO)" = Wasting
# hw72     "Wt/Ht Standard deviations (according to WHO)" = Underweight
# hw73     "BMI Standard deviations (according to WHO)"
# hw5      "Ht/A Standard deviations" = old definition of Stunting 
# hw8      "Wt/A Standard deviations" = old definition of Wasting
# hw11     "Wt/Ht Standard deviations" = old definition of Underweight

nigeriaraw$hw2 <- nigeriaraw$hw2/10
nigeriaraw$hw3 <- nigeriaraw$hw3/10
nigeriaraw$cbmi <- nigeriaraw$hw2/((nigeriaraw$hw3/100)^2)

vars <- c(vars, c("hw2", "hw3", "hw70", "hw71", "hw72", "hw73", "hw5", "hw8",
                  "hw11","cbmi"))
nams <- c(nams, c("cweight", "cheight", "stunting", "wasting", "underweight",
                  "cbmiscore", "stuntingold", "wastingold", "underweightold",
                  "cbmi"))

# v008     "Date of interview (CMC)"
# b3       "Date of birth (CMC)"
# m5       "Months of breastfeeding"
# b4       "Sex of child"
# bord     "Birth order number"
# b0       "Child is twin"

nigeriaraw$cage <- nigeriaraw$v008-nigeriaraw$b3
nigeriaraw$m5[nigeriaraw$m5==94] <- 0
nigeriaraw$m5[nigeriaraw$m5>90] <- NA
nigeriaraw$ctwin <- recode(nigeriaraw$b0, '1st of multiple'='twin' ,'2nd of multiple'='twin','3rd of multiple'='twin','4th of multiple'='twin','5th of multiple'='twin')

vars <- c(vars, c("cage", "m5", "b4", "ctwin", "bord"))
nams <- c(nams, c("cage", "breastfeeding", "csex", "ctwin", "cbirthorder"))


# v445     "Body mass index for respondent"
# v437     "Respondent's weight (kilos-1d)"
# v438     "Respondent's height (cms-1d)"
# b3       "Date of birth (CMC)"
# v011     "Date of birth (CMC)"
# v133     "Education in single years"
# v715     "Partner's education-single years"
# v717     "Respondent's occupation"
# v130     "Religion"
# v025     "Type of place of residence"
# v206     "Sons who have died"
# v207     "Daughters who have died"

# nigeriaraw$v445[nigeriaraw$v445 >= 9998] <- NA
nigeriaraw$v445 <- nigeriaraw$v445/100
# nigeriaraw$v437[nigeriaraw$v437 >= 9994] <- NA
nigeriaraw$v437 <- nigeriaraw$v437/10
# nigeriaraw$v438[nigeriaraw$v438 >= 9994] <- NA
nigeriaraw$v438 <- nigeriaraw$v438/10
nigeriaraw$mage <- round((nigeriaraw$b3-nigeriaraw$v011)/12, 0)
nigeriaraw$munemployed <- as.numeric(nigeriaraw$v717)
nigeriaraw$munemployed[nigeriaraw$munemployed %in% 13] <- NA
nigeriaraw$munemployed[nigeriaraw$munemployed>0] <- 1
nigeriaraw$munemployed <- factor(nigeriaraw$munemployed, levels = c(0,1), labels=c("unemployed","employed"))
#nigeriaraw$cunemployed <- recode(nigeriaraw$v717, "c('prof., tech., manag.','clerical','sales','agric-self employed','agric-employee','household & domestic','services','skilled & unskilled manual','[for nfhs-3 skilled and unskilled manual combined]','unskilled','don\'t know')='working'")
nigeriaraw$mreligion <- nigeriaraw$v130
Code <- levels(factor(nigeriaraw$v130))
nam <- names(attributes(nigeriaraw)$label.table$V130)
nigeriaraw$mreligion[nigeriaraw$mreligion == 99] <- NA
nam <- c("Catholic","other Christian", "Muslim", "other", "other")
nigeriaraw$mreligion <- nam[match(nigeriaraw$v130, as.numeric(Code))]
nigeriaraw$deadchildren <- nigeriaraw$v206 + nigeriaraw$v207

# nigeriaraw$v715[nigeriaraw$v715 >= 97] <- NA

vars <- c(vars, c("v445", "v437", "v438", "mage", "v133", "v715", "munemployed", "mreligion", "v025", "deadchildren"))
nams <- c(nams, c("mbmi", "mweight", "mheight", "mage", "medu", "edupartner", "munemployed", "mreligion", "mresidence", "deadchildren"))


# v190     "Wealth index"
# v119     "Has electricity"
# v120     "Has radio"
# v121     "Has television"
# v122     "Has refrigerator"
# v123     "Has bicycle"
# v124     "Has motorcycle/scooter"
# v125     "Has car"
vars <- c(vars, c("v190", "v119", "v120", "v121", "v122", "v123", "v124", "v125"))
nams <- c(nams, c("wealth", "electricity", "radio", "television", "refrigerator", "bicycle", "motorcycle", "car"))

## Spatial information

# v024    "Region 6 district"
# v023    "Base for region 37 district"

library(shapefiles)
nigeria37 <- read.dbf("01_raw-data/sdr_subnational_boundaries.dbf", header=FALSE)

# match for 37 district map
REGEN <- nigeriaraw$v023
REGEN <- substr(REGEN, 4, nchar(as.character(REGEN))-6)
noMatchREGEN <- unique(REGEN)[!unique(REGEN) %in% unique(nigeria37$dbf$DHSREGEN)]
noMatchMap <- as.character(unique(nigeria37$dbf$DHSREGEN)[!unique(nigeria37$dbf$DHSREGEN) %in% unique(REGEN)])
c(noMatchMap, noMatchREGEN)
REGEN[REGEN == noMatchREGEN] <- noMatchMap

nigeriaraw$REGEN <- REGEN


vars <- c(vars, c("REGEN"))
nams <- c(nams, c("DHSREGEN37"))



nigeria <- nigeriaraw[,vars]
names(nigeria) <- nams


#######
# plausibility checks

hist(nigeria$cweight)
nigeria$cweight[nigeria$cweight>25] <- NA
hist(nigeria$cheight)
nigeria$cheight[nigeria$cheight>140] <- NA
hist(nigeria$stunting)
nigeria$stunting[nigeria$stunting>2000] <- NA
hist(nigeria$wasting)
nigeria$wasting[nigeria$wasting>2000] <- NA
hist(nigeria$underweight)
nigeria$underweight[nigeria$underweight>2000] <- NA

hist(nigeria$cbmi)
nigeria$cbmi[nigeria$cbmi>26] <- NA
nigeria$cbmi[nigeria$cbmi<10] <- NA
plot(nigeria$cbmi, nigeria$stunting)

hist(nigeria$cage)
plot(nigeria$cage, nigeria$stunting)

table(nigeria$breastfeeding)
plot(nigeria$breastfeeding, nigeria$stunting)
sum(nigeria$breastfeeding > nigeria$cage, na.rm=T)

table(nigeria$csex)
plot(nigeria$csex, nigeria$stunting)

table(nigeria$ctwin)
plot(nigeria$ctwin, nigeria$stunting)

table(nigeria$cbirthorder)
nigeria$cbirthorder[nigeria$cbirthorder>5] <- 5
nigeria$cbirthorder <- factor(nigeria$cbirthorder)
plot(nigeria$cbirthorder, nigeria$stunting)

hist(nigeria$mbmi)
nigeria$mbmi[nigeria$mbmi>40] <- NA
plot(nigeria$mbmi, nigeria$stunting)

hist(nigeria$mweight)
nigeria$mweight[nigeria$mweight>120] <- NA
nigeria$mweight[nigeria$mweight<20] <- NA
plot(nigeria$mweight, nigeria$stunting)

hist(nigeria$mheight)
nigeria$mheight[nigeria$mheight>185] <- NA
nigeria$mheight[nigeria$mheight<123] <- NA
plot(nigeria$mheight, nigeria$stunting)

hist(nigeria$mage)
plot(nigeria$mage, nigeria$stunting)

hist(nigeria$medu)
nigeria$medu[nigeria$medu == 99] <- NA
plot(nigeria$medu, nigeria$stunting)

hist(nigeria$edupartner)
nigeria$edupartner[nigeria$edupartner>80] <- NA
plot(nigeria$edupartner, nigeria$stunting)

table(nigeria$munemployed)
plot(nigeria$munemployed, nigeria$stunting)

table(nigeria$mreligion)
plot(as.factor(nigeria$mreligion), nigeria$stunting)

table(nigeria$mresidence)
plot(nigeria$mresidence, nigeria$stunting)

table(nigeria$deadchildren)
nigeria$deadchildren[nigeria$deadchildren>3] <- 3
nigeria$deadchildren <- factor(nigeria$deadchildren)
plot(nigeria$deadchildren, nigeria$stunting)

table(nigeria$wealth)
plot(nigeria$wealth, nigeria$stunting)

table(nigeria$electricity)
nigeria$electricity[nigeria$electricity %in% c(7,9)] <- NA
# nigeria$electricity[nigeria$electricity=="not a dejure resident"] <- NA
nigeria$electricity<- factor(nigeria$electricity, levels = c(0,1), labels=c("no","yes"))
plot(nigeria$electricity, nigeria$stunting)

table(nigeria$radio)
nigeria$radio[nigeria$radio %in% c(7,9)] <- NA
# nigeria$radio[nigeria$radio=="not a dejure resident"] <- NA
nigeria$radio<- factor(nigeria$radio, levels = c(0,1), labels=c("no","yes"))
plot(nigeria$radio, nigeria$stunting)

table(nigeria$television)
nigeria$television[nigeria$television %in% c(7,9)] <- NA
# nigeria$television[nigeria$television=="not a dejure resident"] <- NA
nigeria$television<- factor(nigeria$television, levels = c(0,1), labels=c("no","yes"))
plot(nigeria$television, nigeria$stunting)

table(nigeria$refrigerator)
nigeria$refrigerator[nigeria$refrigerator  %in% c(7,9)] <- NA
# nigeria$refrigerator[nigeria$refrigerator=="not a dejure resident"] <- NA
nigeria$refrigerator<- factor(nigeria$refrigerator, levels = c(0,1), labels=c("no","yes"))
plot(nigeria$refrigerator, nigeria$stunting)

table(nigeria$bicycle)
nigeria$bicycle[nigeria$bicycle %in% c(7,9)] <- NA
# nigeria$bicycle[nigeria$bicycle=="not a dejure resident"] <- NA
nigeria$bicycle<- factor(nigeria$bicycle, levels = c(0,1), labels=c("no","yes"))
plot(nigeria$bicycle, nigeria$stunting)

table(nigeria$motorcycle)
nigeria$motorcycle[nigeria$motorcycle %in% c(7,9)] <- NA
# nigeria$motorcycle[nigeria$motorcycle=="not a dejure resident"] <- NA
nigeria$motorcycle<- factor(nigeria$motorcycle, levels = c(0,1), labels=c("no","yes"))
plot(nigeria$motorcycle, nigeria$stunting)

table(nigeria$car)
nigeria$car[nigeria$car %in% c(7,9)] <- NA
# nigeria$car[nigeria$car=="not a dejure resident"] <- NA
nigeria$car<- factor(nigeria$car, levels = c(0,1), labels=c("no","yes"))
plot(nigeria$car, nigeria$stunting)

nigeria <- nigeria[complete.cases(nigeria),]

nigeria$intercept <- 1

center <- function(x)
  return(x-mean(x))

nigeria$cagec <- center(nigeria$cage)
nigeria$magec <- center(nigeria$mage)
nigeria$mbmic <- center(nigeria$mbmi)
nigeria$meduc <- center(nigeria$medu)
nigeria$breastfeedingc <- center(nigeria$breastfeeding)
nigeria$eduparterc <- center(nigeria$edupartner)

write.table(nigeria, "./02_data/nigeria.raw", col.names=TRUE)

rm(list = grep(paste(env.pre, collapse = "|"), ls(), invert = T, value = T))
