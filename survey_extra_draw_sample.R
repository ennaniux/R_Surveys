## Daniel Ballesteros-Chávez

library(foreign)

## From a data base, Draw a Sample for a survey with weights.

source("/home/daniel/Documents/org/norgtes/nDataAnalysis/myR_utils/myutilities.R")

## MG_INEGI_2020

dfent  <- read.dbf("/home/daniel/Documents/MG_INEGI2020/MG_2020_Integrado/conjunto_de_datos/00ent.dbf")
dfmun  <- read.dbf("/home/daniel/Documents/MG_INEGI2020/MG_2020_Integrado/conjunto_de_datos/00mun.dbf")
dfageb  <- read.dbf("/home/daniel/Documents/MG_INEGI2020/MG_2020_Integrado/conjunto_de_datos/00a.dbf")

dframe  <-  read.csv("/home/daniel/Documents/MG_INEGI2020/Viviendas00.CSV")


### Extraccion de una muestra:
## It requires the following information:
## Let DF be a data frame with the following characteristics:
## DF$EST estratum variable (say DF = UPM, EST)
## Following steps:
DF  <- dfageb
DF$VARIABLE  <-  paste0(DF$CVE_ENT , DF$Ambito)
DF$VARIABLE  <-  as.numeric(as.factor(DF$VARIABLE))

out.strat  <-  mystratcv(DF$VARIABLE,cv = 0.008,ls=4)
## DF2  <-  cbind(DF,"EST"=out.strat$stratumID)
## ## Lets see if now we can estract the sample:
## DF3  <-  DF2[order(DF2$EST),]
## ##there is something doggie with the first one:
## ## DF3  <-  DF3[-1,]
## out.mysample <- strata(DF3, stratanames=c("EST"),out.strat$nh, method="systematic",pik=DF3$HPOP,description=TRUE)





