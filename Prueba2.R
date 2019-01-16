## Prueba 2
## <estratificacion

library(foreign)
library(stratification)
source("sistematico.R")

Base <- read.dbf("prueba.dbf")



# La variable de estratificacion siempre en la tercera columna.

# Numero de clases con Sturges

Kclases <- (1+3.3*log(length(Base[,3]),10))


cum2 <- strata.cumrootf(x=Base[,3],CV=.07,Ls=2,nclass = round(Kclases))


#cum3 <- strata.cumrootf(x=Base[3],CV=.05,Ls=3,nclass = round(Rango/AmpliR) )
#cum4 <- strata.cumrootf(x=Base[3],CV=.05,Ls=4,nclass = round(Rango/AmpliR))
#############################################hasta aqui ok
Base$ESTRATO <- ifelse(
  Base[,3]<= cum2$bh,1,2)

Muestra1 <- Base[Base$ESTRATO%in%1,]
Muestra1 <- Muestra1[c(sys.sample(nrow(Muestra1),cum2$nh[1])),]

Muestra2<- Base[Base$ESTRATO%in%2,]
Muestra2 <- Muestra1[sys.sample(nrow(Muestra2),cum2$nh[2]),]

Muestra <- rbind(Muestra1,Muestra2)
