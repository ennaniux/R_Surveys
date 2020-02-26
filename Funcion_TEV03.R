## This is a function to create the estimates for complex-survey designs.
## version V01: Ballesteros
## version V02: Ballesteros-Becerril
## version V03: Ballesteros-Chávez - Becerril-Cejudo - López-Cruz © 2019
## Licence: https://www.gnu.org/licenses/gpl-3.0.html
## mydesign <- svydesign(id=~ID_CONSECU,strata=~ID_ESTRATO,data=BD1,weights=~FAC_EXPAN1)

## R-Packages needed: survey.

T.estimator <- function(x,y,data=NULL,Vacio=TRUE,lev=0.95,mydeff=FALSE){
sapply(x,function(x){
XX <- svyby(eval(parse(text=paste0("~",x))),by=eval(parse(text=paste0("~",y))),data,svytotal,na.rm=Vacio,deff=mydeff)
YY <- svytotal(eval(parse(text=paste0("~",x))),data,na.rm=Vacio,deff=mydeff)
if(mydeff==TRUE){
Nacional <-    c(NA,mean(YY),SE(YY)[1,1],deff(YY),cv(YY)[1,1],confint(YY,level=lev)[1,1],confint(YY,level=lev)[1,2])
}else{
Nacional <-    c(NA,mean(YY),SE(YY)[1,1],cv(YY)[1,1],confint(YY,level=lev)[1,1],confint(YY,level=lev)[1,2])
}

ZZ <- rbind(
   Nacional,
cbind(data.frame(XX), "CV"=cv(XX),
data.frame("Inf.95"=matrix(confint(XX,level=lev)[,1],nrow=nrow(XX),byrow=FALSE)),
data.frame("Sup.95"=matrix(confint(XX,level=lev)[,2],nrow=nrow(XX),byrow=FALSE))))
levels(ZZ[,1])  <- c(levels(ZZ[,1]),"Nacional")
ZZ[1,1]  <- "Nacional"
if(mydeff==TRUE){
	names(ZZ)[6:7] <- c(paste0("Inf.",lev),paste0("Sup.",lev))
	       }else{
	names(ZZ)[5:6] <- c(paste0("Inf.",lev),paste0("Sup.",lev))
	       }
ZZ},
simplify=FALSE)
}


M.estimator <- function(x,y,data=NULL,Vacio=TRUE,lev=0.95,mydeff=FALSE){
sapply(x,function(x){
XX <- svyby(eval(parse(text=paste0("~",x))),by=eval(parse(text=paste0("~",y))),data,svymean,na.rm=Vacio,deff=mydeff)
YY <- svymean(eval(parse(text=paste0("~",x))),data,na.rm=Vacio,deff=mydeff)
if(mydeff==TRUE){
Nacional <-    c(NA,mean(YY),SE(YY)[1,1],deff(YY),cv(YY)[1,1],confint(YY,level=lev)[1,1],confint(YY,level=lev)[1,2])
}else{
Nacional <-    c(NA,mean(YY),SE(YY)[1,1],cv(YY)[1,1],confint(YY,level=lev)[1,1],confint(YY,level=lev)[1,2])
}

ZZ <- rbind(
Nacional,
cbind(data.frame(XX), "CV"=cv(XX),
data.frame("Inf.95"=matrix(confint(XX,level=lev)[,1],nrow=nrow(XX),byrow=FALSE)),
data.frame("Sup.95"=matrix(confint(XX,level=lev)[,2],nrow=nrow(XX),byrow=FALSE))))
levels(ZZ[,1])  <- c(levels(ZZ[,1]),"Nacional")
ZZ[1,1]  <- "Nacional"
	       if(mydeff==TRUE){
	names(ZZ)[6:7] <- c(paste0("Inf.",lev),paste0("Sup.",lev))
	       }else{
	names(ZZ)[5:6] <- c(paste0("Inf.",lev),paste0("Sup.",lev))
	       }
ZZ},
simplify=FALSE)
}



R.estimator <- function(x,z,y=NULL,data=NULL,Vacio=TRUE,lev=0.95,mydeff=FALSE){
	sapply(x,function(x){
	       XX <- svyby(eval(parse(text=paste0("~",x))),denominator=eval(parse(text=paste0("~",z))) ,by=eval(parse(text=paste0("~",y))),data,svyratio,na.rm=Vacio,deff=mydeff)
	       YY <- svyratio(eval(parse(text=paste0("~",x))),denominator=eval(parse(text=paste0("~",z))),data,na.rm=Vacio,deff=mydeff)
	 
if(mydeff==TRUE){
Nacional <-    c(NA,YY$ratio[1,1],mean(SE(YY)),deff(YY),cv(YY)[1,1],confint(YY,level=lev)[1,1],confint(YY,level=lev)[1,2])
}else{
Nacional <-    c(NA,YY$ratio[1,1],mean(SE(YY)),cv(YY)[1,1],confint(YY,level=lev)[1,1],confint(YY,level=lev)[1,2])
}



               ZZ <- rbind(
                   Nacional,
                   cbind(data.frame(XX), "CV"=cv(XX),
                         data.frame("Inf.95"=matrix(confint(XX,level=lev)[,1],nrow=nrow(XX),byrow=FALSE)),
                         data.frame("Sup.95"=matrix(confint(XX,level=lev)[,2],nrow=nrow(XX),byrow=FALSE))))
levels(ZZ[,1])  <- c(levels(ZZ[,1]),"Nacional")
ZZ[1,1]  <- "Nacional"
	       if(mydeff==TRUE){
                   names(ZZ)[6:7] <- c(paste0("Inf.",lev),paste0("Sup.",lev))
	       }else{
                   names(ZZ)[5:6] <- c(paste0("Inf.",lev),paste0("Sup.",lev))
	       }
               ZZ},
	       simplify=FALSE)
    }





## Create Binary variables
## df$variable.var.names.00 <- ifelse(df$var.names%in%var.values,1,0)

Variable.creation  <-  function(var.names, var.values,data.set = NULL , label="00",neg=FALSE){
    if(is.null(data.set)== TRUE){print("WARNING: You need to provide the data frame")}
    else{
        if(neg==FALSE){
            for (i in 1:length(var.names)){
                newvar  <-  paste("variable",var.names[i],label,sep=".")
                data.set[[newvar]]  <-  ifelse(data.set[,var.names[i]]%in% var.values, 1,0)
            }
        } else {
            for (i in 1:length(var.names)){
                newvar  <-  paste("variable",var.names[i],label,sep=".")
                data.set[[newvar]]  <-  ifelse(!data.set[,var.names[i]]%in% var.values, 1,0)
            }

        }
        XX.result <<-data.set
        XX.newnames <<- paste("variable",var.names,label,sep=".")
    }
}

