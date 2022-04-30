## This is a function to create the estimates for complex-survey designs.
## Funcion_TEV03.R change name to survey_extra_tools.R
## version V01: Ballesteros
## version V02: Ballesteros-Becerril
## version V03: Ballesteros-Chávez - Becerril-Cejudo - López-Cruz © 2019
## survey_extra_tools.R: Ballesteros-Chávez - Becerril-Cejudo - López-Cruz © 2021
## Licence: https://www.gnu.org/licenses/gpl-3.0.html


## R-Packages needed: survey.

## Usage:

## Definition of the weights

## df$W  <- df$Variable.Weights

## ## Definition of  PSU
## df$PSU  <- df$Variable.PSU

## ## Definitinon of Strata
## df$STR  <-  df$Variable.Strata

## ## Definition of the LEVELS
## df$LEVELS  <-  df$Variable.Levels

## varnames  <- c("AP4_3_3")
## codes  <- c(1,2,9)

## denovarnames  <- c("AP4_3_3")
## denocodes  <- c(1,2,9)

## filename  <-  "/path/to/outputfilename"

## mydesign <- svydesign(id=~PSU,strata=~STR,data=df,weights=~W)

## For examples see the README.org file


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




A.estimator  <-  function(x=varnames,y=codes,w=denovarnames,z=denocodes,datadesign = mydesign, rate=1, file.out=filename, write.file=FALSE){

## Denominators
denominators  <- Variable.creation(var.names=w,var.values=z,data.set=df,label="Deno00")
df  <- XX.result
mydesign  <-  svydesign(id=~PSU,strata=~STR,data=XX.result,weight=~W)
TT  <-  lapply(denominators,function(x) T.estimator(x,"LEVELS",mydesign))

#Totals
YY  <- lapply(codes,function(i){
Variable.creation(var.names=x,var.values=i,data.set=df,label=paste0(i))

mydesign  <-  svydesign(id=~PSU,strata=~STR,data=XX.result,weight=~W)
result.total  <-  lapply(XX.newnames,function(x) T.estimator(x,"LEVELS",mydesign))
})

#Ratios
ZZ  <- lapply(codes,function(i){
Variable.creation(var.names=x,var.values=i,data.set=df,label=paste0(i))

mydesign  <-  svydesign(id=~PSU,strata=~STR,data=XX.result,weight=~W)
result.means  <-  lapply(XX.newnames,function(x) R.estimator(x,denominators[which(XX.newnames%in%x)],"LEVELS",mydesign))

})

## Printing procedure
TT  <- unlist(TT,recursive=FALSE)
YY  <- unlist(unlist(YY,recursive=FALSE),recursive=FALSE)
ZZ  <- unlist(unlist(ZZ,recursive=FALSE),recursive=FALSE)

Out1  <- mapply(cbind,TT,YY,ZZ,SIMPLIFY=FALSE)
Out2  <- lapply(Out1,function(x){cbind("NAME"=names(x)[8],x,stringsAsFactors=FALSE)})
std.names  <-  c("NAME","LEVELS","VALUE","SE","CV","INF.95","SUP.95")
std.names19  <- c(std.names[1],paste(std.names[2:7],"denominator",sep="."),paste(std.names[2:7],"total",sep="."),paste(std.names[2:7],"mean",sep="."))
Out3  <- lapply(Out2,function(x){colnames(x)  <-  std.names19; x})
Out3  <- lapply(Out3,function(x){
x[,2]  <- as.character(x[,2]);
x[,8]  <- as.character(x[,8]);
x[,14]  <- as.character(x[,14]);
x})
    
    Out  <- do.call(rbind,c(Out3,make.row.names = FALSE))
## By default rate == 1, rate = 100 is a percentage, rate = 100000 a rate per*/hab.

    Out$VALUE.mean  <- Out$VALUE.mean * rate
    print(paste0("you are using a rate value for the mean of: ",rate))
    Out$rate.info  <- rate
    Out$metadata1  <- metadata1
    Out$metadata2  <- metadata2
    Out$codes.val  <- sub(".*\\.", "", Out$NAME)
    
    if(write.file){
        ## --- Print Results:
        x  <- as.character(format(Sys.time(), "%a_%b_%d_%H%M%S%Y")) 
        write.csv(Out, paste0(file.out,x,".csv"))
        return(Out)
#        write(paste0(file.out,x,".csv"), stdout())

    }
    else{
        return(Out)
    }
}


