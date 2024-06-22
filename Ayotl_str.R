library(sampling)

data <- data.frame(
    ID = 1:400,
    Level = rep(c("freshers", "juniors", "mid-level", "Senior"),
    times=c(100,100,150, 50)),
    Score = rnorm(400, mean=45, sd=2.2))

m.sss.pp <- function(psu=1,st=2,size,data=data){

}
    
