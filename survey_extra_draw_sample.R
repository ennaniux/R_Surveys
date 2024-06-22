## Daniel Ballesteros-Chávez
## Some tools to perform sampling:
## data is a data.frame
## "PSU_ID" and "EST" are assumed for Primary Sampling Unite ID and Strata

###################################
## Simple Random Sampling (no repetition, you can modify accordingly)
###################################
SimSamp <- function(data,m){
NN  <-  nrow(data)
MM   <- m      
PProb  <-  MM/NN
FFac  <- 1/PProb
SimpleSample <- sample(data$PSU_ID,MM)
data$Group  <-  ifelse(data$PSU_ID%in%SimpleSample,"G1","G0")
data$Prob  <-  ifelse(data$PSU_ID%in%SimpleSample,PProb,NA)
data$Fac  <-  ifelse(data$PSU_ID%in%SimpleSample,FFac,NA)
return(data)
}

## The output is data with added variables:
## Group: "G1 - Selected" "G0 - Not selected"
## Prob: Probability of inclusion
## Fac: Weight as inverse of the probability of inclusion.

###################################
## Stratified Simple Sampling
###################################
## Draws the same number of PSU within each Stratum
## The output is a list for first review.
SimSamp.STR  <- function(data,m){
VEST <- sort(unique(data$EST))
MM <- m
YY  <- lapply(VEST, function(i){
XX  <- data[data$EST%in%i,]
SimSamp(XX,MM)
})
return(YY)
}

###################################
## Stratified Simple Sampling Proportional to Size of Strata
###################################
## Draws a sample of size m PSU, proportionally to the size of the Strata
## The output is a list for first review.
SimSamp.STR.PP  <- function(data,m){
VEST <- sort(unique(data$EST))
NN <- nrow(data)
YY  <- lapply(VEST, function(i){
XX  <- data[data$EST%in%i,]
poppr  <- nrow(XX)/NN
MM <- round(m*poppr)
SimSamp(XX,MM)
})
return(YY)
}
