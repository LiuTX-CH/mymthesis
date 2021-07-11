#' Random effects summary of simulations
#'
#' @param simfix A data frame created by arm::sim(), the fixed effect in the result.
#' @param simran A data frame created by arm::sim(), the random effect in the result.
#' @param varfix A character of a variable name used in the fixed effect.
#' @param varran A vector of a set of variable names used in the random effect.
#' @param interval A number of confidence interval value.
#' @param interaction A logical value to confirm whether there exists a interaction. Default is False.
#' @param movarfix A character of a variable name used in the fixed effect containing modifier variable name.
#' @param movarran A character of a variable name used in the random effect containing modifier variable name.
#' @param factorname A vector of factor's name as for Chinese-English transformation. Default is c("non-Confucian Societies","Confucian Societies").
#'
#' @return A data frame
#' @export
#'
simdata <- function(simfix,simran,varfix,varran,interval,interaction=F,movarfix,movarran,factorname=c("non-Confucian Societies","Confucian Societies")){
  beta22 <- simfix[[varfix]] + simran[[varran[1]]]
  beta23 <- simfix[[varfix]] + simran[[varran[2]]]
  beta24 <- simfix[[varfix]] + simran[[varran[3]]]
  beta25 <- simfix[[varfix]] + simran[[varran[4]]]
  beta26 <- simfix[[varfix]] + simran[[varran[5]]]
  beta27 <- simfix[[varfix]] + simran[[varran[6]]]

  if (interaction == F){
    simdat <- data.frame(mean = c(mean(beta22),
                                  mean(beta23),
                                  mean(beta24),
                                  mean(beta25),
                                  mean(beta26),
                                  mean(beta27)),
                       lwr95 = c(quantile(beta22,(1-interval)/2),
                                 quantile(beta23,(1-interval)/2),
                                 quantile(beta24,(1-interval)/2),
                                 quantile(beta25,(1-interval)/2),
                                 quantile(beta26,(1-interval)/2),
                                 quantile(beta27,(1-interval)/2)),
                       upr95 = c(quantile(beta22,(1+interval)/2),
                                 quantile(beta23,(1+interval)/2),
                                 quantile(beta24,(1+interval)/2),
                                 quantile(beta25,(1+interval)/2),
                                 quantile(beta26,(1+interval)/2),
                                 quantile(beta27,(1+interval)/2)),
                       wave = 2:7)
  } else {
    beta32 <- simfix[[movarfix]] + simran[[movarran[1]]]
    beta33 <- simfix[[movarfix]] + simran[[movarran[2]]]
    beta34 <- simfix[[movarfix]] + simran[[movarran[3]]]
    beta35 <- simfix[[movarfix]] + simran[[movarran[4]]]
    beta36 <- simfix[[movarfix]] + simran[[movarran[5]]]
    beta37 <- simfix[[movarfix]] + simran[[movarran[6]]]

    beta2p32 <- beta22 + beta32
    beta2p33 <- beta23 + beta33
    beta2p34 <- beta24 + beta34
    beta2p35 <- beta25 + beta35
    beta2p36 <- beta26 + beta36
    beta2p37 <- beta27 + beta37

    simdat <- data.frame(mean = c(mean(beta22),
                                  mean(beta23),
                                  mean(beta24),
                                  mean(beta25),
                                  mean(beta26),
                                  mean(beta27),
                                  mean(beta2p32),
                                  mean(beta2p33),
                                  mean(beta2p34),
                                  mean(beta2p35),
                                  mean(beta2p36),
                                  mean(beta2p37)),
                         lwr95 = c(quantile(beta22,(1-interval)/2),
                                   quantile(beta23,(1-interval)/2),
                                   quantile(beta24,(1-interval)/2),
                                   quantile(beta25,(1-interval)/2),
                                   quantile(beta26,(1-interval)/2),
                                   quantile(beta27,(1-interval)/2),
                                   quantile(beta2p32,(1-interval)/2),
                                   quantile(beta2p33,(1-interval)/2),
                                   quantile(beta2p34,(1-interval)/2),
                                   quantile(beta2p35,(1-interval)/2),
                                   quantile(beta2p36,(1-interval)/2),
                                   quantile(beta2p37,(1-interval)/2)),
                         upr95 = c(quantile(beta22,(1+interval)/2),
                                   quantile(beta23,(1+interval)/2),
                                   quantile(beta24,(1+interval)/2),
                                   quantile(beta25,(1+interval)/2),
                                   quantile(beta26,(1+interval)/2),
                                   quantile(beta27,(1+interval)/2),
                                   quantile(beta2p32,(1+interval)/2),
                                   quantile(beta2p33,(1+interval)/2),
                                   quantile(beta2p34,(1+interval)/2),
                                   quantile(beta2p35,(1+interval)/2),
                                   quantile(beta2p36,(1+interval)/2),
                                   quantile(beta2p37,(1+interval)/2)),
                         wave = c(2:7-0.05,2:7+0.05),
                         type = rep(factorname, each = 6))
  }
  return(simdat)
}
