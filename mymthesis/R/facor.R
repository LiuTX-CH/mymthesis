#' Principle analysis and correlation test result
#'
#' @param data A data frame used to compare
#' @param lif A vector of variable names for latent variable "life tolerance"
#' @param peo A vector of variable names for latent variable "people's voice"
#' @param gen A vector of variable names for latent variable "gender equality"
#' @param per A vector of variable names for latent variable "personal autonomy"
#' @param ev A vector of variable names for latent variable "emancipative values"
#'
#' @return A list of two components. The first is a data frame used to create correlation plot. The second is a list consist of results of principle components analysis, including KMO, Cronbach's alpha and loadings
#' @export
#'

facor <- function(data, lif = c("lif1","lif2","lif3"),
                  peo = c("peo1g","peo2p","peo3g"), gen = c("gen1","gen2","gen3"),
                  per = c("per1","per2","per3"), ev = c("gen","peo","lif","per")){
  for (j in c(gen,lif,per)) {
    data[[j]] <- as.numeric(data[[j]])
  }
  wvc2 <- dplyr::filter(data, wave == 2)
  wvc3 <- dplyr::filter(data, wave == 3)
  wvc4 <- dplyr::filter(data, wave == 4)
  wvc5 <- dplyr::filter(data, wave == 5)
  wvc6 <- dplyr::filter(data, wave == 6)
  wvc7 <- dplyr::filter(data, wave == 7)
  wvc <- list(data,wvc2,wvc3,wvc4,wvc5,wvc6,wvc7)

  cordata <- data.frame(est = rep(NA,7),
                        lwr = rep(NA,7),
                        upr = rep(NA,7),
                        typ = c(1,rep(2,6)),
                        x = 1:7,
                        obser = rep(NA,7),
                        observa = c(length(data$wave),
                                    length(wvc2$wave),
                                    length(wvc3$wave),
                                    length(wvc4$wave),
                                    length(wvc5$wave),
                                    length(wvc6$wave),
                                    length(wvc7$wave)),
                        proportion = rep(NA,7))
  princp <- rep(list(lif = list(),
                  gen = list(),
                  peo = list(),
                  per = list(),
                  ev = list()),6)
  n <- 1
  for (i in 1:7) {
    sub.lif <- wvc[[i]][,c(which(names(wvc[[i]]) == lif[1]),
                           which(names(wvc[[i]]) == lif[2]),
                           which(names(wvc[[i]]) == lif[3]))]
    pca.lif <- psych::principal(sub.lif, scores = T, rotate = "oblimin")
    wvc[[i]]$lif <- pca.lif$scores

    sub.peo <- wvc[[i]][,c(which(names(wvc[[i]]) == peo[1]),
                           which(names(wvc[[i]]) == peo[2]),
                           which(names(wvc[[i]]) == peo[3]))]
    pca.peo <- psych::principal(sub.peo, scores = T, rotate = "oblimin")
    wvc[[i]]$peo <- pca.peo$scores

    if(i == 2){
      wvc[[i]]$gen <- wvc[[i]]$gen1
    } else {
      sub.gen <- wvc[[i]][,c(which(names(wvc[[i]]) == gen[1]),
                             which(names(wvc[[i]]) == gen[2]),
                             which(names(wvc[[i]]) == gen[3]))]
      pca.gen <- psych::principal(sub.gen, scores = T, rotate = "oblimin")
      wvc[[i]]$gen <- pca.gen$scores}

    sub.per <- wvc[[i]][,c(which(names(wvc[[i]]) == per[1]),
                           which(names(wvc[[i]]) == per[2]),
                           which(names(wvc[[i]]) == per[3]))]
    pca.per <- psych::principal(sub.per, scores = T, rotate = "oblimin")
    wvc[[i]]$per <- pca.per$scores

    sub.ev <- wvc[[i]][,c(which(names(wvc[[i]]) == ev[1]),
                          which(names(wvc[[i]]) == ev[2]),
                          which(names(wvc[[i]]) == ev[3]),
                          which(names(wvc[[i]]) == ev[4]))]
    pca.ev <- psych::principal(sub.ev, scores = T, rotate = "oblimin")
    wvc[[i]]$ev <- pca.ev$scores

    cordata[n,1] <- cor.test(wvc[[i]]$ev, wvc[[i]]$Welzelev)$estimate[[1]]
    cordata[n,2] <- cor.test(wvc[[i]]$ev, wvc[[i]]$Welzelev)$conf.int[1]
    cordata[n,3] <- cor.test(wvc[[i]]$ev, wvc[[i]]$Welzelev)$conf.int[2]
    cordata[n,6] <- cor.test(wvc[[i]]$ev, wvc[[i]]$Welzelev)$parameter[[1]]
    cordata[n,8] <- (cordata[n,7] - cordata[n,6])/cordata[n,7]

    wvtotal <- list(lif = sub.lif, gen = sub.gen, peo = sub.peo, per = sub.per, ev = sub.ev)
    print(paste("loop",n,"(wave, 7 loops in total)"))
    for (v in c("lif","gen","peo","per","ev")) {
      if (i == 2 & v == "gen"){
        princp[[i]][[v]][["kmo"]] <- 999
        princp[[i]][[v]][["alpha"]] <- 999
        princp[[i]][[v]][["pca"]] <- 999
        print(paste("  sub-loop",2,"(variable, 5 loops in total)"))
      } else {
        princp[[i]][[v]][["kmo"]] <- psych::KMO(wvtotal[[v]])$MSA
        princp[[i]][[v]][["alpha"]] <- suppressMessages(psych::alpha(wvtotal[[v]],check.keys = T)$total[[1]])
        princp[[i]][[v]][["pca"]] <- psych::principal(wvtotal[[v]])$loadings
        print(paste("  sub-loop",which(c("lif","gen","peo","per","ev")==v),"(variable, 5 loops in total)"))
      }
    }
    n <- n + 1
  }
  result <- list(cordata,princp)
  return(result)
}
