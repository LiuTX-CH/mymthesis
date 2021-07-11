
#' Recoding variables using Welzel(2011)'s methods in data
#'
#' @param data A dataframe including variables names "gen1","gen2","gen3","peo1","peo2","peo3","peo4","lif1","lif2","lif3"
#'
#' @return A dataframe including the recoded variables above
#' @export
#'
#'
recoding <- function(data) {
  for (var in c("gen1","gen2","gen3",
                "peo1","peo2","peo3","peo4",
                "lif1","lif2","lif3",
                "per1","per2","per3")) {
    data[[var]] <- as.numeric(data[[var]])
  }
  data$gen1[data$gen1 == 1] <- 0
  data$gen1[data$gen1 == 2] <- 1
  data$gen1[data$gen1 == 3] <- 0.5

  data$gen2[data$gen2 == 1] <- 0
  data$gen2[data$gen2 == 2] <- 0.33
  data$gen2[data$gen2 == 3] <- 0.66
  data$gen2[data$gen2 == 4] <- 1

  data$gen3[data$gen3 == 1] <- 0
  data$gen3[data$gen3 == 2] <- 0.33
  data$gen3[data$gen3 == 3] <- 0.66
  data$gen3[data$gen3 == 4] <- 1

  data$peo1g <- ifelse(data$peo3 == 2,1,
                            ifelse(data$peo4 == 2,0.5,0))
  data$peo2p <- ifelse(data$peo3 == 4,1,
                            ifelse(data$peo4 == 4,0.5,0))
  data$peo3g <- ifelse(data$peo1 == 3,1,
                            ifelse(data$peo2 == 3,0.5,0))

  data$lif1 <- 0.1*data$lif1 - 0.01*(11 - data$lif1)
  data$lif1[data$lif1 == 0.99] <- 1
  data$lif2 <- 0.1*data$lif2 - 0.01*(11 - data$lif2)
  data$lif2[data$lif2 == 0.99] <- 1
  data$lif3 <- 0.1*data$lif3 - 0.01*(11 - data$lif3)
  data$lif3[data$lif3 == 0.99] <- 1

  return(data)
}
