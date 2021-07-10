#' Missing data totally in some groups calculation.
#'
#' @param data A data frame with missing values
#' @param varnames A vector of characters consists of names of those variables that need to calculate.
#' @param group A character of group variable name.
#'
#' @return a data frame with missing value proportion.
#' @export
#'
misintt <- function(data, varnames, group){
  mip <- function(var){
    return(sum(is.na(var))/length(var))
  }
  aggregate(data[varnames], list(data[[group]]), mip)
}
