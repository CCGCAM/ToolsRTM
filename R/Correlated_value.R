#' Corretalion function
#' This function generates a new variable that will be correlated with  x variable. The new variable is fixed by a normal distribution function 
#' @param x numeric. Main variable in a vector format 
#' @param r value of correlation coefficient 
#'
#' @return second variable based on the asigned correlation of the main variable in a vector format 
#' @export
#'
#' @examples
#' 
correlatedValue = function(x, r){
  r2 = r**2
  ve = 1-r2
  SD = sqrt(ve)
  e  = rnorm(length(x), mean=0, sd=SD)
  y  = r*x + e
  return(y)
}