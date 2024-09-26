#'  Stefan-Boltzmann equation
#'
#' @param T_C
#'
#' @return
#' @export
#'
#' @examples
#'
get.Stefan_Boltzmann<-function(T_C){

  #if (!require("SCOPEinR")) {
  #  message("The 'SCOPEinR' package is not installed. Please install it using install.packages('ggplot2')")
  #}
  Kelvin_temp <- subset(constants,constant == 'C2K')[[2]]
  sigmaSB <- subset(constants,constant == 'sigmaSB')[[2]];
  H <-sigmaSB * (T_C + Kelvin_temp)^4


  return(H)
}




