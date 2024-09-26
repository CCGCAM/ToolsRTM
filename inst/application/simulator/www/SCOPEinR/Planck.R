#' Plank function to
#'
#' \code{Planck} estimate blackbody radiation emitted by an object at a given temperature and wavelengt
#'
#' @param wl : a numeric vector representing the wavelength(s) of interest in nanometers (nm).
#' @param Tb #' a numeric vector representing the temperature(s) of the object in Kelvin (K).
#' @param em  a numeric vector representing the emissivity of the object at the given
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
get.Planck<-function(wl,Tb,em = NULL){

  c1 = 1.191066e-22
  c2 = 14388.33

  if (missing(em) == T){
    em = rep(1,length(Tb))
  }


  #em_wl <- outer(em, (wl * 1e-9)^(-5), FUN = "*")

  #second_<- outer(Tb, (wl * 1e-3), FUN = "*")

  #Lb = em_wl / (exp(c2 /second_ )-1);

  Lb <- em * c1 * (wl * 1e-9)^(-5) / (exp(c2 / (wl * 1e-3 * Tb)) - 1)






  return(Lb)

}
