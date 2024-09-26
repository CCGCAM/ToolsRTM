#' Calculate the soil thermal intertial
#'
#' @param cs
#' @param rhos
#' @param lambdas
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
Soil_Inertia0<-function(cs,rhos,lambdas){
  # soil thermal inertia
  GAM = sqrt(cs * rhos * lambdas);  # soil thermal intertia
  return(GAM)
}

