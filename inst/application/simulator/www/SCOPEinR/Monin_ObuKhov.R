
#' @title get.Monin.Obukhov function
#' \code{get.Monin.Obukhov}
#' @param data.meteo
#' @param H
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'

get.Monin.Obukhov<-function(data.meteo,H){


  rhoa =  subset( constants,constant == 'rhoa')[[2]]
  cp =  subset( constants,constant == 'cp')[[2]]
  kappa =  subset( constants,constant == 'kappa')[[2]]
  g =  subset( constants,constant == 'g')[[2]]

  L = -rhoa * cp * data.meteo[['ustar']]^3 * (data.meteo[['Ta']]  + 273.15) / (kappa * g * H)
  L[is.na(L)] <- -1e6
  return(L)

}
