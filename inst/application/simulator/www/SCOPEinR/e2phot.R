#' get.e2phot calculates the number of moles of photons
#'
#' \code{get.e2phot} calculates the number of moles of photons
#' corresponding to E Joules of energy of wavelength lambda (m)
#'
#' @param lambda
#' @param E
#' @param constants
#'
#' @return
#' @export
#'
#' @author 	 Wout Verhoef, Christiaan van der Tol, Joris Timmermans (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
get.e2phot <- function(lambda, E, constants) {
  e <- get.ephoton(lambda, constants)
  photons <- E / e
  A <- subset(constants , constant == 'A')['value']

  molphotons <- photons / A$value
  return(molphotons)
}

