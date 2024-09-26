
#' @title get.aggregator.ebal
#' \code{get.aggregator.ebal} Aggregator function  for fluxes based on LAI parameters
#' @param LAI
#' @param sunlit_flux
#' @param shaded_flux
#' @param Fs
#' @param data.canopy
#' @param canopy.choice choice are for meanleaf are: 'angles', 'layers' and 'angles_and_layers'
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#'
#' @examples
#'
#'
get.aggregator.ebal <- function(LAI, sunlit_flux, shaded_flux, Fs, data.canopy, canopy.choice) {

  nl <- data.canopy[['nlayers']]
  nli <- data.canopy[['nlincl']]
  nlazi <- data.canopy[['nlazi']]
  lidf <- data.canopy[['lidf']]

  # if (is.vector(sunlit_flux)){
  #
  #   sunlit_flux_ <- array(0, dim = c(nli, nlazi,nl))
  #   sunlit_flux_[,,1:nl] <- sunlit_flux
  #
  # }
  #
  #
  # if (is.vector(shaded_flux)){
  #
  #   shaded_flux_ <- array(0, dim = c(nli, nlazi,nl))
  #   shaded_flux_[,,1:nl] <- shaded_flux
  # }
  #

  flux_tot <- LAI * (meanleaf.v2(data.canopy, sunlit_flux, canopy.choice, Fs) + meanleaf.v2(data.canopy, shaded_flux, canopy.choice = 'layers', 1 - Fs))
  return(flux_tot)
}


#' @title meanleaf.v2
#' \code{meanleaf.v2} is the meanleaf version adapted for vectors
#'
#' @param data.canopy
#' @param F_
#' @param canopy.choice
#' @param Ps
#'
#' @return
#' @export
#'
#' @examples
#'
meanleaf.v2 <- function(data.canopy, F_, canopy.choice, Ps) {
  nl <- data.canopy[['nlayers']]
  nli <- data.canopy[['nlincl']]
  nlazi <- data.canopy[['nlazi']]
  lidf <- data.canopy[['lidf']]

  Fout <- array(0, dim = c(nli, nlazi, nl))

  switch(canopy.choice,

         "angles" = {
           for (j in 1:nli) {
             Fout[j,,] <- F_[j,,] * lidf[j]
           }

           Fout_vertical <- sum(Fout) / nlazi

         },

         "layers" = {

           #this is only for a single vector (nlayers)

           #F_nlayer <- apply(F_, c(3), sum)
           #Fout_vertical <- Ps[1:nl] *  F_nlayer / nl
           #return a single value
           Fout_vertical = sum(Ps * (F_) ) / nl
         },

         "angles_and_layers" = {
           for (j in 1:nli) {
             Fout[j,,] <- F_[j,,] * lidf[j]
           }
           for (j in 1:nl) {
             Fout[,,j] <- Fout[,,j] * Ps[j]
           }
           Fout_vertical <- sum(sum(sum(Fout))) / (nlazi * nl)
         }
  )

  return(Fout_vertical)
}

