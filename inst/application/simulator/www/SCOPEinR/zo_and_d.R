#' @title get.zo_and_d model
#' \code{get.zo_and_d} Calculates roughness length for momentum and zero
#'  plane displacement from vegetation height and LAI
#' @param inputLUT
#' @param constants
#'
#' @return
#' @export
#' @author 	A. Verhoef (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' last updates:
#'   - 17 November 2008
#    - 17 April 2013 (structures)

#' @references Verhoef, McNaughton & Jacobs (1997), HESS 1, 81-91
#' @examples
#'
get.zo_and_d<-function(inputLUT,constants,calc.heat,calc.rss_rbs){

  zo_and_d = list()
  ## constants
  # constants used (as global)
  #   kappa       Von Karman's constant

  load("www/data/SCOPEinR/constants.rda")

  kappa   =  subset(constants,constant == 'kappa')[[2]]

  canopy<- getinputLUT(inputLUT, dataset='canopy')
  soil<- getinputLUT(inputLUT, dataset='soil',
                               calc.heat = calc.heat,
                               calc.rss_rbs =  calc.rss_rbs)


  # soil fields used:
  #   Cd          Averaged drag coefficient for the vegetation
  #   CR          Drag coefficient for isolated tree
  #   CSSOIL      Drag coefficient for soil
  #   CD1         Fitting parameter
  #   Psicor      Roughness layer correction
  ## parameters
  CR      = canopy[['CR']];
  CSSOIL  = canopy[['CSSOIL']];
  CD1     = canopy[['CD1']];
  Psicor  = canopy[['Psicor']];

  # canopy fields used as inpuyt:
  #   LAI         one sided leaf area index
  #   hc           vegetation height (m)
  LAI     = canopy[['LAI']];
  hc       = canopy[['hc']];

  ## calculations
  sq      = sqrt(CD1 * LAI / 2);
  G1      = max(3.3, (CSSOIL + CR * LAI / 2)^(-0.5));
  if((LAI > 1e-7) & (hc > 1e-7)){
    d  = hc * (1 - (1 - exp( -sq))/sq);
    zo_and_d$d<-d
  } else {
    zo_and_d$d = 0
    zo_and_d$d<-d
  }
  # output:
  #   zom  roughness lenght for momentum (m)
  #   d zero plane displacement (m)

  # Eq 12 in Verhoef et al (1997)
  zom = (hc - d) * exp(-kappa * G1 + Psicor);
  zo_and_d$zom <-zom
  return(zo_and_d)
}

