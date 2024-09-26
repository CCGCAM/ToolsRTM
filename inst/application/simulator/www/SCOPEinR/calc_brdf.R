
#' @title get.brdf
#' \code{get.brdf} Calculate the brdf effect
#'

#' @param data.spectral spectral information for the model
#' @param data.angles observe  angles from LUT
#' @param data.rad radiometric parameters
#' @param directional directional angles from File or brdf
#' @param atmo atmospheric  fiel
#' @param data.soil soil properties
#' @param data.leafopt leaf optical properties
#' @param data.canopy canopy properties
#' @param data.leafbio leaf properties (Cab, Cw ...)
#' @param data.gap
#' @param data.meteo  meteo characterisitics
#' @param data.thermal thermal constants
#' @param data.bcu
#' @param data.bch
#' @param data.opts options for estimating calc_planck, fluoresecence and xanthophyllabs
#'
#' @return
#' @export
#'
#' @examples
#'
get.brdf <- function(data.spectral,data.angles,data.rad,
                     data.directional,
                     atmo,
                     data.soil,data.leafopt,data.leafbio,data.canopy,data.gap,
                     data.meteo,data.thermal,
                     data.bcu,data.bch,data.opts,
                     get.plots=F) {




  # simulates observations from a large number of viewing angles
  # modified: 30 April 2020, CvdT, removed repeated angle combinations.

  if (missing(data.opts)){
    stop('please use options for Lite, Calc_vert_profiles ')

  } else{

    options.calc_planck         = data.opts[3,]   # calculate spectrum of thermal radiation
    options.calc_fluor          = data.opts[2,]   # calculate chlorophyll fluorescence in observation direction
    options.calc_xanthophyllabs = data.opts[4,]   # include simulation of reflectance dependence on de-epoxydation state

  }



  if (missing(get.plots)){
    get.plots = FALSE
  }
  ## input
  tts <- data.angles$tts
  psi_hot <- c(0, 0, 0, 0, 0, 2, 358) # [noa_o] angles for hotspot oversampling
  tto_hot <- c(tts, tts + 2, tts + 4, tts - 2, tts - 4, tts, tts) # [noa_o] angles for hotspot oversampling

  psi_plane <- c(rep(0, 6), rep(180, 6), rep(90, 6), rep(270, 6)) # angles for plane oversampling
  tto_plane <- c(10:60, 10:60, 10:60, 10:60) # angles for plane oversampling

  psi <- c(data.directional[['psi']], psi_hot, psi_plane)
  tto <- c(data.directional[['tto']] , tto_hot, tto_plane)
  directional <- list()
  ## remove duplicates
  unique_angles <- unique(cbind(psi, tto))
  directional[['psi']] <- unique_angles[,1]
  directional[['tto']] <- unique_angles[,2]
  na <- length(unique_angles[,1])

  ## allocate memory
  directional[['brdf_']] <- matrix(0, nrow=length(data.spectral$wlS), ncol=na) # [nwlS, no of angles]
  directional[['Eoutte']] <- matrix(0, nrow=1, ncol=na) # [1, no of angles]
  directional[['BrightnessT']] <- matrix(0, nrow=1, ncol=na) # [1, no of angles]

  directional[['LoF_']] <- matrix(0, nrow=length(data.spectral$wlF), ncol=na) # [nwlF, no of angles]
  directional[['Lot_']] <- matrix(0, nrow=length(data.spectral$wlT), ncol=na) # [nwlF, no of angles]

  directional[['refl_']] <- matrix(0, nrow=length(data.spectral$wlS), ncol=na) # [nwlF, no of angles]
  directional[['rso_']] <- matrix(0, nrow=length(data.spectral$wlS), ncol=na)
  directional[['Lo_']] <- matrix(0, nrow=length(data.spectral$wlS), ncol=na)
  ## other preparations
  directional_angles <- data.angles

  ## loop over the angles

  progress_bar = txtProgressBar(min=0, max=na, style = 3, char="=")

  for (j in 1:na) {

    setTxtProgressBar(progress_bar, j)
    # optical BRDF
    directional_angles[['tto']] <- directional[['tto']][j]
    directional_angles[['psi']] <- directional[['psi']][j]

    directional.getRTM0<-getRTMo(data.spectral,atmo,data.soil,data.leafopt,data.canopy,data.leafbio,
                       data.angles=directional_angles,data.meteo,data.opts=data.opts,get.plots=F)

    directional.rad <-directional.getRTM0$data.rad


    directional[['refl_']][,j] <- directional.rad[['refl']] # [nwl] reflectance (spectral) (nm-1)
    directional[['rso_']][,j] <- directional.rad[['rso']] # [nwl] BRDF (spectral) (nm-1)

    # thermal directional brightness temperatures (Planck)
    if (options.calc_planck$Value == 1) {


      directional.RTMt.planck <- get.RTMt.planck(data.spectral=data.spectral,data.rad=data.rad,data.soil=data.soil,
                                                 data.leafbio=data.leafbio, data.leafopt=data.leafopt,
                                                 data.canopy=data.canopy,
                                                 data.gap=data.gap,Tcu=data.thermal[['Tcu']],Tch=data.thermal[['Tch']],
                                  Tsu=data.thermal[['Tsu']],Tsh=data.thermal[['Tsh']],
                                  get.plots=F)

      directional[['Lot_']][,j] <- directional.RTMt.planck$Lot_[data.spectral$IwlT] + directional.RTMt.planck$Lo_[data.spectral$IwlT] # [nwlt] emitted plus reflected diffuse radiance at

    }

    if (options.calc_fluor$Value == 1) {

      directional.RTMf <- get.RTMf(data.spectral=data.spectral,data.rad=data.rad,
                                   data.soil=data.soil,data.leafopt=data.leafopt,
                                   data.canopy=data.canopy,data.gap=data.gap,data.angles=data.angles,
                           #relative fluorescence emission efficiency for sunlit leaves
                           data.etau=data.bcu[['eta']],
                           #relative fluorescence emission efficiency for shaded leaves
                           data.etah =data.bch[['eta']],get.plots=F)


      directional$LoF_[, j] <- directional.RTMf$LoF_
    }

    if (options.calc_xanthophyllabs$Value == 1) {

      directional.RTMz <- get.RTMz(data.spectral=data.spectral,data.rad=data.rad,
                                   data.soil=data.soil,data.leafopt=data.leafopt,
                                   data.canopy=data.canopy,data.gap=data.gap,data.angles=data.angles,
                           data.Knu = data.bcu[['Kn']],data.Knh = data.bch[['Kn']],
                           get.plots=F)



     directional$Lo_[,j] <- directional.RTMz$Lo_

    }
  } # end loop na for all angles
  close(progress_bar)
  return(directional)

} # end angles
