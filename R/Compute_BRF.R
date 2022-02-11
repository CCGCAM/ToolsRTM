
#' Computes bidirectional reflectance factor based on outputs from PRO-4SAIL and sun position
#'
#' Authors of the version:Jean-Baptiste FERET (jb.feret@teledetection.fr)
#' Florian de BOISSIEU (fdeboiss@gmail.com)
#' Copyright 2019/11 Jean-Baptiste FERET
#' 
#' The direct and diffuse light are taken into account as proposed by:
#' Francois et al. (2002) Conversion of 400-1100 nm vegetation albedo
#' measurements into total shortwave broadband albedo using a canopy
#' radiative transfer model, Agronomie
#' 
#' Es = direct
#' Ed = diffuse
#'
#' @param rdot numeric. Hemispherical-directional reflectance factor in viewing direction
#' @param rsot numeric. Bi-directional reflectance factor
#' @param tts numeric. Solar zenith angle
#' @param data.light list. direct and diffuse radiation for clear conditions, is NULL use default values
#' @return BRF numeric. Bidirectional reflectance factor
#' @export
#' 
Compute_BRF  <- function(rdot=NULL,rsot=NULL,tts=NULL,data.light=NULL){
  
  ############################## #
  ##	direct / diffuse light	##
  ############################## #
  if (is.null(data.light)){
    Es <- ToolsRTM::dataSpec_PDB[,11]
    Ed <- ToolsRTM::dataSpec_PDB[,12]
    rd <- pi / 180
    
  } else{
    Es <- data.light$direct_light ##
    Ed <- data.light$diffuse_light ##
    rd <- pi / 180
   
  }

  #
  # if (skyl == 0.1){
  #   #  by default skyl = 0.1
  #   skyl = 0.847 - 1.61 * sin_90tts + 1.04 * sin_90tts ** 2 #this equation return sky=0.1 
  # } else {
  #   skyl=inputLUT[,'skyl']
  # }
  
  skyl <- 0.847- 1.61 * sin((90 - tts) * rd)+ 1.04 * sin((90 - tts) * rd)*sin((90 - tts) * rd) # diffuse radiation (Francois et al., 2002)
  PARdiro <- (1 - skyl) * Es
  PARdifo <- skyl * Ed
  BRF <- (rdot * PARdifo + rsot * PARdiro)/(PARdiro + PARdifo)
  
  return(BRF)
}