
#' Computes bidirectional reflectance factor based on outputs from PROSAIL and sun position
#'
#' The direct and diffuse light are taken into account as proposed by:
#' Francois et al. (2002) Conversion of 400-1100 nm vegetation albedo
#' measurements into total shortwave broadband albedo using a canopy
#' radiative transfer model, Agronomie
#' Es = direct
#' Ed = diffuse
#'
#' @param rdot numeric. Hemispherical-directional reflectance factor in viewing direction
#' @param rsot numeric. Bi-directional reflectance factor
#' @param tts numeric. Solar zenith angle
#' @param SpecATM_Sensor list. direct and diffuse radiation for clear conditions
#' @return BRF numeric. Bidirectional reflectance factor
#' @export
#' 
Compute_BRF  <- function(rdot=NULL,rsot=NULL,tts=NULL,SpecATM_Sensor=NULL){
  
  ############################## #
  ##	direct / diffuse light	##
  ############################## #
  Es <- SpecATM_Sensor$direct_light ##ToolsRTM::dataSpec_PDB[,11]
  Ed <- SpecATM_Sensor$diffuse_light ##ToolsRTM::dataSpec_PDB[,12]
  rd <- pi/180
  skyl <- 0.847- 1.61*sin((90-tts)*rd)+ 1.04*sin((90-tts)*rd)*sin((90-tts)*rd) # diffuse radiation (Francois et al., 2002)
  PARdiro <- (1-skyl)*Es
  PARdifo <- skyl*Ed
  BRF <- (rdot*PARdifo+rsot*PARdiro)/(PARdiro+PARdifo)
  return(BRF)
}