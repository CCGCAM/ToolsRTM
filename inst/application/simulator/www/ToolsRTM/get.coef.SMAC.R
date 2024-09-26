# get the coeficient for SMAC model
#' @param sensor get the rda for each spensor Landsat, Sentinel-2, Sentinel-3 and MODIS (Aqua and Terra)
#'
#' @examples
get.coef.SMAC <- function(sensor){

  Sensor.name <- sensor[['mission']]
  coef <-sensor[['SMAC_coef']]
  center.wvl <- sensor[['center_wvl']]
  wl.smac <- sensor[['wl_smac']]
  wl.srf.smac <-sensor[['wl_srf_smac']]
  # Assuming df is your data frame
  wl.srf.smac[is.na(wl.srf.smac)] <- NA
  p.srf.smac <- sensor$p_srf_smac
  Coeffs <- coef
  
  return(list(coefs.SMAC = Coeffs,wl.smac =wl.smac, 
              Sensor.name = Sensor.name,
              center.wvl = center.wvl, 
              wl.smac = wl.smac,
              wl.srf.smac = wl.srf.smac,
              p.srf.smac = p.srf.smac))

   

}
