#' Get spectral bandset for interpolation
#'
#' @param sensor a character with name of the sensor; options avalaible are:"ALI", "Hyperion", "Landsat4" ,"Landsat5","Landsat7","Landsat8"
#'  "MODIS", "Quickbird", "RapidEye", "Sentinel2a", "Sentinel2b", "WorldView2-4", "WorldView2-8"
#'
#' @return
#' @export
#'
#' @examples
#' df.sentinel2a <- get.spectral(sensor='Sentinel2a')
#'
get.spectral <- function(sensor='Sentinel2a') {

  data<-ToolsRTM::sensor.characteristics
  sensors<-unique(data$Sensor)
  if (sensor %in% sensor) {
    df.sensor <- subset(data, Sensor == sensor)
    return(df.sensor)
  } else {
    stop(paste('Please use the avalaible sensors : ',sensors,sep = ''))
  }

}
