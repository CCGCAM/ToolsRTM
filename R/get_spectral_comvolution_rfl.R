#' Get Spectral Convolution for Reflectance
#'
#' This function performs spectral convolution on reflectance data using a specified sensor's
#' spectral response function. The convolution is essential for simulating how well the sensor
#' captures the true reflectance values based on its spectral characteristics.
#'
#' @param df A data frame containing the high-resolution reflectance data to be convoluted.
#' @param sensor.i A character string with sensor information for which the calculations are performed. 
#' Supported sensors should have predefined spectral response functions. Options include:
#' "LANDSAT4.TM", "LANDSAT5.TM", "LANDSAT7.ETM", "LANDSAT8.OLI", "Sentinel2A.MSI", 
#' "Sentinel2B.MSI", "Sentinel3A.OLCI", "Sentinel3B.OLCI", and "TerraAqua.MODIS".
#' @param get.plots A boolean value indicating whether to generate plots of the spectral convolution.
#'                  Default is TRUE.
#'
#' @return A data frame containing the convoluted reflectance values for the specified sensor.
#' @export
#' @examples
#' # Example data frame with reflectance data
#' df <- data.frame(Wavelength = seq(400, 2500, by = 10),
#'                  Reflectance = runif(211)) # Simulated reflectance values
#' 
#' # Perform spectral convolution for a specific sensor
#' convoluted_results <- get.spectral.convolution.rfl(df, sensor.i = "LANDSAT8.OLI", get.plots = TRUE)

get.spectral.convolution.rfl <- function(df, sensor.i, get.plots=T) {

  # Spectral convolution for a given spectral response function
  # input:
  # get.spectral.convolution:    irradiance or radiance in high resolution, to be convoluted
  # sensor.i:       sensor characterisrtics

  sensors.properties = get.coef.SMAC(sensor = sensor.i)
  wlSensor =  sensors.properties[['wl.smac']]
  coefs.SMAC  = sensors.properties[['coefs.SMAC']]
  Sensor.name = sensors.properties[['Sensor.name']] ## this is mission name

  bands_df <- data.frame(sensors.properties$wl.srf.smac)
  weights_df <- data.frame(sensors.properties$p.srf.smac)

  if ( Sensor.name == 'Sentinel3B' || Sensor.name =='Sentinel3A' || Sensor.name == 'TerraAqua'){
    bands_df <- colMeans(bands_df,na.rm=T)
    bands_df <- round(bands_df, digits = 0)
    weights_df <- colMeans(weights_df,na.rm=T)
  }

  # Function to select wavelengths for each band including weights
  selectWavelengths <- function(band, weights) {
    merged_df <- merge(data.frame(wave = band, weight = weights), df, by = "wave", all.x = TRUE)
    return(merged_df)
  }

  # Function to calculate convolution for each band
  calculateConvolution <- function(selected_band) {
    if (sum(selected_band$weight) == 0) {
      conv_result <- NA
    } else {
      conv_result <- sum(selected_band$weight * selected_band$rfl, na.rm = TRUE) / sum(selected_band$weight)
    }
    return(conv_result)
  }

  # Apply the function to each band with respective weights
  selected_wavelengths <- Map(selectWavelengths, bands_df, weights_df)
  # Apply the convolution function to each selected band
  convolution_results <- lapply(selected_wavelengths, calculateConvolution)

  df.conv = data.frame(wave=wlSensor,  RFL=as.numeric(convolution_results))


  if (get.plots ==  T){

    plot.conv <- ggplot2::ggplot(data = df.conv, aes(x = wave, y = RFL)) +
      labs(y= " Extraterrestrial irradiance", x = "") +
      geom_line() + theme_bw()

    print(plot.conv)

  }
  return(df.conv)
}





