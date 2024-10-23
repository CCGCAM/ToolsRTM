#' Retrieve Satellite Collections
#'
#' This function retrieves satellite image collections from the Planetary Computer STAC API based on specified parameters, including bounding box, date range, and cloud cover threshold.
#'
#' @param scenario A character string representing the scenario, which is used to obtain the bounding box.
#' @param collection A character string specifying the name of the satellite data collection (e.g., "sentinel-2-l2a").
#' @param date_range A character vector of length 2 indicating the start and end dates in "YYYY-MM-DD" format.
#' @param cloud_threshold A numeric value representing the maximum allowable cloud cover percentage.
#' @param buffer_size A numeric value indicating the buffer size around the bounding box in meters (default is 300).
#'
#' @return A filtered image collection based on the specified criteria.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage of the get.satellite_collection function
#' scenario <- "your_scenario_here"  # Replace with your actual scenario
#' collection <- "sentinel-2-l2a"  # Sentinel-2 Level-2A collection
#' date_range <- c("2023-01-01", "2023-01-31")  # Date range for January 2023
#' cloud_threshold <- 20  # Maximum cloud cover of 20%
#'
#' # Retrieve the satellite collection
#' satellite_data <- get.satellite_collection(scenario, collection, date_range, cloud_threshold)
#'
#' # Print the retrieved collection details
#' print(satellite_data)
#' }
#'
get.satellite_collection <- function(scenario, collection, date_range, cloud_threshold, buffer_size = NULL) {
  # Get bounding box based on the scenario centroid with a buffer of buffer_size meters

  gdalcubes::gdalcubes_options(parallel = 1)
  # Ensure the scenario is in sf format
  if (!inherits(scenario, "sf")) {
    scenario <- sf::st_as_sf(scenario)
  }
  bbox <- get_bounding_box(scenario, buffer_size)
  planetary_computer <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  # Search for items using the planetary computer STAC API
  items <- planetary_computer |>
    stac_search(collections = collection,
                bbox = bbox[1:4],  # Use the reduced bounding box
                datetime = paste0(date_range[[1]], "/", date_range[[2]]),
                limit = 500) |>
    post_request()

  # Sign the STAC items for authentication
  items <- items_sign(
    items,
    sign_planetary_computer()
  )

  if (length(items$features) == 0) {
    stop("No images found in the specified collection for the given bounding box and date range.")
  } else {
    cat("Images found:", length(items$features), "\n")
  }

  # Get the assets for the first item
  first_item_assets <- items$features[[1]]$assets
  asset_names <- names(first_item_assets)
  print("Asset names:")  # Debug: Check the available asset names
  print(asset_names)


  # Define asset names based on the collection
  assets <- switch(collection,
                   "landsat-c2-l2" = asset_names[c(3:4,12:16)], # for landsat level-2
                   "sentinel-2-l2a" = asset_names[c(3:9,13,11:12,14)], # Example for Sentinel-2 Level-2A
                   "modis-17A2HGF-061" = c("Gpp_500m",'PsnNet_500m'), # Example for MODIS Gross Primary Productivity
                   "modis-11A2-061" = c("LST_Day_1km"), # or MODIS Land Surface Temperature
                   "modis-09A1-061" = asset_names[c(3:9)], #for MODIS Surface Reflectance 8-Day (500m)
                   "modis-09Q1-061" = c( "sur_refl_b01","sur_refl_b02","sur_refl_qc_250m","sur_refl_state_250m"), # Example for MODIS Surface Reflectance 8-Day (250m)
                   "modis-15A2H-061" = c("Lai_500m","Fpar_500m","LaiStdDev_500m",'FparStdDev_500m','FparLai_QC'), # Example for MODIS Leaf Area Index/FPAR 8-Day
                   "modis-15A3H-061" = c("Lai_500m","Fpar_500m","LaiStdDev_500m",'FparStdDev_500m','FparLai_QC')  # Example for MODIS Leaf Area Index/FPAR 4-Day
  )

  # Determine if cloud filtering is applicable
  if (collection == "modis-09A1-061" || collection == "modis-17A2HGF-061" || collection == "modis-11A2-061"
      || collection == 'modis-09Q1-061'|| collection == 'modis-09A1-061'
      || collection == 'modis-15A2H-061'|| collection == 'modis-15A3H-061') {
    # Create image collection without cloud cover filtering
    clt <- items$features |>
      stac_image_collection(asset_names = assets)
  } else {
    # Create image collection with cloud cover filtering for other collections
    clt <- items$features |>
      stac_image_collection(asset_names = assets,
                            property_filter = function(x) {
                              x[["eo:cloud_cover"]] < cloud_threshold
                            })
  }
  # Print satellite collection for debugging
  print(clt)
  return(clt)  # Return the filtered image collection
}
