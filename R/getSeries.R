#' get series in a dataframe from rasters
#'
#' @param pathRaster folder with the raster in tif format
#' @param shapefile shapefile file (.shp)
#' @param band_names names of the bands of the raster
#' @param factorR factor for the reflectance bands
#' @param get.indices A boolean is True, get spectral indices pre-define in getIndicesSE2, is not (FALSE) provide only bands
#'
#' @return
#' @export
#'
#' @examples
#'
getSeries<-function(pathRaster=NULL, shapefile=NULL, band_names=NULL,factorR=NULL,get.indices = T){
  options(warn=-1) ###avoid warnings

  if (is.null(factorR)){
    message('please provide factor scale for reflectance bands')
    stop()
  } else{
    factor = factorR
  }

  files = list.files(pathRaster,pattern="*.tif$", full.names=TRUE)
  dates = list.files(pathRaster,pattern="*.tif$", full.names=F)
  dates=substr(dates,6,15)
  if  (length(files) == 0){
    message('please provide raster in TIFF format')
    stop('TIF format is needed')
  } else {
      message('processing rasters ...')

      ##################################################################################

      progress_bar = txtProgressBar(min=0, max=length(files), style = 3, char="=")
      list.indices<-list()
      for (k in c(1:length(files))){
        setTxtProgressBar(progress_bar, k)
        #rs <- stack(files)
        ## by order
        rs <- brick(files[k])
        names(rs) <- band_names

        if (class(shapefile)[1] == 'SpatialPointsDataFrame'){
          r.extract <- raster::extract(rs, shapefile, df = T, na.rm = T, cellnumbers = F)
          data.write<-data.frame(ID=r.extract[,'ID'],Date= dates[k],r.extract[,c(2:dim(r.extract)[2])]* factor)
          se2.bands<-names(data.write[,grep(colnames(data.write),pattern="B",fixed = TRUE)])
        } else {
          r.extract <- raster::extract(rs, shapefile, df = T, na.rm = T, cellnumbers = T)
          data.write<-data.frame(ID=r.extract[,'ID'],cell = r.extract[,'cell'],Date= dates[k],r.extract[,c(3:dim(r.extract)[2])]* factor)
          se2.bands<-names(data.write[,grep(colnames(data.write),pattern="B",fixed = TRUE)])
        }

        #r.extract[is.nan(r.extract)] <- NA

        if (is.null(get.indices) | get.indices == F){

          list.indices[[k]]<- data.write
        } else {
          message(cat(' '))
          message(paste('adding spectral indices for the SE-2 image: ',k,'/',length(files),sep = ''))
          list.indices[[k]]<-ToolsRTM::getIndicesSE2(df=data.write,sensor='Sentinel-2a', df.data=data.write)

        }

      }
      ##################################################################################
  }
close(progress_bar)
data.export<-do.call(rbind.data.frame, list.indices)
return(data.export)

}
