#' get series in a dataframe from rasters
#'
#' @param pathRaster folder with the raster in tif format
#' @param shapefile shapefile file (.shp)
#' @param band_names names of the bands of the raster
#' @param factorR factor for the reflectance bands
#'
#' @return
#' @export
#'
#' @examples
#' 
getSeries<-function(pathRaster=NULL, shapefile=NULL, band_names=NULL,factorR=NULL){
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
        
       
     
        if (is.null(band_names) | length(se2.bands) == 12) {
          #message('B1-8, B8A-B9 and B11-B12 were used')
          wavelengths.sentinel<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4)
          data.write.sb<-data.write[,se2.bands]
          colnames(data.write.sb)<-paste('R.',c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4),sep='')
          
        } else if(length(se2.bands) == 13) {
          wavelengths.sentinel<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1313.15,1613.7,2202.4)
          data.write.sb<-data.write[,se2.bands]
          colnames(data.write.sb)<-paste('R.',c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1313.15,1613.7,2202.4),sep='')
          
        } else if(length(se2.bands) == 10) { ## For bands without B1 and B9 but with SCL
          wavelengths.sentinel<-c(492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,1613.7,2202.4)
          data.write.sb<-data.write[,se2.bands]
          colnames(data.write.sb)<-paste('R.',c(492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,1613.7,2202.4),sep='')
          
        }
        
        else{
          message('please check the band form Sentinel-2A')
          stop('number of bands are incorrect')
        }
      
        list.indices[[k]]<-ToolsRTM::getIndicesSE2a(data.write.sb,wavelengths.sentinel, data.write,header = F)  
       
      }
      ##################################################################################
  }
close(progress_bar)
data.export<-do.call(rbind.data.frame, list.indices)
return(data.export)

}
 