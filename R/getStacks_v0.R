

#' Title
#'
#' @param rasterFiles  path with the Netcdf
#' #' @param frequency Daily
#' @param bands  a vector with the names of the inputs of the NetCDF
#' @param output  path of the outputs
#'
#' @return stacks
#' @export
#'
#' @examples here adding examples ....
#' 
getStacks<-function(rasterFiles=NULL, frequency='Daily', bands=NULL,output=NULL){
  options(warn=-1) ###avoid warnings
  files = list.files(rasterFiles,pattern="day_*", full.names=F)
  dates<-as.Date(substr(files,5,14))
  dates.unique<-unique(dates)
  #### create the Stack of images in all files for "
  progress_bar = txtProgressBar(min=0, max=length(dates.unique), style = 3, char="=")
  for (k in c(1:length(dates.unique))){ 
    setTxtProgressBar(progress_bar, k)
    path_out=paste(rasterFiles,'/day-',dates.unique[k],'',sep='') 
    files = list.files(path_out,pattern="*.tif$", full.names=TRUE)
    #rs <- stack(files) 
    ## by order B1-B2-B3-B4-B5-B6-B7-B8-B8A-B9-B11-B12
    if ('SCL' %in% bands & 'B1' %in% bands){
      rs <- stack(files[1],files[4],files[5],files[6],files[7],files[8],files[9],files[10],files[11],files[12],files[2],files[3],files[13])
      names(rs) <- bands
    } else if (!('B1' %in% bands)){
      rs <- stack(files[3],files[4],files[5],files[6],files[7],files[8],files[9],files[10],files[1],files[2],files[11])
      names(rs) <- bands
    } else if (!('B9' %in% bands)){
      rs <- stack(files[3],files[4],files[5],files[6],files[7],files[8],files[9],files[10],files[1],files[2],files[11])
      names(rs) <- bands
    }
    else {
      rs <- stack(files[1],files[4],files[5],files[6],files[7],files[8],files[9],files[10],files[11],files[12],files[2],files[3])
      names(rs) <- bands[1:12]
    }
   
    #plot(rs)
    path_out<-paste(output,'/Stacks',sep="")
    ifelse(!dir.exists(path_out), dir.create(path_out), FALSE)
    raster.file<-paste(path_out,'/SE2A-',dates[k],'',sep="")
    writeRaster(rs, raster.file, "GTiff", overwrite=TRUE,bylayer=F)
  
    
}
  close(progress_bar)
  m_final<-message('Stacks files were generated sucessfully')
  return(m_final)


}
