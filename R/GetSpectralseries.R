#' Extract all bands from SE images (adapted to NETCDF dataset)
#'
#' @param netCDFs  List of NetCDF
#' @param bands  a vector with the names of the inputs of the NetCDF
#' @param shapefile  shapefile (point)
#' @param factorSE  factor to apply in reflectance files (point)
#' @param Indices  if TRUE or null, the function estimate the Indices for each date.
#' @return
#' @export
#'
#' @examples
#' 

GetSpectralseries<-function(netCDFs=NULL, bands=NULL, shapefile=NULL, factorSE = 1/10000, Indices = T){

  
  if (is.null(bands)) {
    message('indicate the bands of the sensor')
    close()
  } 
  #bands<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')
  #shapefile=shape.sb
  #netCDFs =path_netCDFs

  is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
  #files_nc = list.files(netCDFs,pattern="*.nc$", full.names=TRUE)
  files_nc = netCDFs
  message(' TIFFs conversion is processing ...')
  
  progress_bar = txtProgressBar(min=0, max=length(files_nc), style = 3, char="=")
  table.to.save<-list()
  
  for (nc_file in c(1:length(files_nc))){
    setTxtProgressBar(progress_bar, nc_file)
    nc_data <- nc_open(paste(files_nc[nc_file],sep=''))
    
    lon <- ncvar_get(nc_data, "x")
    lat <- ncvar_get(nc_data, "y", verbose = F)
    t <- ncvar_get(nc_data, "time")
    tunits<-ncatt_get(nc_data,"time",attname="units")
    tustr<-strsplit(tunits$value, " ")
    dates<-as.Date(t,origin=unlist(tustr)[3])
    
    table.info<-list()
      for (i in c(bands)){
        B.array <- raster::brick(paste(files_nc[nc_file],sep=''), varname=i)
        table.info[[i]]<- t(raster::extract(B.array, shapefile, df = F, na.rm = T, cellnumbers = F) * factorSE)
      
      }

    table.in<-data.frame(dates,do.call(cbind, table.info))
    colnames(table.in)<-c('Date',bands)
    table.to.save[[nc_file]]<-table.in


  }  
  
  table.f<-data.frame(do.call(rbind, table.to.save))
  
  if (Indices == T | is.null(Indices)) {
    wavelengths.sentinel<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4)
    table.f<-ToolsRTM::getIndicesSE2a( table.f[,c(2:13)],wavelengths.sentinel,table.f,header = F)  
  }  
    
  close(progress_bar)
  return(table.f)
  }
