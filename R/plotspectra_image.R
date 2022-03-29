#' Plot the spectra in points over the image
#'
#' @param Raster a brick or a Stack
#' @param n_spectra  number of spectra for selecting in the image
#' @param sensor the sensor, for the moment, this function works for Sentinel-2 images
#' @param factor the factor for reflectance
#' @param method or ggplot or classical method, by default is taken  'ggplot'
#' @return a dataframe with the selected point  and also show a plot with the seoctral signal of each poin
#' @export
#'
#' @examples
#' 

plotspectra_image<-function(Raster = NULL,n_spectra=1, sensor='Sentinel2a', factor=NULL, method='ggplot'){
  #Raster = r.brick.crop
  #sink("NUL")
  if (is.null(factor)){
    message('factor is empty, please add a factor for scaling plot')
    stop()
  }

  
  if (is.null(Raster) | class(raster)[1] == 'raster'){
    message('raster is empty, please add a raster stack')
    stop()
  }
  
  if (class(raster)[1] == 'RasterStack' | class(raster)[1] == 'RasterBrick'){
    message('please select some points')
  }

  if (is.null(n_spectra)){
    n_spectra=5
  }
  
  print(raster::plotRGB(Raster, scale=300, stretch = "lin"))
  points<-raster::click(Raster,n=n_spectra,id=T, xy=T, cell=T, type="p", pch=16, col="magenta", col.lab="red")
  points_df <- points[,c(4:dim(points)[2])] * factor
  # convert raster cell number into row and column (used to extract spectral signature below)
  row <- points$cell%/%nrow(Raster)+1 # add 1 because R is 1-indexed
  col <- points$cell%%ncol(Raster)
  points<-cbind(ID=c(1:n_spectra),Row=row,Col=col,points)
  
  ## melt by bands
  if (is.null(method) | method == 'ggplot' | method != 'classical' ){
  
    data_points <-cbind(points[1:6], stack(points[7:dim(points)[2]]))
    # Now, let's plot some spectral signatures!
    spectral_plot<-ggplot(data_points, aes(x=ind, y=values  * factor, group=ID)) +theme_bw() +
      geom_line(aes(color=as.factor(ID)),linetype = "dashed", size=0.8) +
      labs(color = "IDs", x='',y='Reflectance')+  ggtitle("spectral signatures")+
      geom_point(aes(color=as.factor(ID))) +
      theme(plot.title = element_text(hjust = 0.5, size=18))
    print(spectral_plot)
    to_export = list('Data'=points,'Plot'=spectral_plot)
    return(to_export)
    
  } else if (method == 'classical') {
    # add the 'wavelength' and rotate the df
    # (i didn't find the actual wavelength values, but hey).
    points_df <- cbind(1:ncol(points_df), t(points_df)) 
    ToolsRTM::plotspectra_SE(points_df, y=2:ncol(points_df), cols = c(rainbow(ncol(points_df)-2),'black'),
                             type='l', main=sensor,ylab="Reflectance", xlab = '')
    legend("topleft", legend = points$ID,
           fill=c(rainbow(ncol(points_df)-2),'black'),cex=0.8)
    return(points)
    
  }


#sink()
  
  
  
}

#' for plotting spectra adapted to Sentinel bands
#'
#' @param df 
#' @param x 
#' @param y 
#' @param cols 
#' @param xlim 
#' @param ylim 
#' @param main 
#' @param xlab 
#' @param ylab 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
plotspectra_SE <- function(df, x=1, y=2, cols=y,
                        xlim=range(df[,x], na.rm = T),
                        ylim=range(df[,y], na.rm = T),
                        main="", xlab="", ylab="", ...){
  
  
  at_plot = range(df[,x], na.rm = T)
  # setup plot frame
  par(mfrow=c(1,1),  mar = c(5,5,1.1,1),bg= "white", 
      font.main=1.0, cex.main=1.0,font.axis=2, cex.axis=1, las=1,
      font.lab=2, cex.lab=1)
  plot(NULL, xaxt = "n",
       xlim=xlim, 
       ylim=ylim,
       main=main, xlab=xlab, ylab=ylab)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
  axis(1, at=c(at_plot[1]:at_plot[2]), labels=names(df[,x]))

  # plot all your y's against your x
  pb <- sapply(seq_along(y), function(i){
    points(df[,c(x, y[i])], col=cols[i], ...)
  })
  grid(lwd = 2) 
}


