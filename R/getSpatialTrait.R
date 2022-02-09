#' Spaital mapping of the main triats
#'
#' @param rasterFiles  path with the images (in RasterBrick/RasterStack formats) or a single stackRaster
#' @param ForestMask  shape
#' @param factorR 
#'
#' @return
#' @export
#'
#' @examples
getSpatialTrait<-function(rasterFiles=NULL,ForestLayers=NULL,
                          saveFile=NULL,proj=3035,shape.Path=NULL,
                          model.ML =NULL, trait='Cab',
                          factorR=NULL, GetMosaics=T ){
  options(warn=-1) ###avoid warnings
  
  ### to remove   ######################
  rasterFiles='examples/SEdata/TIFFs/Area1/Stacks/'
  model.ML<-readRDS('examples/outputs/models/Cab_nnet_fourSAIL2_5k.RData')
  model.ML<-model.ML$Cab$model
  saveFile='examples/SEdata/ForestMaps/outs/'
  shapePath='examples/field-dataset/Shapefiles/Areas_byETRS89/Area_1.shp'
  factorR=1/10000
  trait='Cab'
  proj=3035
  ######################
  
  shape<-sf::read_sf(shapePath)
  files.sensor = list.files(rasterFiles,pattern="*.tif$", full.names=TRUE)
  
  if (is.null(proj)) {
    message('missing projection')
    message('EPSG:3035 will be used')
    proj=3035
  } 
  #proj_db <- system.file("proj/proj.db", package = "sf")
  #if (proj_db == "") proj_db <- proj_db_path
  #crs_table <- sf::read_sf(proj_db, "crs_view") # extracts the "crs_view" table
  #subset(crs_table, grepl("Belg|Ostend", name) & auth_name == "EPSG")[2:5]
  
  crs_files <- sf::st_crs(proj)
  crs_to_raster<-crs_files$proj4string
  
  if (is.null(Sensor) | Sensor == 'Sentinel2a'){
    sensor.bands <- data.frame('B01'=442.7,'B02'=492.4, 'B03'=559.8, 'B04'=664.6, 'B05'=704.1, 'B06'=740.5,
                        'B07' = 782.8, 'B08' = 832.8, 'B8A' = 864.7, 'B09' = 945.1, 'B11' = 1613.7, 'B12' = 2202.4)
    
    sensor.rfl<-paste('R.',c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4),sep='')
  } else{
    message('Please provide information for bands')
    stop(' needs a sensor information')
  }
  
  if (GetMosaics == T ){
    files.forest = list.files(ForestLayers,pattern="*.tif$", full.names=TRUE)
    shape.area<-sf::read_sf(shapePath)
    e <- extent(shape.area)
    template <- raster(e)
    projection(template) <- crs_to_raster
    writeRaster(template, file=paste(saveFile,"Forest_mask_v2.tif",sep=''), format="GTiff", overwrite=T)
    gdalUtils::mosaic_rasters(gdalfile=files.forest,dst_dataset=paste(saveFile,"Forest_mask.tif",sep=''),of="GTiff")
    ## clean memory in raster folder
    file.remove(list.files(paste(tempdir(),'raster/',sep=''),full.names = T))
    forest_m<-raster::brick(paste(saveFile,"Forest_mask.tif",sep=''))
    
  } else{
    files.forest = list.files(ForestLayers,pattern="*.tif$", full.names=TRUE)
    forest_m<-brick(files.forest[1])
  }
  
  ##### Generate a mask for forest map in fishnet area
  forest.sb<-raster::crop(forest_m,shape)
  raster::values(forest.sb)[raster::values(forest.sb) < 1] = NA
  writeRaster(forest.sb, filename=paste(saveFile,"Forest_mask_crop.tif",sep=''), "GTiff", overwrite=TRUE,bylayer=F)
  
  ##get_clouds of points
  ncases= round((raster::ncell(forest.sb)-raster::freq(forest.sb, value = NA)) / 100,0)
  shp_points <- raster::sampleRandom(forest.sb, size = ncases, xy = TRUE, sp=TRUE, na.rm = TRUE)
  shp_points$ID<-c(1:dim(shp_points)[1])
  shp_sf<-sf::st_as_sf(shp_points,crs = st_crs(3035))
  sf::st_write(shp_sf, paste(saveFile,"Forest_points.shp",sep=''),delete_layer = TRUE) # overwrites
  
  plot(forest.sb,col='blue')
  plot(shp_points, add=T, col='black', pch=20)
  # initiate progress bar
  bar.progress <- progress::progress_bar$new(format = "Processing [:bar] :percent in :elapsedfull, estimated time remaining :eta",
                                             total = length(files.sensor), clear = F, width= 100)
  for (i in c(1:length(files.sensor))){
    bar.progress$tick()
    raster.to<- raster::brick(files.sensor[i]) * factorR
    names(raster.to)<-names(sensor.bands)
    ##get Reflectance for predict specific trait
    df.e <- raster::extract(raster.to, shp_points, df = T, na.rm = T, cellnumbers = T)
    df.e <- na.omit(df.e)
    colnames(df.e)<-c('ID', 'cell',sensor.rfl)
    df.e$pred<-predict(object = model.ML,df.e[,sensor.rfl])

    
   
    if (trait == 'Cab') {
      df.e$pred = ifelse(df.e$pred < 0.5, NA, df.e$pred)
      df.e$pred = ifelse(df.e$pred > 90, NA, df.e$pred)
      df.e<-na.omit(df.e)
      shape_togetIndices<-merge(shp_points[,'ID'],df.e, by.x='ID' ,by.y='ID', all.x=F)
    } else if (trait == 'LAI') {
      df.e$pred = ifelse(df.e$pred < 0.1, NA, df.e$pred)
      df.e$pred = ifelse(df.e$pred > 7, NA, df.e$pred)
      df.e<-na.omit(df.e)
      shape_togetIndices<-merge(shp_points[,'ID'],df.e, by.x='ID' ,by.y='ID', all.x=F)
    }
  
    
    
    filter.column<-list()
    names_i<-colnames(df.e)[c(3:dim(df.e)[2])]
    for (i in c(1:length(names_i))){
      filter.column[[i]]<-ToolsRTM::filter_outliers(df=df.e, input=names_i[i])
    }
    df<-cbind(df.e[,1:2],do.call(cbind, filter.column))
    colnames(df)<-names(df.e)
    plot(df$R.664.6,df$pred)
    rm(df.e)
    df$pred_grop <- cut(df$pred,
                        breaks = seq(0,100,0.2))
    
    df.mean = aggregate(df[,names_i],
                    by = list(df$pred_grop),
                    FUN=quantile, probs  = 0.5,  na.rm = TRUE)
    for (i in sensor.rfl){
    plot(df.mean$pred, df.mean[,i])
    print(i)
    print(round(cor(df.mean$pred, df.mean[,i],use='pairwise.complete.obs')^2,5))
    }
    
    ### applicar for all bands and indices
    #getIndices in table
    ## get lm models with the best indice,
  # get de spatial map

    
    
    
    
  }

  
  
}


#' remove ouptliers
#'
#' @param df  dataframe
#' @param input  string, name of the variable
#'
#' @return
#' @export
#'
#' @examples
filter_outliers<-function(df=NULL, input=NULL){
  qnt <- quantile(df[,input], probs=c(.25, .60), na.rm = T)
  outs <- quantile(df[,input], probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(df[,input], na.rm = T)
  df[,input][ df[,input] < (qnt[1] - H)] <- NA #outs[1]
  df[,input][ df[,input] > (qnt[2] + H)] <- NA #outs[2]
  vector<-df[,input]
  return(vector)
}

