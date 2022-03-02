#
rm(list= ls())
raster_temp_dir<-paste(tempdir(),'raster/',sep='')
files_to_be_removed<-list.files (raster_temp_dir,full.names = T)
file.remove(files_to_be_removed)

##############################################################################################################################
#	0. Libraries   -----    
##############################################################################################################################

if (!require("raster")) { install.packages("raster"); require("raster") }  ### hsdar for PROSAIL
if (!require("hsdar")) { install.packages("hsdar"); require("hsdar") }  ### hsdar for PROSAIL
if (!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }  ### colors
if (!require("pls")) { install.packages("pls"); require("pls") }  ### PLSR
if (!require("signal")) { install.packages("signal"); require("signal") }  ### interpolations
if (!require("prospectr")) { install.packages("prospectr"); require("prospectr") }  ## for resample2
if (!require("MASS")) { install.packages("MASS"); require("MASS") }  ## for smoth t the rfl from leaves
if (!require("caret")) { install.packages("caret"); require("caret") }  ##  models (random forest)
if (!require("ncdf4")) { install.packages("ncdf4"); require("ncdf4") }  ##  models (random forest)
if (!require("rgdal")) { install.packages("rgdal"); require("rgdal") }  ##  models (random forest)
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }  ##  models (random forest)
if (!require("sf")) { install.packages("sf"); require("sf") }  ### 
if (!require("rgeos")) { install.packages("rgeos"); require("rgeos") }  ### 
if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") }  ### 
if (!require("svMisc")) { install.packages("svMisc"); require("svMisc") }  ### 

###
#######
Bands<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B10','B11','B12')
regions<-c('Coast aerosol','Blue','Green','Red', 'REd-edge1', 'REd-edge2', 'REd-edge3', 'NIR', 'REd-edge4', 'Water-Vapour', 'SWIR-Cirrus', 'SWIR1','SWIR2')
wave_2A<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1373.5,1613.7,2202.4)
db.SE<-cbind(Bands,regions,wave_2A)
head(db.SE)

##############################################################################################################################
#	1. generate stack and TIFF NCDF     -----    
##############################################################################################################################

## Bans for each NetCDF
SE_20m<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12') ##all bands
SE_10m<-c('B2','B3','B4','B8')
SE_60m<-c('B1','B9')

names_Areas<-c('Area1','Area2','Area3','Area4','Area5')

for (i in (1:5)){
  print(i)
  file_netCDF<-paste('examples/SEdata/',names_Areas[i],sep='')
  output.tiffs<-paste('examples/SEdata/TIFFs/',names_Areas[i],sep='')
  getTIFFs(netCDFs = file_netCDF,bands = SE_20m, output=output.tiffs)
  
  daily.files<-paste(output.tiffs,'/daily/',sep='')
  getStacks(rasterFiles = daily.files,bands = SE_20m, output=output.tiffs)
}

##############################################################################################################################
#	2. Extract series from Stack files      -----    
##############################################################################################################################

### Shape with polygons
shape<-readOGR(paste('examples/field-dataset/Shapefiles/polygons/Area_by_regionsETRS89.shp',sep = ''))

areas<-c('1','2','3','4','5')
factor = 1/10000

# output folder
paths.outs='examples/SEdata/Series/'
ifelse(!dir.exists(paths.outs), dir.create(paths.outs), FALSE)

for (j in c(1:4)){
  print(j)
  shape.sp<-subset(shape, Area == areas[j])
  #inputs
  paths.se2a=paste('examples/SEdata/TIFFs/Area',areas[j],'/Stacks/',sep='')
  data.serie<-getSeries(pathRaster=paths.se2a, shapefile=shape.sp, band_names=SE_20m,factorR=factor)
  file.to.export<-paste(paths.outs,'TimeSerie_SE2a_Area-',areas[j],'_polygons.csv',sep='')
  write.table(data.serie, file = file.to.export, sep=",", row.names = FALSE, col.names = T,append = F)
  }

### Shape with leaf dataset
shape<-readOGR(paste('examples/field-dataset/Shapefiles/points/Interaction_tree_ETRS89.shp',sep = ''))
areas<-c(1:3)

for (j in c(1:3)){
  print(j)
  shape.sp<-subset(shape, Area == areas[j])
  #inputs
  paths.se2a=paste('examples/SEdata/TIFFs/Area',areas[j],'/Stacks/',sep='')
  data.serie<-getSeries(pathRaster=paths.se2a, shapefile=shape.sp, band_names=SE_20m,factorR=factor)
  file.to.export<-paste(paths.outs,'TimeSerie_SE2a_Area-',areas[j],'_leafdata.csv',sep='')
  write.table(data.serie, file = file.to.export, sep=",", row.names = FALSE, col.names = T,append = F)
}


### Shape with localization of ICOS data
shape<-readOGR(paste('examples/field-dataset/Shapefiles/points/Field_cloudETRS89.shp',sep = ''))
areas<-c(1:5)

for (j in c(1:5)){
  print(j)
  shape.sp<-subset(shape, Area == areas[j])
  #inputs
  paths.se2a=paste('examples/SEdata/TIFFs/Area',areas[j],'/Stacks/',sep='')
  data.serie<-getSeries(pathRaster=paths.se2a, shapefile=shape.sp, band_names=SE_20m,factorR=factor)
  file.to.export<-paste(paths.outs,'TimeSerie_SE2a_Area-',areas[j],'_icos_data.csv',sep='')
 write.table(data.serie, file = file.to.export, sep=",", row.names = FALSE, col.names = T,append = F)
}


##############################################################################################################################
#	3. Get Forest Mask for study region  and Get Spatial trait mapping    -----    
##############################################################################################################################

## for Czech Republic  (1:4), San Rossore area 5 
areas<-c(1:5)
paths.with.shape<-'examples/field-dataset/Shapefiles/Areas_byETRS89/'
file_shape.Area<- list.files(paths.with.shape,pattern="*.shp$", full.names=TRUE)
factorSE = 1/10000

### optianal get a mosaic from tiles
ForestLayers='examples/SEdata/ForestMaps/FTY_2018_010m_cz_03035_v010/DATA/'
for (j in areas[1:4]){
  svMisc::progress(j, progress.bar = T)
  paths.with.shape =file_shape.Area[j]
  GetMosaics(ForestLayers = 'examples/SEdata/ForestMaps/FTY_2018_010m_cz_03035_v010/DATA/',
             shapeLayer =paths.with.shape,output = 'examples/SEdata/ForestMaps/outs/', proj=3035)
  
}

##############################################################################################################################
#	4. Get Forest Mask for study region  and Get Spatial trait mapping    -----    
##############################################################################################################################

## Get Spatial trait mapping 
## severla images
areas<-c(1:5)
factorSE = 1/10000
paths.with.shape<-'examples/field-dataset/Shapefiles/Areas_byETRS89/'
file_shape.Area<- list.files(paths.with.shape,pattern="*.shp$", full.names=TRUE)
model.nne<-readRDS("examples/outputs/models/Cab_nnet_fourSAIL2_5k.RData")


for (j in areas){
  print (j)
  paths.with.shape =file_shape.Area[j]
  
  paths.se2a=paste('examples/SEdata/TIFFs/Area',areas[j],'/Stacks',sep='')

  files.sensor=list.files(paths.se2a,pattern=".tif$", full.names=TRUE)
  path.export=paste('examples/SEdata/TIFFs/Area',areas[j],'/SpatialTraits/',sep='')
  ifelse(!dir.exists(path.export), dir.create(path.export), FALSE)
  if (j == 5){
    path.with.forest.mask='examples/SEdata/ForestMaps/FTY_2018_010m_it_03035_v010/DATA/FTY_2018_010m_E43N22_03035_v010.tif'
    
  } else if (j == 1) {
    path.with.forest.mask='examples/SEdata/ForestMaps/outs/Forest_mask_crop.tif'
  }
  
    #### generate the spatial maps for study areas
    for (i in c(1:length(files.sensor))){
      
      svMisc::progress(i, progress.bar = T)
      spatial.maps<-getSpatialTrait(rasterFiles = files.sensor[i], 
                                    ForestLayer = path.with.forest.mask,
                                    Sensor='Sentinel2a',saveFile = path.export,
                                    proj = 3035,
                                    shapeLayer = paths.with.shape,
                                    model.ML = model.nne$Cab$model,
                                    trait = 'Cab',factorR=factorSE)
      raster_temp_dir<-paste(tempdir(),'raster/',sep='')
      files_to_be_removed<-list.files (raster_temp_dir,full.names = T)
      file.remove(files_to_be_removed)
    }
} # end j









