
#' Get in a LUT all main outputs from SCOPE model
#'
#' @param pathin path with the outfile directory
#' @param nsamples number of simulations
#' @param resampling  for resampling simulation at Sentinel-2 resolution, option 'Sentinel2a' is null no resampling i used
#' @param reflectance Type of reflectance 'apparent' or  'reflectance', NULL get the apparent reflectance (adding Fluorescence)
#' @param SIF is TRUE estimate the Fluorescence emission based on FLD-2 method, Need radiance parameter
#' @param radiance  is TRUE get Radiance
#'
#' @return
#' @export
#'
#' @examples
#' 
getSCOPE_outputs<-function(pathin=NULL,nsamples=100, resampling='Sentinel2a',
                            reflectance ='apparent', SIF=T,
                            radiance =T){
  
  if (is.null(pathin)){
    stop('please give a path to find the SCOPE tables ...')
  } else{
    ## get files
    #pathin = 'examples/SCOPE/OHP_2022-02-25-1826/'
    list.outs<-list.files(pathin,full.names =T)
   
  }
  ## get wavelength
  wl_files<-list.files(pathin,pattern='wlS.txt',full.names =F)
  data.wlS<-read.table(paste(pathin,'/',wl_files,sep=""),header=F)
  colnames(data.wlS)<-c('wavelS')
  
  wave<-c(data.wlS$wavelS)
  inputs<-data.table::fread(paste(pathin,'pars_and_input_short.csv',sep=''),header=F,skip=2,nrows=nsamples,sep=',')
  if (length(colnames(inputs)) == 18){
    colnames(inputs)<-c('n_pars','Cab','Cca','Cdm','Cw','N','Cant','Vcmax25','BallBerrySlope','LAI','LIDFa','Rin','Ta','Rli','u','Ca','tts','tto')
    
  } else if (length(colnames(inputs)) == 20) {
    colnames(inputs)<-c('n_pars','Cab','Cca','Cdm','Cw','N','Cant','Vcmax25','BallBerrySlope','LAI','LIDFa','Rin','Ta','Rli','p','ea','u','Ca','tts','tto')
    
  }
 
  
   # ######################################################################################
   # #reflectance  #fraction of radiation in observation direction *pi / irradiance 
   # ######################################################################################
   if (reflectance == 'apparent'| is.null(reflectance)){
     rfl.app <- as.matrix(data.table::fread(paste(pathin,'apparent_reflectance.csv',sep=''), skip=2,nrows = nsamples))
     SpecRefl <- hsdar::speclib(rfl.app[,c(1:2001)], wave[c(1:2001)])
     #plot(SpecRefl,ylab='Reflectance adding radiance with F')
   } else  if (reflectance == 'reflectance'){
     rfl <- as.matrix(data.table::fread(paste(pathin,'reflectance.csv',sep=''), skip=2,nrows = nsamples))
     SpecRefl <- hsdar::speclib(rfl[,c(1:2001)], wave[c(1:2001)])
     #plot(SpecRefl,ylab='radiance with F')
   }
   
   if (radiance == T){
     
     ######################################################################################
     ### Get Solar(Esun) and sky (Esky) irradiance above the canopy
     #irradiance in (W m-2 um-1)
     #Rin*(fEsun+fEsky)
     ######################################################################################
     # get diffuse top of canopy irradiance
     Esky_ <- as.matrix(data.table::fread(paste(pathin,'Esky.csv',sep=''), skip=2,nrows = nsamples ))
     # get direct top of canopy irradiance
     Esun_ <- as.matrix(data.table::fread(paste(pathin,'Esun.csv',sep=''), skip=2,nrows = nsamples))
     Etotal<-(Esky_+Esun_)/cos(0)
     SpecIrrad<- hsdar::speclib(Etotal[,c(1:2001)], wave[c(1:2001)])
     #plot(SpecIrrad,ylab='Irradiance')
     
     # get hemispherical outgoing radiation spectrum
     Etotal_ <- as.matrix(data.table::fread(paste(pathin,'Eout_spectrum.csv',sep=''), skip=2,nrows = nsamples))
     #SpecEtotal_<- hsdar::speclib(Etotal_[,c(1:2001)], wave[c(1:2001)])
     #plot(SpecEtotal_,ylab='Irradiance')
     
     #upwelling radiance including fluorescence
     #W m-2 um-1 sr-1
     rad.LoF <- as.matrix(data.table::fread(paste(pathin,'Lo_spectrum_inclF.csv',sep=''), skip=2,nrows = nsamples))
     SpecLoF <- hsdar::speclib(rad.LoF[,c(1:2001)], wave[c(1:2001)])
     #plot(SpecLoF,ylab='radiance with F')
     #plot(SpecLoF,FUN=min,ylab='radiance with F',xlim=c(740,780))
   }
   
   ## Radiation
   radiation<-data.table::fread(paste(pathin,'radiation.csv',sep=''),header=F,skip=2,nrows=nsamples,sep=',')
   colnames(radiation)<-c('simulation_number','year','DoY','ShortIn','LongIn','HemisOutShort','HemisOutLong','lo','Lot','Lote')
   
   ### Fluxes
   fluxes<-data.table::fread(paste(pathin,'fluxes.csv',sep=''),header=F,skip=2,nrows=nsamples,sep=',')
   colnames(fluxes)<-c('simulation_number','nu_iterations','year','DoY','Rnctot','lEctot','Hctot','Actot','Tcave','Rnstot','lEstot','Hstot','Gtot','Tsave','Rntot','lEtot','Htot')
   
   # Fluoresecence Parameters
   Fluo_scalar<-data.table::fread(paste(pathin,'fluorescence_scalars.csv',sep=''),header=F,skip=2,nrows=nsamples,sep=',')
   colnames(Fluo_scalar)<-c('F_1stpeak','wl_1stpeak','F_2ndpeak','wl_2ndpeak','F687','F760','LFtot','EFtot','EFtot_RC')
   
   # Vegeetation Parameters
   vegetation<-data.table::fread(paste(pathin,'vegetation.csv',sep=''),header=F,skip=2,nrows=nsamples,sep=',')
   colnames(vegetation)<-c('simulation_number','year','DoY','aPAR','aPARbyCab','aPARbyCab(energyunits)','Photosynthesis','Electron_transport','NPQ_energy','LST')
   #
   if (resampling == 'Sentinel2a'){
     if (radiance == T){
        SpecLoF.SE<-hsdar::spectralResampling(SpecLoF, "Sentinel2a",response_function = TRUE)
     }
        SpecRefl.SE<-hsdar::spectralResampling(SpecRefl, "Sentinel2a",response_function = TRUE)
   }
   if (SIF == T){
     Vcmax<-c()
     SIF_1nm<-c()
     SIF_SE<-c()
     message('Processing SIF by FLD2 method')
     progress_bar = txtProgressBar(min=0, max=nsamples, style = 3, char="=")
       for (m in c(1:nsamples)) {
         #print(m)
         Vcmax[m]<-inputs[m,'Vcmax25']
         ### rad at 1nm
         rad.total.i<-SpecLoF@spectra[m][341:451]
         wave_rad<-SpecLoF@wavelength[341:451]
         rad.SE.i<-SpecLoF.SE@spectra[m]
         wave.SE.rad<-SpecLoF.SE@wavelength
         
         wave.i<-SpecIrrad@wavelength[341:451]##wave from 740:850
         irrad.i<-data.frame( wave=wave.i,Eo=SpecIrrad@spectra[m][341:451])
         #print(rad.total.i[c(10,23)])
         SIF_1nm[m]<-ToolsRTM::getFLD2(rad.total.i,wave_rad,irrad.i)
         SIF_SE[m]<-ToolsRTM::getFLD2_SE(rad.SE.i,wave.SE.rad,irrad.i)
         setTxtProgressBar(progress_bar, value = m)
       }
   }
   ## get a LUT table
   
   outputs<-cbind(vegetation,Fluo_scalar,fluxes,radiation)
   if( SIF == T) {
     outputs$SIF_1nm<-SIF_1nm
     outputs$SIF_SE<-SIF_SE
   }
   
   LUT<-cbind(inputs,outputs)
 
   if (resampling == 'Sentinel2a'){
  
      rfl.sim<- raster::as.data.frame(SpecRefl.SE)
      colnames(rfl.sim)<-paste('R.',SpecRefl.SE@wavelength,sep='')
      LUT_rfl<-cbind(LUT,rfl.sim)
     if (radiance == T){
       rad.sim<-raster::as.data.frame(SpecLoF.SE)
       colnames(rad.sim)<-paste('L.',SpecLoF.SE@wavelength,sep='')
       LUT_rfl<-cbind(LUT_rfl,rad.sim)
     }
    } else{
      
        rfl.sim<-raster::as.data.frame(SpecRefl)
        colnames(rfl.sim)<-paste('R.',SpecRefl@wavelength,sep='')
        LUT_rfl<-cbind(LUT,rfl.sim)
          if (radiance == T){
            rad.sim<-raster::as.data.frame(SpecLoF)
            colnames(rad.sim)<-paste('L.',SpecLoF@wavelength,sep='')
            LUT_rfl<-cbind(LUT_rfl,rad.sim)
          }
        }
   
   return(LUT_rfl)

}

