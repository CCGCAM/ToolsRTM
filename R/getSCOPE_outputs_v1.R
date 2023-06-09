
#' Get in a LUT all main outputs from SCOPE model
#'
#' @param pathin path with the outfile directory
#' @param nsamples number of simulations
#' @param resampling  for resampling simulation at Sentinel-2 resolution, option 'Sentinel2a' is null no resampling i used
#' @param reflectance Type of reflectance 'apparent' or  'reflectance', NULL get the apparent reflectance (adding Fluorescence)
#' @param SIF is TRUE estimate the Fluorescence emission based on FLD-2 method, Need radiance parameter
#' @param radiance  is TRUE get Radiance
#'
#' @return outputs from SCOPE model
#' @export
#'
#' @examples here adding examples ....
#' 
getSCOPE_outputs_v1 <- function(pathin=NULL,nsamples=100, resampling,
                            reflectance ='apparent', SIF=T,
                            radiance =T) {
  
  if (is.null(pathin)){
    stop('please give a path to find the SCOPE tables ...')
  } else{
    ## get files
    #pathin = 'examples/SCOPE/OHP_2022-02-25-1826/'
    list.outs<-list.files(pathin,full.names =T)
   
  }
  ## get wavelength
  
  data.spectral <-SCOPEinR::define.bands()
  wave<- data.spectral$wlS
  
  inputs<-data.table::fread(paste(pathin,'pars_and_input_short.csv',sep=''),header=F,skip=2,nrows=nsamples,sep=',')
  if (length(colnames(inputs)) == 18){
    colnames(inputs)<-c('n_pars','Cab','Cca','Cdm','Cw','N','Cant','Vcmax25','BallBerrySlope','LAI','LIDFa','Rin','Ta','Rli','u','Ca','tts','tto')
    
  } else if (length(colnames(inputs)) == 20) {
    colnames(inputs)<-c('n_pars','Cab','Cca','Cdm','Cw','N','Cant','Vcmax25','BallBerrySlope','LAI','LIDFa','Rin','Ta','Rli','p','ea','u','Ca','tts','tto')
    
  } else if (length(colnames(inputs)) == 24) {
      ## version SCOPE 2.1
    colnames(inputs)<-c('n_pars','Cab','Cca','Cdm','Cw','Cs','N','Cant','Cp','Cbc','Vcmax25','BallBerrySlope','LAI','LIDFa','Rin','Ta','Rli','p','ea','u','Ca','tts','tto','psi')
    
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
     pathIrrad = 'Tables/LUTs/SCOPE/'
     
     Esky_ <- as.matrix(data.table::fread(paste(pathIrrad,'Esky.csv',sep=''), skip=2,nrows = nsamples ))
     # get direct top of canopy irradiance
     Esun_ <- as.matrix(data.table::fread(paste(pathIrrad,'Esun.csv',sep=''), skip=2,nrows = nsamples))
     Etotal<-(Esky_+Esun_)/cos(0)
     SpecIrrad<- hsdar::speclib(Etotal[,c(1:2001)], wave[c(1:2001)])
     #plot(SpecIrrad,ylab='Irradiance')
     
  
     #upwelling radiance including fluorescence
     #W m-2 um-1 sr-1
     rad.LoF <- as.matrix(data.table::fread(paste(pathin,'Lo_spectrum_inclF.csv',sep=''), skip=2,nrows = nsamples))
     SpecLoF <- hsdar::speclib(rad.LoF[,c(1:2001)], wave[c(1:2001)])
     #plot(SpecLoF,ylab='radiance with F')
     #plot(SpecLoF,FUN=min,ylab='radiance with F',xlim=c(740,780))
   }
   
  ## Apar
  aPAR<-data.table::fread(paste(pathin,'aPAR.csv',sep=''),header=F,skip=2,nrows=nsamples,sep=',')
  
  colnames(aPAR)<-c('simulation_number','year','DoY','iPAR','iPARE','LAIsunlit','LAIshaded','aPARtot',
                         'aPARsun','aPARsha','aPARCabtot','aPARCabsun','aPARCabsha','aPARCartot','aPARCarsun',
                         'aPARCarsha','aPARtotE','aPARsunE','aPARshaE','aPARCabtotE','aPARCabsunE','aPARCabshaE',
                         'aPARCartotE','aPARCarsunE','aPARCarshaE')
  
  
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
   ##old version
   # colnames(vegetation)<-c('simulation_number','year','DoY','aPAR','aPARbyCab','aPARbyCab(energyunits)','Photosynthesis','Electron_transport','NPQ_energy','LST')
   ## new version
   colnames(vegetation)<-c('simulation_number','year','DoY','Photosynthesis','Electron_transport','NPQ_energy','NPQ_photon','canopy_level_FQE','LST','emis')

   
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
         #rad.SE.i<-SpecLoF.SE@spectra[m]
         #wave.SE.rad<-SpecLoF.SE@wavelength
         
         wave.i<-SpecIrrad@wavelength[341:451]##wave from 740:850
         irrad.i<-data.frame( wave=wave.i,Eo=SpecIrrad@spectra[1][341:451])
         #print(rad.total.i[c(10,23)])
         SIF_1nm[m]<-ToolsRTM::getFLD2(rad.total.i,wave_rad,irrad.i)
         #SIF_SE[m]<-ToolsRTM::getFLD2_SE(rad.SE.i,wave.SE.rad,irrad.i)
         setTxtProgressBar(progress_bar, value = m)
       }
   }
   ## get a LUT table
   
   outputs<-as.data.frame(cbind(vegetation,Fluo_scalar,fluxes,aPAR,radiation))
   # Find Duplicate Column Names
   duplicated_names <- duplicated(colnames(outputs))
   # Remove Duplicate Column Names
   outputs<-outputs[!duplicated_names]
   
   if( SIF == T) {
     outputs$SIF_1nm<-SIF_1nm
     #outputs$SIF_SE<-SIF_SE
   }
   
   LUT<-cbind(inputs,outputs)
 
   rfl.sim<-raster::as.data.frame(SpecRefl)
   colnames(rfl.sim)<-paste('R.',SpecRefl@wavelength,sep='')
   LUT_rfl<-cbind(LUT,rfl.sim)
   
   if (radiance == T){
     rad.sim<-raster::as.data.frame(SpecLoF)
     colnames(rad.sim)<-paste('L.',SpecLoF@wavelength,sep='')
     LUT_rfl<-cbind(LUT_rfl,rad.sim)
   }
   
   
   return(LUT_rfl)

}

