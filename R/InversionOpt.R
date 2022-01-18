
InversionOpt_nOpt<-function (rfl.sensor=NULL,rfl.prosail=NULL,LUT=NULL,wave=NULL, n=NULL,method=NULL,nOpt=NULL) 
{
  ##rfl.sensor is a matrix
  ##rfl.prosail is a matrix
  ### wave wavelengths
  ### n_cases =n (number of simulations)
  ##method is the method (opt ='merit-RMSE','merit-DWT',merit-1stD')
  ## LUT = Look-up table with the inputs
  ###############################################################################
  ###############################################################################
  ##### RMSE function -------
  rmse_f = function(sim,obs){
    sqrt(mean((sim - obs)^2))}
  ###############################################################################
  ###############################################################################
  ## Outputs-------
  RMSE_sim<-c()
  number_id<-list()
  
  RMSE_<-list()
  LUT_<-list()
 
  Table.rmse<-list()
  Table.rmse.nOpt<-list()
  Table.rmse.mean<-list()
  rfl.best<-list()
  ###############################################################################
  ###############################################################################
  
  if (is.null(method) | method == 'merit-RMSE') {
    method='merit-RMSE'
    version<-method
    print(message('Merit fuction using RMSE without transformation is processing'))
    ###############################################################################
    ###############################################################################
    progress_bar = txtProgressBar(min=0, max=dim(rfl.sensor)[1], style = 3, char="=")
    for (i in (1:dim(rfl.sensor)[1])){
      rfl.sensor.i<-rfl.sensor[i,]
      for (j in 1:n_cases){
        rfl.prosail.i<-rfl.prosail[j,]
        rmse<-rmse_f(rfl.prosail.i,rfl.sensor.i)
        RMSE_sim[j]=rmse
        number_id[j]=j
        
      }
      RSME_j<-do.call(rbind, lapply(RMSE_sim, as.data.frame))
      #Table.rmse<-cbind(ID_lut = 1:n_cases, RSME_j)
      Table.rmse<-cbind(ID_lut = 1:dim(LUT)[1], RSME_j)
      colnames(Table.rmse)<-c('ID_lut','RMSE')
      Table.rmse<-cbind(Table.rmse,LUT)
      ## order for spectrum
      Table.rmse<-Table.rmse[order(Table.rmse$RMSE),]
      best<-Table.rmse$ID_lut[1:nOpt]
      
      ## best  (nOpt)
      Table.rmse.nOpt[[i]]<-Table.rmse[1:nOpt,]
      Table.rmse.mean[[i]]<-round(colMeans(Table.rmse.nOpt[[i]]),3)
      #Table.rmse.mean[[i]]<-round(robustbase::colMedians(as.matrix(Table.rmse.nOpt[[i]])),3)
      if (nOpt == 1) {
      rfl.best[[i]]<-rfl.prosail[best[nOpt],]
      }
      if (nOpt != 1) {
      rfl.best[[i]]<-colMeans(rfl.prosail[best[1:nOpt],])
      }
      
      RMSE_[[i]]<-RMSE_sim

     
      setTxtProgressBar(progress_bar, value = i)
    }
    close(progress_bar)
      ###############################################################################
      ###############################################################################
    
  }

  if (method == 'merit-DWT') {
    version<-method
    print(message('Merit fuction using RMSE with DW transformation is processing'))
    if (!require("wavelets")) { install.packages("wavelets"); require("wavelets") }  ### load wavelets packages
    
    ###############################################################################
    ###############################################################################
    progress_bar = txtProgressBar(min=0, max=dim(rfl.sensor)[1], style = 3, char="=")
    for (i in (1:dim(rfl.sensor)[1])){
      rfl.sensor.i<-rfl.sensor[i,]
      for (j in 1:n_cases){
        rfl.prosail.i<-rfl.prosail[j,]
        wt.dwt.sim <- dwt(rfl.prosail.i, filter="haar", fast = T)
        wt.dwt.sim<-as.vector(unlist(wt.dwt.sim@W))
        wt.dwt.hyper <- dwt(rfl.sensor.i, filter="haar", fast = T)
        wt.dwt.hyper<-as.vector(unlist(wt.dwt.hyper@W))
        rmse<-rmse_f(wt.dwt.sim,wt.dwt.hyper)
        RMSE_sim[j]=rmse
        number_id[j]=j
        
      }
      RSME_j<-do.call(rbind, lapply(RMSE_sim, as.data.frame))
      #Table.rmse<-cbind(ID_lut = 1:n_cases, RSME_j)
      Table.rmse<-cbind(ID_lut = 1:dim(LUT)[1], RSME_j)
      colnames(Table.rmse)<-c('ID_lut','RMSE')
      ## order for spectrum
      Table.rmse<-cbind(Table.rmse,LUT)
      Table.rmse<-Table.rmse[order(Table.rmse$RMSE),]

      best<-Table.rmse$ID_lut[1:nOpt]
      
      ## best  (nOpt)
      Table.rmse.nOpt[[i]]<-Table.rmse[1:nOpt,]
      Table.rmse.mean[[i]]<-round(colMeans(Table.rmse.nOpt[[i]]),3)
      #Table.rmse.mean[[i]]<-round(robustbase::colMedians(as.matrix(Table.rmse.nOpt[[i]])),3)
      if (nOpt == 1) {
        rfl.best[[i]]<-rfl.prosail[best[nOpt],]
      }
      if (nOpt != 1) {
        rfl.best[[i]]<-colMeans(rfl.prosail[best[1:nOpt],])
      }
      
      RMSE_[[i]]<-RMSE_sim
      
      setTxtProgressBar(progress_bar, value = i)
    }
    close(progress_bar)
    ###############################################################################
    ###############################################################################
    
  }
  
  else if (method == 'merit-1stD') {
    version<-method
    print(message('Merit fuction using RMSE with 1st derivative transformation is processing'))
    if (!require("hsdar")) { install.packages("hsdar"); require("hsdar") }  ### load hsdar packages
    
    ### sensor
    sp.sensor<-speclib(rfl.sensor, wave)
    d1.sensor<- derivative.speclib(sp.sensor)
    d1.sensor<-as.matrix(spectra(d1.sensor))
    ### simulations
    sp.sim<-speclib(rfl.prosail, wave)
    d1.sim<- derivative.speclib(sp.sim)
    d1.sim<-as.matrix(spectra(d1.sim))
    ###############################################################################
    ###############################################################################
    progress_bar = txtProgressBar(min=0, max=dim(rfl.sensor)[1], style = 3, char="=")
    
    for (i in (1:dim(d1.sensor)[1])){
      d1.sensor.i<-d1.sensor[i,]
      for (j in 1:n_cases){
        d1.sim.i<-d1.sim[j,]
        rmse<-rmse_f(d1.sim.i,d1.sensor.i)
        RMSE_sim[j]=rmse
        number_id[j]=j
        
      }
      RSME_j<-do.call(rbind, lapply(RMSE_sim, as.data.frame))
      #Table.rmse<-cbind(ID_lut = 1:n_cases, RSME_j)
      Table.rmse<-cbind(ID_lut = 1:dim(LUT)[1], RSME_j)
      colnames(Table.rmse)<-c('ID_lut','RMSE')
      Table.rmse<-cbind(Table.rmse,LUT)
      ## order for spectrum
      Table.rmse<-Table.rmse[order(Table.rmse$RMSE),]
      best<-Table.rmse$ID_lut[1:nOpt]
      ## best  (nOpt)
      Table.rmse.nOpt[[i]]<-Table.rmse[1:nOpt,]
      Table.rmse.mean[[i]]<-round(colMeans(Table.rmse.nOpt[[i]]),3)
      #Table.rmse.mean[[i]]<-round(robustbase::colMedians(as.matrix(Table.rmse.nOpt[[i]])),3)
      if (nOpt == 1) {
        rfl.best[[i]]<-rfl.prosail[best[nOpt],]
      }
      if (nOpt != 1) {
        rfl.best[[i]]<-colMeans(rfl.prosail[best[1:nOpt],])
      }
      
      RMSE_[[i]]<-RMSE_sim
      
      setTxtProgressBar(progress_bar, value = i)
    }
    close(progress_bar)
    ###############################################################################
    ###############################################################################
    
  }
  ###############################################################################
  ###############################################################################

  ## for the lowest spectra from PROSAIL (nOpt with lowest RMSE)
  
  LUT.best<-as.data.frame(do.call(rbind, Table.rmse.mean))
  rfl.b<-do.call(rbind, rfl.best)
  colnames(rfl.b)<-paste('R',wave,sep='.')
  Table.best<-cbind(ID = c(1:dim(rfl.sensor)[1]), LUT.best,rfl.b)
 
  
  return(list(Table.best))
  
}


