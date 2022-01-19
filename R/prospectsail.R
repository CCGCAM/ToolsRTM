##############################################################################################################################
######################## 2.   CALL  ModelPRO4SAIL  ----     
##############################################################################################################################

# rdot: hemispherical-directional reflectance factor in viewing direction
# rsot: bi-directional reflectance factor
# rsdt: directional-hemispherical reflectance factor for solar incident flux
# rddt: bi-hemispherical reflectance factor

prospectsail<- function(LUT=NULL, rsoil=NULL, PROSPECTversion=NULL){
    n_cases=dim(LUT)[1]
    version=PROSPECTversion
    #### Inversion without LMA data info
    sim.rfl<-list()
    simulations<-foreach::foreach(i=1:n_cases) %dopar% {
      data.prosail<-PRO4SAIL(LUT[i,1],LUT[i,2],LUT[i,3],LUT[i,4],LUT[i,5],LUT[i,6],LUT[i,7],LUT[i,8],LUT[i,9],
                                         LUT[i,10],LUT[i,11],LUT[i,12],LUT[i,13],LUT[i,14],LUT[i,15],LUT[i,16],LUT[i,17],
                                         rsoil[[i]],PROSPECTversion = version)
      #data.prosail is a  list(rdot,rsot,rddt,rsdt)
      rdot<-data.prosail[[1]]
      rsot<-data.prosail[[2]]
      
      ##############################
      #	direct / diffuse light	##
      ##############################
      # the direct and diffuse light are taken into account as proposed by:
      # Francois et al. (2002) Conversion of 400?1100 nm vegetation albedo
      # measurements into total shortwave broadband albedo using a canopy
      # radiative transfer model, Agronomie
      # Es = direct
      # Ed = diffuse
      
      Es  <- data[,9]
      Ed  <- data[,10]
      rd  <- pi/180
      skyl	 <- 	0.847- 1.61*sin((90-LUT$tts[i])*rd)+ 1.04*sin((90-LUT$tts[i])*rd)*sin((90-LUT$tts[i])*rd)# # diffuse radiation
      
      PARdiro	 <- 	(1-skyl)*Es
      PARdifo	 <- 	(skyl*Ed)
      #print(paste('simulation ',i,sep=''))
          
      resv	 <-  (rdot*PARdifo+ rsot*PARdiro)/(PARdiro+PARdifo)  # resv : directional reflectance
      sim.rfl[[i]]<-resv
          
      } ##end paralle
    sims=simulations
return(sims)  
}


