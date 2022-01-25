
# Clement Atzberger im November 1995
# version adpated to R by Carlos Camino
#' SAIL-PROSPECT-SOIL to compute crown transmittance for observation direction
#'
#' @param lai  LAI 
#' @param ala average angle of leaf inclination (degrees LIDFa when TypeLIDF ==2)
#' @param hot arametre hotspot: rapport entre diametre feuille et hauteur strate
#' @param tto zenith angle of obsevation
#' @param skyl diffuse radiation component
#' @param rsoil reflectance soil (reference)
#' @param tts solar zenith angle
#' @param phi  Azimuth angle 
#' @param refl leaf reflectance tran 
#' @param tran leaf reflectance tran 
#' @return
#' @export
#'
#' @examples
#' 
sail_t_o<-function(lai,ala,hot,tto,skyl,rsoil,tts,psi,TypeLidf=2,refl,tran){

  rho	 <- refl
  tau	 <- 	tran
  tts = tto
  LIDFa <- ala
  # ------ Begin SAIL-Modells       ------------ #
  ########################################
  #	1.2 Geometric quAnthities
  #########################################
  rd <- pi/180
  cts		 <-  cos(rd*tts)
  cto		 <-  cos(rd*tto)
  ctscto	 <-  cts*cto
  tAnths	 <-  tan(rd*tts)
  tAntho	 <-  tan(rd*tto)
  cospsi	 <-  cos(rd*psi)
  dso		 <-  sqrt(tAnths*tAnths+tAntho*tAntho-2*tAnths*tAntho*cospsi)
  
  
  ###########################################################################################################################
  #	1.3 Generate leaf angle distribution from average leaf angle (ellipsoidal) or (a,b) parameters
  ###########################################################################################################################
  
  if (TypeLidf==1){
    LeafDistribution <- dladgen(LIDFa,LIDFb)
    lidf <- LeafDistribution$lidf
    litab <- LeafDistribution$litab
    
  } else if (TypeLidf==2){
    LeafDistribution <- campbell(LIDFa)
    lidf <- LeafDistribution$lidf
    litab <- LeafDistribution$litab
  }
  
  # angular distance, compensation of shadow length
  #	Calculate geometric factors associated with extinction and scattering
  #	Initialise sums
  ks	 <-  0
  ko	 <-  0
  bf	 <-  0
  sob	 <-  0
  sof	 <-  0
  
  #	Weighted sums over LIDF
  na <- length(litab)
  #ksli<-rep(NA,13)
  for (i in 1:na){
    ttl <- litab[i]# leaf inclination discrete values
    
    ctl <- cos(rd*ttl)
    #	SAIL volume scattering phase function gives interception and portions to be
    #	multiplied by rho and tau
    
    chi_s_chi_o_frho_ftau <- volscatt(tts,tto,psi,ttl)
    chi_s<-chi_s_chi_o_frho_ftau[[1]]
    chi_o<-chi_s_chi_o_frho_ftau[[2]]
    frho<-chi_s_chi_o_frho_ftau[[3]]
    ftau<-chi_s_chi_o_frho_ftau[[4]] 
    
    #********************************************************************************
    #*                   SUITS SYSTEM COEFFICIENTS
    #*
    #*	ks  : Extinction coefficient for direct solar flux
    #*	ko  : Extinction coefficient for direct observed flux
    #*	att : Attenuation coefficient for diffuse flux
    #*	sigb : Backscattering coefficient of the diffuse downward flux
    #*	sigf : Forwardscattering coefficient of the diffuse upward flux
    #*	sf  : Scattering coefficient of the direct solar flux for downward diffuse flux
    #*	sb  : Scattering coefficient of the direct solar flux for upward diffuse flux
    #*	vf   : Scattering coefficient of upward diffuse flux in the observed direction
    #*	vb   : Scattering coefficient of downward diffuse flux in the observed direction
    #*	w   : Bidirectional scattering coefficient
    #********************************************************************************
    
    #	Extinction coefficientsksli
    ksli <- chi_s/cts
    koli <- chi_o/cto
    
    #	Area scattering coefficient fractions
    sobli	 <-  frho*pi/ctscto
    sofli	 <-  ftau*pi/ctscto
    bfli	 <-  ctl*ctl
    ks	 <-  ks+ksli*lidf[i]
    
    ko	 <-  ko+koli*lidf[i]
    bf	 <-  bf+bfli*lidf[i]
    sob	 <-  sob+sobli*lidf[i]
    sof	 <-  sof+sofli*lidf[i]
  }
  
  ######################################################
  #	Geometric factors to be used later with rho and tau
  #####################################################
  
  sdb	 <-  0.5*(ks+bf)
  sdf	 <-  0.5*(ks-bf)
  dob	 <-  0.5*(ko+bf)
  dof	 <-  0.5*(ko-bf)
  ddb	 <-  0.5*(1+bf)
  ddf	 <-  0.5*(1-bf)
  
  #	Here rho and tau come in
  sigb <-  ddb*rho+ddf*tau
  sigf <-  ddf*rho+ddb*tau
  att	 <-  1-sigf
  m2  <- (att+sigb)*(att-sigb)
  m2[which(m2<= 0)]<-0
  
  m    <- sqrt(m2)
  
  sb  <- sdb*rho+sdf*tau
  sf	 <-  sdf*rho+sdb*tau
  vb	 <-  dob*rho+dof*tau
  vf	 <-  dof*rho+dob*tau
  w	 <-  sob*rho+sof*tau
  
  ######################################################
  #	Here the LAI comes in
  #   Outputs for the case LAI = 0
  ######################################################
  
  if (lai<0){
    tss		 <-  1
    too		 <-  1
    tsstoo	 <-  1
    rdd		 <-  0
    tdd		 <-  1
    rsd		 <-  0
    tsd		 <-  0
    rdo		 <-  0
    tdo		 <-  0
    rso		 <-  0
    rsos	 <-  0
    rsod	 <-  0
    
    rddt	 <-  rsoil
    rsdt	 <-  rsoil
    rdot	 <-  rsoil
    rsodt	 <-  0*rsoil
    rsost	 <-  rsoil
    rsot	 <-  rsoil
    
  }
  ######################################################
  #	Other cases (LAI > 0)
  ######################################################
  
  e1		 <-  exp(-m*lai)
  e2		 <-  e1*e1
  rinf	 <-  (att-m)/sigb
  rinf2	 <-  rinf*rinf
  re		 <-  rinf*e1
  denom	 <-  1-rinf2*e2
  
  ############### 
  
  J1ks    <- Jfunc1(ks,m,lai)
  J2ks    <- Jfunc2(ks,m,lai)
  J1ko    <- Jfunc1(ko,m,lai)
  J2ko    <- Jfunc2(ko,m,lai)
  
  Ps  <- (sf+sb*rinf)*J1ks
  Qs  <- (sf*rinf+sb)*J2ks
  Pv  <- (vf+vb*rinf)*J1ko
  Qv  <- (vf*rinf+vb)*J2ko
  
  rdd	 <-  rinf*(1-e2)/denom
  tdd	 <-  (1-rinf2)*e1/denom
  tsd	 <-  (Ps-re*Qs)/denom
  rsd	 <-  (Qs-re*Ps)/denom
  tdo	 <-  (Pv-re*Qv)/denom
  rdo	 <-  (Qv-re*Pv)/denom
  
  tss	 <-  exp(-ks*lai)
  too	 <-  exp(-ko*lai)
  z	 <-  Jfunc3(ks,ko,lai)
  g1	 <-  (z-J1ks*too)/(ko+m)
  g2	 <-  (z-J1ko*tss)/(ks+m)
  
  Tv1 <- (vf*rinf+vb)*g1
  Tv2 <- (vf+vb*rinf)*g2
  T1	 <-  Tv1*(sf+sb*rinf)
  T2	 <-  Tv2*(sf*rinf+sb)
  T3	 <-  (rdo*Qs+tdo*Ps)*rinf
  
  #############################################################################
  #	Multiple scattering contribution to bidirectional canopy reflectance
  rsod <- (T1+T2-T3)/(1-rinf2)
  #############################################################################
  
  
  #############################################################################
  #	Treatment of the hotspot-effect
  #############################################################################
  
  alf <- 1e6
  
  ######################################################################
  #	Apply correction 2/(K+k) suggested by F-M Br?on
  if (hot >0 ){
    alf <- (dso/hot)*2/(ks+ko)
  }
  ######################################################################
  
  
  ######################################################################
  if (alf>200){# inserted H Bach 1/3/04
    alf <- 200
  }
  ######################################################################
  
  ######################################################################
  if (alf==0) {
    #	The pure hotspot - no shadow
    tsstoo <- tss
    sumint <- (1-tss)/(ks*lai)
    
  } else {
    
    ######################################################################
    #	Outside the hotspot
    ######################################################################  
    fhot <- lai*sqrt(ko*ks)
    #	Integrate by exponential Simpson method in 20 steps
    #	the steps are arranged according to equal partitioning
    #	of the slope of the joint probability function
    x1 <- 0
    y1 <- 0
    f1 <- 1
    fint <- (1.-exp(-alf))*0.05
    sumint <- 0
    
    for (i in 1:20){
      if (i<20){ 
        x2 <- -log(1-i*fint)/alf 
      } else {
        x2 <- 1 }
      
      y2 <- -(ko+ks)*lai*x2+fhot*(1-exp(-alf*x2))/alf
      #print(y2)
      f2 <- exp(y2)
      #print(sumint)
      
      sumint <- sumint+(f2-f1)*(x2-x1)/(y2-y1)
      
      x1 <- x2
      y1 <- y2
      f1 <- f2
    }
    tsstoo <- f1
  }
  ######################################################################
  
  
  
  #	Bidirectional reflectance
  #	Single scattering contribution
  rsos <- w*lai*sumint
  #	Total canopy contribution
  rso <- rsos+rsod
  
  #	Interaction with the soil
  dn <- 1-rsoil*rdd
  # rddt: bi-hemispherical reflectance factor
  rddt <- rdd+tdd*rsoil*tdd/dn
  # rsdt: directional-hemispherical reflectance factor for solar incident flux
  rsdt <- rsd+(tsd+tss)*rsoil*tdd/dn
  # rdot: hemispherical-directional reflectance factor in viewing direction
  rdot <- rdo+tdd*rsoil*(tdo+too)/dn
  # rsot: bi-directional reflectance factor
  rsodt <- rsod+((tss+tsd)*tdo+(tsd+tss*rsoil*rdd)*too)*rsoil/dn
  rsost <- rsos+tsstoo*rsoil
  rsot <- rsost+rsodt
  
  

  #############################################################################################
  #############################################################################################
  
  PARdifo=skyl;
  PARdiro=1-skyl;
  
  trans_hemi=(rdot*PARdiro+rsot*PARdifo)/(PARdiro+PARdifo);
  
  
  return(trans_hemi)
  
}
