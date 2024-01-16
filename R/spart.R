# Function to compute top-of-canopy (TOC) reflectance, top-of-atmosphere (TOA) reflectance, and top-of-atmosphere (TOA) radiance
#' Title
#'
#' @param inputLUT 
#' @param optipar 
#' @param CanopyModel 
#' @param LeafModel 
#' @param df.irradiance 
#' @param sensor.i 
#' @param get.plots 
#'
#' @return
#' @export
#'
#' @examples
SPART <-  function(inputLUT, optipar=NULL, CanopyModel = 'fourSAIL',LeafModel='PROSPECT-PRO',
                   sensor.i = NULL , df.irradiance=NULL, 
                   get.plots=T){
  
  ## Soil-Plant-Atmosphere Radiative Transfer model for top-of-canopy and top-of-atmosphere reflectance 
  # Developed by Peiqi Yang               (p.yang@utwente.nl)
  #              Christiaan van der Tol   (c.vandertol@utwente.nl)
  #              Wout Verhoef             (w.verhoef@utwente.nl)
  # ported to R C.Camino
  
  ##### Initial conditions
  # inputs = ToolsRTM::inputs.SPART
  # LUT<-as.data.frame(getLUT(inputs = inputs, nLUT=1, setseed = 1234))
  # inputLUT=LUT[1,]
  # optipar=SCOPEinR::optipar2021.Pro.CX;
  # CanopyModel = 'fourSAIL';LeafModel='PROSPECT-PRO';
  # sensor.i =ToolsRTM::LANDSAT7.ETM; get.plots=T;


  
  ##################################################################################
  ### 0.1 Check the plots (TRUE or FALSE)
  ##################################################################################
  
  if (missing(get.plots)){
    get.plots = FALSE
  } else {
    get.plots = TRUE
  }
  
  ##################################################################################
  ### 0.1 Check the model
  ##################################################################################
  
  if (missing(LeafModel)){
    LeafModel='PROSPECT-PRO'
  } 

  if (missing(CanopyModel)){
    CanopyModel='fourSAIL'
  } 
  
  if (missing(inputLUT)){
    inputLUT = ToolsRTM::inputs.SPART
    inputLUT<-as.data.frame(ToolsRTM::getLUT(inputs = inputs, nLUT=1, setseed = 1234))
  } 
  ##################################################################################
  ### 0.2 Check the canopy model
  ##################################################################################
  
  if (missing(CanopyModel)){
    model = 'fourSAIL'
    
  } else {
    model = CanopyModel
  }
  
  ##################################################################################
  ### 0.0 Load optipar (optical leaf properties)
  ##################################################################################
  
  if (missing(optipar)){
    optipar = SCOPEinR::optipar2021.Pro.CX
  } else{
    
    optipar =  optipar
    
  }
  
  df.optipar <- data.frame(Kp = optipar$Kp, 
                           Kcab = optipar$Kab,
                           Kca = optipar$Kca,
                           Kant = optipar$Kant,
                           Kcbc = optipar$Kcbc, 
                           wave = optipar$wl)
  
  ref.optipar <- ToolsRTM::dataSpec_PRO
  df.ref <- data.frame(Kp = ref.optipar$spA_Protein, 
                       Kcab = ref.optipar$spA_Cab,
                       Kca = ref.optipar$spA_Car,
                       Kant = ref.optipar$spA_Atn,
                       Kcbc = ref.optipar$spA_nonPro,
                       wave = ref.optipar$wave)
  if (get.plots ==  T){
    
    plot.optipar<- ggplot(data = df.optipar, aes(x = wave)) +
      labs(y = "specific absorption coefficient", x = "") + xlim(400, 2500) +
      geom_line(aes(y = Kp, color = "Proteins")) +
      geom_line(aes(y = Kcbc, color = "CBC")) +
      theme_bw() +
      guides(color = guide_legend(title = "Absorption coefficient"), linetype = guide_legend(title = "Absorption coefficient"), shape = guide_legend(title = "Absorption coefficient")) +
      
      theme(legend.position = "right")
    
    print(plot.optipar)
    
    plot.pigments<- ggplot(data = df.optipar, aes(x = wave)) +
      labs(y = "specific absorption coefficient", x = "") + xlim(400, 2500) +
      geom_line(aes(y = Kcab, color = "Cab")) +
      geom_line(aes(y = Kca, color = "Car")) +
      geom_line(aes(y = Kant, color = "Anth")) +
      theme_bw() +
      guides(color = guide_legend(title = "Absorption coefficient"), linetype = guide_legend(title = "Absorption coefficient"), shape = guide_legend(title = "Absorption coefficient")) +
      
      theme(legend.position = "right")
    
    print(plot.pigments)
    
  }
  
  ##################################################################################
  ### 0.1 Load spectral dataset (optical leaf properties)
  ##################################################################################
  
  data.spectral<-ToolsRTM::get.spectra.spart(getSpectral = T)

  IwlP <- data.spectral[['IwlP']]
  IwlT <- data.spectral[['IwlT']]
  nwlP <- data.spectral[['nwlP']]
  nwlT <- data.spectral[['nwlT']]
  
  ##################################################################################
  ### 1. Run the BSM model for soil reflectance
  ##################################################################################
  # Parameters for BSM model
  soilemp<-list()
  soilemp[['SMC']] = 25         # empirical parameter (fixed) for BSM (25, original value in SCOPE )
  soilemp[['film']] = 0.015    #empirical parameter (fixed) for BSM
  soilemp[['SMp']] = 15    #soil moisture volume percentage (5 - 55)
  
  data.opts = SCOPEinR::data.opts
  options.soil_heat_method     <-  data.opts[13,]    # Value 0 will estimate the GAM parameters with Soil_Inertia0(lambdas); Value 1 will estimate the GAM with Soil_Inertia1(SMC); Value 2 will estimate  G by 0.35*Rn (where always in no TS)
  options.calc_rss_rbs          <-  data.opts[14,] # Value 0 is fixed and Value 1 is calculated
  
  calc.heat = options.soil_heat_method$Value
  calc.rss_rbs = options.calc_rss_rbs$Value
  
  data.soil<-SCOPEinR::getinputLUT(inputLUT=SCOPEinR::SCOPE.LUT.default[1,], dataset='soil',
                                   calc.heat = calc.heat,
                                   calc.rss_rbs =  calc.rss_rbs)

  spec<-list()
  spec[['GSV']] <- optipar$GSV #optipar2020.prospectD.BSM2019$GSV
  spec[['Kw']] <-optipar$Kw # water absorption spectrum
  ## this is for get soil with 2500nm
  #spec[['Kw']] <-ref.optipar$spA_Cw[1:10] # water absorption spectrumr
  ##optipar2020.prospectD.BSM2019$nw
  spec[['nw']] <- optipar$nw # water refraction index spectrum
  
  
  #print('getting  soil reflectance using a Brightness-Shape-Moisture soil model model (BSM) ...')
  
  rsoil.BSM <- ToolsRTM::getBSM.toolsRTM(soilpar=data.soil,spec=spec,emp = soilemp);
  
  wave.comp = c(data.spectral[['reg1']],data.spectral[['reg2']],data.spectral[['reg3']])
  rfl.comp = c(c(rsoil.BSM), rep(data.soil[['rs_thermal']],length(data.spectral[['IwlT']])))
  
  db.rsoil <- data.frame(wave=wave.comp, rfl.soil = rfl.comp)
  
  if (get.plots ==  T){
    
    plot.soil <- ggplot(data = db.rsoil, aes(x = wave, y = rfl.soil)) +
      labs(y= "soil reflectance", x = "")+ xlim(400,2400) +
      geom_line() + theme_bw()
    
    print(plot.soil)
    
  }
  
  ##################################################################################
  ### 2. Run the model
  ##################################################################################
  
  
  model.sim<-ToolsRTM::foursail(inputLUT=inputLUT[1,],rsoil=db.rsoil[['rfl.soil']],
                                LeafModel = LeafModel, spectrum.all = F)
  
  rfl.canopy.brdf<-ToolsRTM::Compute_BRF(rdot=model.sim$rdot,rsot=model.sim$rsot,tts=LUT[1,'tts'],data.light=ToolsRTM::dataSpec_PDB,  short.waves=T)
  
  df.toc <- data.frame(wave= data.spectral[['wlO']], rfl_toc = rfl.canopy.brdf)
  
  ### get SMAC coeff. for specifi sensor
  sensors.properties = get.coef.SMAC(sensor = sensor.i)
  wlSensor =  sensors.properties[['wl.smac']]
  coefs.SMAC  = sensors.properties[['coefs.SMAC']]
  Sensor.name = sensors.properties[['Sensor.name']]
  center.wvl <- sensors.properties[['center.wvl']]
  wl.smac <- sensors.properties[['wl.smac']]
  
  df.sensor = data.frame(wlSensor= wlSensor,center.wvl=center.wvl,wl.smac=wl.smac  )
  ### Interpolation using sensor
  rv_so <- signal::interp1(data.spectral[['wlO']], model.sim[['rsot']] ,wlSensor,'spline',1E-4)
  rv_do  <- signal::interp1(data.spectral[['wlO']], model.sim[['rdot']] ,wlSensor,'spline',1E-4)
  rv_dd <- signal::interp1(data.spectral[['wlO']], model.sim[['rddt']] ,wlSensor,'spline',1E-4)
  rv_sd <- signal::interp1(data.spectral[['wlO']], model.sim[['rsdt']] ,wlSensor,'spline',1E-4)
  
  rfl.canopy.brdf.sensor <- signal::interp1(data.spectral[['wlO']], rfl.canopy.brdf ,wlSensor,'spline',1E-4)
  # Run the atmosphere radiative transfer model
  atmopt <- get.smac(inputLUT=inputLUT, sensor= sensor.i )
  
  # Extract results from the model output
  ta_ss <- atmopt[['Ta_ss']]
  ta_sd <- atmopt[['Ta_sd']]
  ta_oo <- atmopt[['Ta_oo']]
  ta_do <- atmopt[['Ta_do']]
  ra_dd <- atmopt[['Ra_dd']]
  ra_so <- atmopt[['Ra_so']]
  T_g   <- atmopt[['Tg']]
  
  # Upscale TOC to TOA
  R_TOC <- (ta_ss * rv_so + ta_sd * rv_do) / (ta_ss + ta_sd)
  
  # Calculate components for TOA reflectance
  rtoa1 <- (ta_sd * rv_do + ta_ss * rv_sd * ra_dd * rv_do) * ta_oo / (1 - rv_dd * ra_dd)
  rtoa2 <- (ta_ss * rv_sd + ta_sd * rv_dd) * ta_do / (1 - rv_dd * ra_dd)
  rtoa0 <- ra_so + ta_ss * rv_so * ta_oo
  
  # Calculate TOA reflectance
  R_TOA <- T_g * (rtoa0 + rtoa1 + rtoa2)
  
  df.refl <- data.frame(wave = wlSensor,rfl.toa = R_TOA, rfl.canopy.brdf= rfl.canopy.brdf.sensor )
  
  if (get.plots ==  T){
    
    plot.rfl.toa <- ggplot(data = df.refl, aes(x = wave)) +
      labs(y = "reflectance", x = "") + xlim(400, 2450) +
      geom_point(aes(y = rfl.toa, color = "TOA Reflectance"), size = 2) +
      geom_line(aes(y = rfl.toa, color = "TOA Reflectance")) +
      geom_point(aes(y = rfl.canopy.brdf, color = "Canopy BRDF Reflectance"), size = 2) +
      geom_line(aes(y = rfl.canopy.brdf, color = "Canopy BRDF Reflectance")) +
      theme_bw() +
      guides(color = guide_legend(title = "Reflectance"), linetype = guide_legend(title = "Reflectance"), shape = guide_legend(title = "Reflectance")) +
      
      theme(legend.position = "top")
    
    print(plot.rfl.toa)
    
  }
  
  # Default dataset when df.irradiance is NULL
  if (missing(df.irradiance) || is.null(df.irradiance)) {

    wl_Ea = ToolsRTM::Extraterrestrial_irradiance$wave # wl
    Ea0 = ToolsRTM::Extraterrestrial_irradiance$EIrrad # Ea0 = W M-2 nm-1
    df.irradiance <- data.frame(wave = wl_Ea, irrad = Ea0)
  }
  
  if (!is.null(df.irradiance) && is.data.frame(df.irradiance)) {
    
    df.irradiance <- data.frame(wave = df.irradiance[,1], irrad = df.irradiance[,2])
  } else {
   stop('please provide a dataframe with wave and Eo ...')
  }
  


  if (get.plots ==  T){
    
    plot.Irrad <- ggplot(data = df.irradiance, aes(x = wave, y = irrad)) +
      labs(y= " Extraterrestrial irradiance", x = "")+ xlim(400,2450) +
      geom_line() + theme_bw()
    
    print(plot.Irrad)
    
  }
  
  La_extra <-  ToolsRTM::get.spectral.convolution(df.irradiance = df.irradiance,sensor.i, get.plots=F)   
  # Calculate TOA radiance
  L_TOA <- La_extra$Eo * R_TOA
  
  df.rad.toa<- data.frame(wave = La_extra$wave, rad.toa = L_TOA, rfl.toa = R_TOA)
  
  if (get.plots ==  T){
    
    plot.rad <- ggplot(data = df.rad.toa, aes(x = wave)) +
      labs(y= " TOA rfl-rad", x = "") +
      geom_line(aes(y = rad.toa, color = "Rad")) +
      geom_line(aes(y = rfl.toa, color = "RFL")) + theme_bw() +
      guides(color = guide_legend(title = "TOA"), linetype = guide_legend(title = "TOA"), shape = guide_legend(title = "TOA")) 
      
    
    print(plot.rad)
    
  }
  
  df.all <- data.frame(wave = wlSensor, rad.toa = L_TOA, rfl.toa = R_TOA, rfl_TOC_BRDF= rfl.canopy.brdf.sensor)
  rownames(df.all) <-NULL
  outputs <- list(TOA = df.all, Irrad_TOA = La_extra, rfl.toc.brdf = df.toc, 
                  rf.soil = db.rsoil, sensor = df.sensor, 
                  inputLUT)
  
  return(outputs)
  
    
}
