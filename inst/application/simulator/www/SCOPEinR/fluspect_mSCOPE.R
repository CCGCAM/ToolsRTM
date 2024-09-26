#' #' @title get.fluspect_mSCOPE
#' \code{get.fluspect_mSCOPE} an adaptation of the fluspect model for SCOPE model
#'
#' @param mly
#' @param spectralinformation about wavelengths and resolutions
#' @param leafbio biochemical properties
#' @param soil  soil properties
#' @param optipar optical properties at leaf level
#' @param nl canopy layers
#' @param step a step for wavelengths for getting a matrix of Mf and Mb, by default uses a step of 1 nm, This step return matrix of 211x351
#' the SCOPE model uses a step of 5 for getting matrix of 53x71
#' @param get.plots  is true plot the intermediate plots
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol  (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
get.fluspect_mSCOPE <- function(mly, spectral, leafbio, soil, optipar, nl,step,get.plots=T) {


  if (missing(get.plots)){
    get.plots = FALSE
  }

  leafopt<-list()
  # leaf reflectance, transmittance and the excitation-fluorescence matrices calculation
  # for 60 sublayers
  indStar <- c(1, floor(cumsum(mly[['pLAI']]/sum(mly[['pLAI']])) * nl))  # index of starting for each different layer


  if (missing(step)){
    step_to_model <- 5
    ## not use  this print for get.SCOPE reasons
    #print(paste('fluspect model was running at ',step_to_model,' nm ',sep=''))
    wle <- spectral[['wlE']]  # excitation wavelengths, transpose to column
    wlf <- spectral[['wlF']]  # fluorescence wavelengths, transpose to column
  } else {
    step_to_model <- step

    if (step_to_model >= 8){

      stop('The fluspect model needs to run with a step lower than 8 nm')

    } else if (step_to_model == 1) {

      ## not use  this print for get.SCOPE reasons
      #print('fluspect model was running at 1 nm ')

      wle <- seq(min(spectral[['wlE']]),max(spectral[['wlE']]),step)
      wlf <- seq(min(spectral[['wlF']]),max(spectral[['wlF']]),step)

    } else {
      ## not use  this print for get.SCOPE reasons
      #print(paste('fluspect model was running at ',step_to_model,' nm ',sep=''))

      wle <- seq(min(spectral[['wlE']]),max(spectral[['wlE']]),step)
      wlf <- seq(min(spectral[['wlF']]),max(spectral[['wlF']]),step-1) # fluorescence wavelengths, transpose to column
    }

  }




  leafopt <- list(refl = matrix(nrow=mly$nly, ncol=length(spectral$wlP)),
                  tran = matrix(nrow=mly$nly, ncol=length(spectral$wlP)),
                  Mb = array(dim=c(length(wlf), length(wle), indStar[2])),
                  Mf = array(dim=c(length(wlf), length(wle), indStar[2])),
                  kChlrel = matrix(nrow=mly$nly, ncol=length(spectral$wlP)),
                  kCarrel = matrix(nrow=mly$nly, ncol=length(spectral$wlP)))



  rho_temp <- matrix(NA, nrow = nl, ncol = length(spectral$wlP))
  tau_temp <- matrix(NA, nrow = nl, ncol = length(spectral$wlP))
  kChlrel_temp <- matrix(NA, nrow = nl, ncol = length(spectral$wlP))
  kCarrel_temp <- matrix(NA, nrow = nl, ncol = length(spectral$wlP))

  for (i in 1:mly$nly) { ### before mly$nly , here nly =1
    leafbio$Cab <- mly$pCab[i]
    leafbio$EWT <- mly$pEWT[i]
    leafbio$Car <- mly$pCar[i]
    leafbio$LMA <- mly$pLMA[i]
    leafbio$Cs <- mly$pCs[i]
    leafbio$N <- mly$pN[i]

    leafopt_ml <- getFluspect.Cx.SCOPE(inputsLeaf=leafbio,inputsOptipar=optipar2021.Pro.CX,
                                                 version = 'SCOPE', step=step_to_model)


    #plot(leafopt_ml$refl,type='l',col='red')
    #plot(leafopt_ml$tran,type='l',col='navyblue')
    #plot(leafopt_ml$kChlrel,type='l',col='forestgreen')
    #plot(leafopt_ml$kCarrel,type='l',col='brown')
    #plot(leafopt_ml$Mb[,10],type='l',col='red')

    leafopt$refl[i,] <- leafopt_ml$refl
    leafopt$tran[i,] <- leafopt_ml$tran

    leafopt$kChlrel[i,] <- leafopt_ml$kChlrel
    leafopt$kCarrel[i,] <- leafopt_ml$kCarrel


    leafopt$Mb[,,i] <- leafopt_ml$Mb
    leafopt$Mf[,,i] <- leafopt_ml$Mf


    in1 <- indStar[i]
    in2 <- indStar[i+1]

    rho_temp[in1:in2,] <- matrix(rep(leafopt$refl[i,], in2-in1+1), ncol=length(spectral$wlP), byrow=TRUE)
    tau_temp[in1:in2,] <- matrix(rep(leafopt$tran[i,], in2-in1+1), ncol=length(spectral$wlP), byrow=TRUE)

    kChlrel_temp[in1:in2,] <- matrix(rep(leafopt$kChlrel[i,], in2-in1+1), ncol=length(spectral$wlP), byrow=TRUE)
    kCarrel_temp[in1:in2,] <- matrix(rep(leafopt$kCarrel[i,], in2-in1+1), ncol=length(spectral$wlP), byrow=TRUE)

    Mb_3d <- array(replicate(in2, leafopt_ml$Mb), dim = c(dim(leafopt_ml$Mb)[1], dim(leafopt_ml$Mb)[2], in2))
    Mf_3d <- array(replicate(in2, leafopt_ml$Mf), dim = c(dim(leafopt_ml$Mf)[1], dim(leafopt_ml$Mf)[2], in2))
  }



  leafopt[['refl']]=rho_temp
  leafopt[['tran']]=tau_temp
  leafopt[['kChlrel']] <- kChlrel_temp
  leafopt[['kCarrel']] <-kCarrel_temp

  leafopt[['Mb']] <-  Mb_3d
  leafopt[['Mf']] <- Mf_3d

  wlS <- spectral[['wlS']] # SCOPE wavelengths, make column vectors
  wlF <- spectral[['wlF']]

  iw_coincidents <- match(wlF, wlS)

  leafopt[['phiI']] <- optipar2021.Pro.CX$phiI[iw_coincidents]
  leafopt[['phiII']] <- optipar2021.Pro.CX$phiII[iw_coincidents]


  if (get.plots == T) {


    ########################################################################
    ### Get leaf reflectance Z
    ########################################################################

    wave.fluspect = c(spectral[['reg1']],spectral[['reg2']],spectral[['reg3']])
    rfl.fuspect = c(leafopt$refl[1,], rep(soil[['rs_thermal']],length(spectral[['IwlT']])))
    rfl.kChlrel = c(leafopt$kChlrel[1,], rep(soil[['rs_thermal']],length(spectral[['IwlT']])))
    rfl.kCarrel = c(leafopt$kCarrel[1,], rep(soil[['rs_thermal']],length(spectral[['IwlT']])))

    df.rfl <- data.frame(wave.fluspect=wave.fluspect, rfl.fuspect = rfl.fuspect,
                         kChlrel=rfl.kChlrel, kCarrel=rfl.kCarrel)


    p.z0 <- ggplot(data = df.rfl, aes(x = wave.fluspect, y = rfl.fuspect)) +
      labs(y= " leaf reflectance Z0 (fluspect-Cx)", x = "")+ xlim(400,2499) +
      geom_line()+ theme_bw()

    print(p.z0)


    ########################################################################
    ### Get Relative portion of pigments contribution
    ########################################################################

    p.k <- ggplot(data = df.rfl, aes(x = wave.fluspect)) +
      labs(y= "Relative portion of pigments contribution", x = "") +
      geom_line(aes(y = kChlrel, color = "chlorophylls"), linewidth = 0.5) +
      geom_line(aes(y = kCarrel, color = "carotenoids"), linewidth = 0.5) +
      scale_color_manual(name = "Irradiance type",
                         values = c("chlorophylls" = "forestgreen", "carotenoids" = "brown")) +
      theme_bw() + xlim(400,800) +
      theme(legend.position = "top") +
      guides(color = guide_legend(title = NULL))
    print(p.k)

    ########################################################################
    ### Get PhiI and PhiII
    ########################################################################


    df.phi<- data.frame(wave=spectral$wlF, phiI = leafopt$phiI,
                        phiII=leafopt$phiII, phiSum=leafopt$phiII+leafopt$phiI)

    p.phi <- ggplot(data = df.phi, aes(x = wave)) +
      labs(y= "phi contribution", x = "") +
      geom_line(aes(y = phiI, color = "phiI"), linewidth = 0.5) +
      geom_line(aes(y = phiII, color = "phiII"), linewidth = 0.5) +
      geom_line(aes(y = phiSum, color = "phiSum"), linewidth = 0.5) +
      scale_color_manual(name = "phi",
                         values = c("phiI" = "forestgreen", "phiII" = "brown",
                                    'phiSum' = 'black')) +
      theme_bw()  +
      theme(legend.position = "top") +
      guides(color = guide_legend(title = NULL))
    print(p.phi)

    ########################################################################
    ### Get MB and Mf
    ########################################################################

    wlf <- seq(640, 850, by = step-1) #
    df.Mb_Mf <- data.frame(wave.f=wlf, Mb = leafopt$Mb[,1,1],
                           Mf=leafopt$Mf[,1,1])

    p.mb <- ggplot(data = df.Mb_Mf, aes(x = wave.f)) +
      labs(y= "Relative portion of pigments contribution", x = "") +
      geom_line(aes(y = Mb, color = "chlorophylls"), linewidth = 0.5) +
      geom_line(aes(y = Mf, color = "carotenoids"), linewidth = 0.5) +
      scale_color_manual(name = "Irradiance type",
                         values = c("chlorophylls" = "forestgreen", "carotenoids" = "brown")) +
      theme_bw()  +
      theme(legend.position = "top") +
      guides(color = guide_legend(title = NULL))
    print(p.mb)

  }


  return(leafopt)

}
