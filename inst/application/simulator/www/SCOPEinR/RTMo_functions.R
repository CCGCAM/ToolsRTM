
#' get.volscatt.scope version 2.0 from SCOPE model
#'
#' \code{volscatt.scope} calculates the scattering phase functions using a
#' radiative transfer model based on the optical properties of leaves. Specifically,
#' the model computes the fraction of radiation scattered in the forward and backward directions
#' for a given set of input parameters
#'
#' @param tts  Sun: zenith angle in degrees
#' @param tto  observation:zenith angle in degrees
#' @param psi   Difference of  azimuth angle between solar and viewing position
#' @param ttli leaf inclination array
#'
#' @return The function returns a list of four items: "chi_s", "chi_o", "frho", and "ftau".
#' These items represent the scattering phase functions for direct and diffuse radiation, respectively.

#' @export
#' Date:
#'  - 11 February 2008
#' @author 	 Wout Verhoef, Joris Timmermans (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
get.volscatt.scope <- function(tts, tto, psi, ttli) {

  deg2rad <- pi/180
  nli     <- length(ttli)

  psi_rad         <- psi * deg2rad * rep(1, nli)

  cos_psi         <- cos(psi * deg2rad)                 # cosine of relative azimuth angle

  cos_ttli        <- cos(ttli * deg2rad)                # cosine of normal of upperside of leaf
  sin_ttli        <- sin(ttli * deg2rad)                # sine   of normal of upperside of leaf

  cos_tts         <- cos(tts * deg2rad)                 # cosine of sun zenith angle
  sin_tts         <- sin(tts * deg2rad)                 # sine   of sun zenith angle

  cos_tto         <- cos(tto * deg2rad)                 # cosine of observer zenith angle
  sin_tto         <- sin(tto * deg2rad)                 # sine   of observer zenith angle

  Cs              <- cos_ttli * cos_tts                 # p305{1}
  Ss              <- sin_ttli * sin_tts                 # p305{1}

  Co              <- cos_ttli * cos_tto                 # p305{1}
  So              <- sin_ttli * sin_tto                 # p305{1}

  As              <- pmax(Ss, Cs)
  Ao              <- pmax(So, Co)

  bts             <- acos(-Cs/As)                       # p305{1}
  bto             <- acos(-Co/Ao)                       # p305{2}

  chi_o           <- 2/pi * ((bto-pi/2) * Co + sin(bto) * So)
  chi_s           <- 2/pi * ((bts-pi/2) * Cs + sin(bts) * Ss)

  delta1          <- abs(bts-bto)                       # p308{1}
  delta2          <- pi - abs(bts + bto - pi)           # p308{1}

  Tot             <- psi_rad + delta1 + delta2           # pag 130{1}

  bt1             <- pmin(psi_rad, delta1)
  bt3             <- pmax(psi_rad, delta2)
  bt2             <- Tot - bt1 - bt3

  T1              <- 2*Cs*Co + Ss*So*cos_psi
  T2              <- sin(bt2) * (2*As*Ao + Ss*So*cos(bt1)*cos(bt3))

  Jmin            <- (bt2) * T1 - T2
  Jplus           <- (pi - bt2) * T1 + T2

  frho            <-  Jplus / (2 * pi^2)
  ftau            <- -Jmin / (2 * pi^2)

  # pag.309 wl-> pag 135{1}
  frho            <- pmax(0, frho)
  ftau            <- pmax(0, ftau)

  return(list(chi_s = chi_s, chi_o = chi_o, frho = frho, ftau = ftau))
}



#' get.Pso function
#'
#' \code{get.Pso} calculates the proportion of incoming PAR that is intercepted by the canopy at the reference plane
#'
#' @param K numeric value representing a coefficient related to the diffuse attenuation of photosynthetically active radiation (PAR) in the canopy.
#' @param k  a numeric value representing a coefficient related to the extinction of PAR in the canopy
#' @param LAI a numeric value representing the leaf area index (LAI) of the canopy.
#' @param q a numeric value representing the ratio of diffuse PAR to total PAR outside the canopy
#' @param dso a numeric value representing the distance from the top of the canopy to the reference plane.
#' @param xl a numeric value representing the length of the canopy segment over which the calculation is performed.
#'
#' @return
#' @export
#'
#' @author 	 Wout Verhoef, Christiaan van der Tol, Joris Timmermans (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
get.Pso <- function(K, k, LAI, q, dso, xl) {
  if (dso != 0) {
    alf <- (dso/q) * 2/(k + K)
    pso <- exp((K + k) * LAI * xl + sqrt(K * k) * LAI/alf * (1 - exp(xl * alf)))
  } else {
    pso <- exp((K + k) * LAI * xl - sqrt(K * k) * LAI * xl)
  }
  return(pso)
}

#' get.reflectances
#'
#' \code{get.reflectances} Calculate reflectance
#' @param tau_ss
#' @param tau_sd
#' @param tau_dd
#' @param rho_dd
#' @param rho_sd
#' @param rsoil
#' @param nl
#' @param nwl
#'
#' @return
#' @export
#'
#' @author 	 Wout Verhoef, Christiaan van der Tol, Joris Timmermans (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
get.reflectances <- function(tau_ss, tau_sd, tau_dd, rho_dd, rho_sd, rsoil, nl, nwl=2162) {


  if (nwl != 2162){
    ### check the wavelength for thermal region (>2500)
    message('please verify the length of the wavelengths (400:2500)')
    stop()


  }

    R_sd <- matrix(0, nl + 1, nwl)
    R_dd <- matrix(0, nl + 1, nwl)
    Xsd <- matrix(0, nl, nwl)
    Xdd <- matrix(0, nl, nwl)
    Xss <- rep(0, nl)
    # Initialize Xss as a matrix of zeros
    #Xss <- matrix(0, nrow = nl, ncol = length(k))

    R_sd[nl + 1, ] <- rsoil
    R_dd[nl + 1, ] <- rsoil


  #### Get RFL and Trans
  for (j in nl:1) {
    Xss[j] <- tau_ss[j]
    dnorm <- 1 - rho_dd[j, ] * R_dd[j + 1, ]
    Xsd[j, ] <- (tau_sd[j, ] + tau_ss[j] * R_sd[j + 1, ] * rho_dd[j, ]) / dnorm
    Xdd[j, ] <- tau_dd[j, ] / dnorm
    R_sd[j, ] <- rho_sd[j, ] + tau_dd[j, ] * (R_sd[j + 1, ] * Xss[j] + R_dd[j + 1, ] * Xsd[j, ])
    R_dd[j, ] <- rho_dd[j, ] + tau_dd[j, ] * R_dd[j + 1, ] * Xdd[j, ]
  }



  return(list(R_sd = R_sd, R_dd = R_dd, Xss = Xss, Xsd = Xsd, Xdd = Xdd))
}

#' get.fluxprofile
#' \code{get.fluxprofile} calculate the flux profile
#'
#' @param Esun_
#' @param Esky_
#' @param rs
#' @param Xss
#' @param Xsd
#' @param Xdd
#' @param R_sd
#' @param R_dd
#' @param nl
#' @param nwl
#' @param rs.thermal get rs_thermal value, by default use 0.06
#' @return
#' Es_ represents the direct solar irradiance at each layer of the atmosphere.
#' Emin_ represents the downward diffuse irradiance at each layer of the atmosphere.
#' It is calculated based on the upward diffuse irradiance at the previous layer Es_(j,:)
#' and the downward diffuse irradiance at the previous layer Emin_(j,:).
#' Eplu_ represents the upward diffuse irradiance at each layer of the atmosphere.
#' It is calculated based on the direct solar irradiance at the current layer Es_(j,:),
#' the downward diffuse irradiance at the current layer Emin_(j,:), and the reflectivity
#' of the surface rs.
#'
#' @export
#'
#' @author 	 Wout Verhoef, Christiaan van der Tol, Joris Timmermans (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
get.fluxprofile <- function(Esun_, Esky_, rsoil, Xss, Xsd, Xdd, R_sd, R_dd, nl, nwl, rs.thermal=0.06) {

  if (missing(rs.thermal)){
    rs_thermal = 0.01
  } else{
    rs_thermal = rs.thermal
  }

  #nl <- nl + 1 # increment nl to match indexing
  Es_ <- matrix(rs_thermal, nrow = nl+1, ncol = nwl)
  Emin_ <- matrix(rs_thermal, nrow = nl+1, ncol = nwl)
  Eplu_ <- matrix(rs_thermal, nrow = nl+1, ncol = nwl)

  ### adding value of
  if (dim(Xsd)[2] == 2001){


    rs_vec <- 2162 - 2001
    r_thermal <-matrix(rs_thermal, ncol = rs_vec, nrow= nrow(R_dd))
    r_thermal.X <-matrix(rs_thermal, ncol = rs_vec, nrow= nrow(Xsd))

    ### convert rfl to a matrix with columns ==nwl (all spectrla domain addinfg thermal)
    # repeat rs_thermal to match the number of columns to add
    Xsd <- cbind(Xsd, r_thermal.X)
    Xdd <- cbind(Xdd, r_thermal.X)
    R_dd <- cbind(R_dd, r_thermal)
    R_sd <- cbind(R_sd, r_thermal)
    rsoil = c(rsoil, rep(rs_thermal,rs_vec))
  }


  Es_[1,] <- Esun_
  Emin_[1,] <- Esky_

  for (j in c(1:nl)) {

    Es_[j+1,] <- Xss * Es_[j,] #matrix(Xss,ncol=1)[j] %*% (Es_[j,]) --> before
    #Emin_[j+1,] <- Xsd[j,] * Es_[j,] + matrix(Xdd,ncol=1)[j] %*% Emin_[j,] --> before
    Emin_[j+1,] <- Xsd[j,] * Es_[j,] + Xdd[j,] * Emin_[j,]
    Eplu_[j,] <- R_sd[j,] * Es_[j,] + R_dd[j,] * Emin_[j,]

  }
  #Eplu_[nl,] <- rsoil * (Es_[nl,] + Emin_[nl,])
  Eplu_[nl + 1, ] <- rsoil * (Es_[nl + 1, ] + Emin_[nl + 1, ])
  # CvdT added calculation of Eplu_[nl,]

  return(list(Es_ = Es_, Emin_ = Emin_, Eplu_ = Eplu_))
}


#' get.calcTOCirr
#'
#' \code{get.calcTOCirr} Calculation of incoming light Esun_ and Esky_
#'  Extract MODTRAN atmosphere parameters at the SCOPE wavelengths
#'
#' @param atmo
#' @param meteo
#' @param rdd
#' @param rsd
#' @param wl
#' @param nwl
#'
#' @return
#' @export
#'
#'
#' @author 	 Wout Verhoef, Christiaan van der Tol, Joris Timmermans (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
get.calcTOCirr <- function(atmo, meteo, rdd, rsd, wl, nwl){

  Fd      <- rep(0, nwl)
  Ls      <- get.Planck(wl, meteo[['Ta']] + 273.15)

  ##### This function need to be revised with a MODtrans file


  if(!"Esun_" %in% names(atmo)){

    wln_ <-c(spectral$reg1,spectral$reg2,spectral$reg3)
    atmo.selected <- atmo[atmo$WN %in% wl, c('WN','T1', 'T2','T3','T4','T5','T12','T16')]

    t1  <- atmo.selected[, 'T1']
    t3  <- atmo.selected[, 'T3']
    t4  <- atmo.selected[, 'T4']
    t5  <- atmo.selected[, 'T5']
    t12 <- atmo.selected[, 'T12']
    t16 <- atmo.selected[, 'T16']

    # radiation fluxes, downward and upward (these all have dimenstion [nwl]
    # first calculate hemispherical reflectances rsd and rdd according to SAIL
    # these are assumed for the reflectance of the surroundings
    # rdo is computed with SAIL as well
    # assume Fd of surroundings = 0 for the momemnt
    # initial guess of temperature of surroundings from Ta;

    Esun_   <- pmax(1e-6, pi*t1*t4)

    rdd_wl <- c(c(rdd), rep(soil[['rs_thermal']],length(spectral[['IwlT']])))
    rsd_wl <- c(c(rsd), rep(soil[['rs_thermal']],length(spectral[['IwlT']])))
    Esky_   <- pmax(1e-6, pi/(1-t3*rdd_wl)*(t1*(t5+t12*rsd_wl)+Fd+(1-rdd_wl)*Ls*t3+t16))

    # fractional contributions of Esun and Esky to total incident radiation in
    # optical and thermal parts of the spectrum
    if(meteo[['Rin']] != -999){
      # fractional contributions of Esun and Esky to total incident radiation in
      # optical and thermal parts of the spectrum

      fEsuno  <- rep(0, nwl)
      fEskyo  <- rep(0, nwl)
      fEsunt  <- rep(0, nwl)
      fEskyt  <- rep(0, nwl)

      J_o             <- which(wl<3000)             #find optical spectrum
      Esunto          <- 0.001 * Sint(Esun_[J_o], wl[J_o])  #Calculate optical sun fluxes (by Integration), including conversion mW >> W
      Eskyto          <- 0.001 * Sint(Esky_[J_o], wl[J_o])  #Calculate optical sun fluxes (by Integration)
      Etoto           <- Esunto + Eskyto             #Calculate total fluxes
      fEsuno[J_o]     <- Esun_[J_o]/Etoto            #fraction of contribution of Sun fluxes to total light
      fEskyo[J_o]     <- Esky_[J_o]/Etoto            #fraction of contribution of Sky fluxes to total light

      J_t             <- which(wl>=3000)            #find thermal spectrum
      Esuntt          <- 0.001 * Sint(Esun_[J_t], wl[J_t])  #Themal solar fluxes
      Eskytt          <- 0.001 * Sint(Esky_[J_t], wl[J_t])  #Thermal Sky fluxes
      Etott           <- Eskytt + Esuntt             #Total
      fEsunt[J_t]     <- Esun_[J_t]/Etott            #fraction from Esun
      fEskyt[J_t]     <- Esky[J_t]/Etott  #fraction from Esky


      Esun_[J_o] = fEsuno[J_o] * meteo[['Rin']]
      Esky_[J_o] = fEskyo[J_o] * meteo[['Rin']]
      Esun_[J_t] = fEsunt[J_t] * meteo[['Rli']]
      Esky_[J_t] = fEskyt[J_t] * meteo[['Rli']]

    }
  } else {
    Esun_ = atmo[['Esun_']]
    Esky_ = atmo[['Esky_']]
  }

  return(list(Esun_ = Esun_, Esky_ = Esky_))
}


