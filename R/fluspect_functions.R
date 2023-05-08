
#' Define constant for FLuxPesp
#'
#' @return constants
#' @export
#'
define_constants <- function() {
  const <- list(
    A = 6.02214E23, # [mol-1]       Constant of Avogadro
    h = 6.6262E-34, # [J s]         Planck's constant
    c = 299792458,  # [m s-1]       Speed of light
    cp = 1004,      # [J kg-1 K-1]  Specific heat of dry air
    R = 8.314,      # [J mol-1K-1]  Molar gas constant
    rhoa = 1.2047,  # [kg m-3]      Specific mass of air
    g = 9.81,       # [m s-2]       Gravity acceleration
    kappa = 0.4,    # []            Von Karman constant
    MH2O = 18,      # [g mol-1]     Molecular mass of water
    Mair = 28.96,   # [g mol-1]     Molecular mass of dry air
    MCO2 = 44,      # [g mol-1]     Molecular mass of carbon dioxide
    sigmaSB = 5.67E-8, # [W m-2 K-4] Stefan Boltzman constant
    deg2rad = pi/180,  # [rad]       Conversion from deg to rad
    C2K = 273.15       # [K]         Melting point of water
  )
  return(const)
}


#' define spectral region
#'
#' @return spectral regions
#' @export
#'
#' @examples here adding examples ....
define_bands <- function() {
  spectral <- list()
  
  # Define spectral regions for SCOPE v_1.40
  # All spectral regions are defined here as row vectors
  # WV Jan. 2013
  
  # 3 spectral regions for SCOPE
  
  reg1 <-  seq(400,2400,1)
  reg2 <- seq(2500,15000,100)
  reg3 <- seq(16000,50000,1000)
  
  spectral$wlS  <- c(reg1, reg2, reg3)
  
  # Other spectral (sub)regions
  spectral$wlP   <- reg1 # PROSPECT data range
  spectral$wlE   <- as.integer(seq(400, 750, 1))
  spectral$wlF   <- as.integer(seq(640, 850, 1)) # chlorophyll fluorescence in E-F matrix
  spectral$wlO   <- reg1 # optical part
  spectral$wlT   <- c(reg2, reg3) # thermal part
  
  # Other spectral (sub)regions
  
  spectral$wlP   <- reg1                            # PROSPECT data range
  spectral$wlE   <- seq(400,750,1)                       # excitation in E-F matrix
  spectral$wlF   <- seq(640,850,1)                       # chlorophyll fluorescence in E-F matrix
  spectral$wlO   <- reg1                            # optical part
  spectral$wlT   <- c(reg2, reg3)                   # thermal part
  
  wlS            <- spectral$wlS
  spectral$wlPAR <- wlS[wlS >= 400 & wlS <= 700]     # PAR range
  
  # Data used by aggreg routine to read in MODTRAN data
  
  spectral$SCOPEspec$nreg  <- 3
  spectral$SCOPEspec$start <- c(400, 2500, 16000)
  spectral$SCOPEspec$end   <- c(2400, 15000, 50000)
  spectral$SCOPEspec$res   <- c(1, 100, 1000)
  
  return(spectral)
}



#' @title num-jacobian function
#'
#' @param x 
#' @param spectral 
#' @param inputLeaf 
#' @param optipar 
#'
#' @author Wout Verhoef, Christiaan van der Tol, Joris Timmermans, 
#' @author Ported to R: Carlos. Camino
#' 
#' @return numjacobian  values
#' @export
#' @examples here adding examples ....
numjacobian <- function(x, spectral, inputLeaf, optipar) {
  n <- length(x)
  res <- ToolsRTM::calc_fluspect_bcar(x, spectral, leafbio, optipar)
  fx <- c(res$refl, res$tran)
  step <- 1e-6
  J <- array(0, dim = c(length(fx), 2, n))
  
  for (i in 1:n) {
    xstep <- x
    xstep[i] <- x[i] + step
    res_step <- ToolsRTM::calc_fluspect_bcar(xstep, spectral, inputLeaf, optipar)
    fxstep <- c(res_step$refl, res_step$tran)
    J[, , i] <- (fxstep - fx) / step
  }
  
  return(J)
}

#' @title calc_fluspect_bcar function
#'
#' @param params 
#' @param spectral 
#' @param leafbio 
#' @param optipar 
#'
#' @return rfl and trans
#' @export
#'
#' @examples here adding examples ....
#' 
calc_fluspect_bcar <- function(params, spectral, leafbio, optipar) {
  leafbio$Cab <- params[1]
  leafbio$Cdm <- params[2]
  leafbio$Cw <- params[3]
  leafbio$Cs <- params[4]
  leafbio$Cca <- params[5]
  leafbio$N <- params[6]
  
  leafopt <- ToolsRTM::fluspect_B_CX_PSI_PSII_combined(spectral, leafbio, optipar)
  
  return(list(refl = leafopt$refl, tran = leafopt$tran))
}


#' Fucntion for calculating the energy of a single photon given its wavelength.
#'
#' @param lambda 
#'
#' @return e
#' @export
#'
#' @examples here adding examples ....
#' 
ephoton <- function(lambda) {
  h <- 6.62607004E-34 # Planck constant [J.s]
  c <- 2.99792458E8 # Speed of light [m/s]
  e <- h * c / lambda
  return(e)
}

#' @title  Simpson integration
#'
#' @param y  must be any vector (rows, columns), but of the same length
#' @param x  must be any vector (rows, columns), but of the same length; must be a monotonically increasing series
#' develope by WV Jan. 2013, for SCOPE 1.40
#' @return int
#' @export
#'
#' @examples here adding examples ....
#' 
Sint <- function(y, x) {

  nx <- length(x)
  if (length(dim(x)) == 1) {
    x <- t(x)
  }
  if (length(dim(y)) != 1) {
    y <- t(y)
  }
  step <- x[2:nx] - x[1:(nx-1)]
  mean <- 0.5 * (y[1:(nx-1)] + y[2:nx])
  int <- mean * step
  return(int)
}



#' @title Cost fucntions
#'
#' @param params 
#' @param measurement 
#' @param input 
#'
#' @return rfl and tran resampled
#' @export
#'
#' @examples Here adding examples
#' 
COST_4Fluspect <- function(params, measurement, input) {
  
  leafbio <- input[[1]]
  optipar <- input[[2]]
  spectral <- input[[3]]
  include <- input[[4]]
  target <- input[[5]]
  range <- input[[6]]
  
  leafbio$Cab <- params[1] * include$Cab + (1-include$Cab) * leafbio$Cab
  leafbio$Cdm <- params[2] * include$Cdm + (1-include$Cdm) * leafbio$Cdm
  leafbio$Cw <- params[3] * include$Cw + (1-include$Cw) * leafbio$Cw
  leafbio$Cs <- params[4] * include$Cs + (1-include$Cs) * leafbio$Cs
  leafbio$Cca <- params[5] * include$Cca + (1-include$Cca) * leafbio$Cca  # default is 25% of Cab
  leafbio$Cant <- params[6] * include$Cant + (1-include$Cant) * leafbio$Cant
  leafbio$Cx <- params[7] * include$Cx + (1-include$Cx) * leafbio$Cx
  leafbio$N <- params[8] * include$N + (1-include$N) * leafbio$N
  
  # leafopt <- fluspect_bcar(spectral, leafbio, optipar)
  leafopt <- fluspect_B_CX_PSI_PSII_combined(spectral, leafbio, optipar)
  # leafopt <- fluspect_b(spectral, leafbio, optipar)
  
  refl <- approx(spectral$wlP, leafopt$refl, xout = spectral$wlM, method = "spline", rule = 2)$y
  tran <- approx(spectral$wlP, leafopt$tran, xout = spectral$wlM, method = "spline", rule = 2)$y
  
  i <- which(spectral$wlM >= range$wlmin & spectral$wlM <= range$wlmax & !is.na(measurement$refl * measurement$tran * refl * tran))
  
  switch(target,
         "0" = {
           er1 <- (refl[i] - measurement$refl[i]) / measurement$std[i]
           er2 <- (tran[i] - measurement$tran[i]) / measurement$std[i]
           er <- cbind(er1, er2)
         },
         "1" = {
           er <- (refl[i] - measurement$refl[i]) / measurement$std[i]
         },
         {
           er <- (tran[i] - measurement$tran[i]) / measurement$std[i]
         }
  )
  
  return(list(er = er, refl = refl, tran = tran, leafopt = leafopt))
}

