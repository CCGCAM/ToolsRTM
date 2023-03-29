

#' get random plant traits using a uniform distribution
#'
#' @param RTmodel RT model: options are: 'PROSPECT-PRO';'PROSPECT-D'; 'INFORM'; and 'fourSAILH'
#' @param nsamples nummber of combinations
#' @param seed  the seed number for the  uniform distribution
#'
#' @return a LUT 
#' @export
#'
#' @examples
#' 
#' 
get_data <- function(RTmodel='PROSPECT-D', nLUT=100, random.input =100) {
  
  if (is.null(nLUT)){
    message('number of varitions for each input is fixed to 100')
    nLUT = 100
  }
  
  if (is.null(random.input)){
    message('number of varitions for each input is fixed to 100')
    random.input = 1234
    set.seed(random.input)
  } else {
    set.seed(random.input)
  }

  
  if (RTmodel == "PROSPECT-PRO") {
    # Create lookup table for prospect model
    lut <- data.frame(
      N = runif(nsamples, min = 1, max = 3),
      Cab = runif(nsamples, min = 0, max = 100),
      Car = runif(nsamples, min = 0, max = 40),
      Anth = runif(nsamples, min = 0, max = 7.5),
      Cbrown = runif(nsamples, min = 0, max = 1),
      EWT = runif(nsamples, min = 0.001, max = 0.04),
      LMA = runif(nsamples, min = 0.001, max = 0.04),
      alpha = runif(nsamples, min = 10, max = 50),
      Prot = runif(nsamples, min = 0.001, max = 0.02),
      CBC = runif(nsamples, min = 0.001, max = 0.02),
    )
  } else if (RTmodel == "PROSPECT-D") {
      # Create lookup table for prospect model
      lut <- data.frame(
        N = runif(nsamples, min = 1, max = 3),
        Cab = runif(nsamples, min = 0, max = 100),
        Car = runif(nsamples, min = 0, max = 40),
        Anth = runif(nsamples, min = 0, max = 7.5),
        Cbrown = runif(nsamples, min = 0, max = 1),
        EWT = runif(nsamples, min = 0.001, max = 0.04),
        LMA = runif(nsamples, min = 0.001, max = 0.04),
        alpha = runif(nsamples, min = 10, max = 50)
      )

    } else if (RTmodel == "Liberty") {
      # Create lookup table for liberty model
      lut <- data.frame(
        cell.d = runif(nsamples, min = 20, max = 200),
        inter.c = runif(nsamples, min = 0.01, max = 0.1),
        baseline.abs = runif(nsamples, min = 0.0004, max = 0.0006),
        leaf.thick = runif(nsamples, min = 1, max = 10),
        albino.abs = runif(nsamples, min = 0, max = 4),
        Cab = runif(nsamples, min = 0, max = 100),
        EWT = runif(nsamples, min = 0.001, max = 0.04),
        lign.cell = runif(nsamples, min = 10, max = 80),
        Nitrogen = runif(nsamples, min = 0.3, max = 2)
 
      )

  } else if (RTmodel == "fourSAILH") {
    # Create lookup table for fourSAILH model
    lut <- data.frame(
      
      LAI  = runif(nsamples, min = 0, max = 9),
      TypeLidf = 2,
      LIDFa = runif(nsamples, min = 0, max = 90),
      LIDFa = 0,
      hspot  = runif(nsamples, min = 0, max = 1),
      tts = runif(nsamples, min = 0, max = 30),
      tto = runif(nsamples, min = 0, max = 55),
      psi = runif(nsamples, min = 0, max = 180),
      psoil = runif(nsamples, min = 0, max = 1)
     
    )
  } else if (RTmodel == "INFORM") {
    # Create lookup table for INFORM model
    lut <- data.frame(
      LAI  = runif(nsamples, min = 0, max = 9),
      TypeLidf = 2,
      LIDFa = runif(nsamples, min = 0, max = 90),
      LIDFa = 0,
      hspot  = runif(nsamples, min = 0, max = 1),
      tts = runif(nsamples, min = 0, max = 30),
      tto = runif(nsamples, min = 0, max = 55),
      psi = runif(nsamples, min = 0, max = 180),
      psoil = runif(nsamples, min = 0, max = 1),
      LAIu = runif(nsamples, min = 0.0, max = 2),
      cd = runif(nsamples, min = 0, max = 10),
      sd = runif(nsamples, min = 0, max = 1200),
      h = runif(nsamples, min = 1, max = 40),
      skyl = runif(nsamples, min = , max = )
    )
 
  } else {
    # Throw an error if an invalid model is specified
    stop("Invalid model specified.")
  }
 
  return(lut)
}
