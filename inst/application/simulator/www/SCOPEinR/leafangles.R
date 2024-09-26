






#'Subroutine FluorSail_dladgen (Version 2.3)
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @author 	Joris Timmermans, Christiaan van der Tol  (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @references # For more information look to page 128 of "theory of radiative transfer models applied in optical remote sensing of
# vegetation canopies"
#
#' @examples
#'
leafangles<-function(a,b){

  # Subroutine FluorSail_dladgen taken from PROSAIL
  # FluorSail for Matlab
  # FluorSail is created by Wout Verhoef,

  litab <- c(5,15,25,35,45,55,65,75,81,83,85,87,89)
  freq=c() ##added
  for (i1 in c(1:8)){
    t   <- i1 * 10
    freq[i1] <- dcum(a,b,t)
  }

  for (i2 in c(9:12)){
    t   <- 80 + (i2 - 8) * 2
    freq[i2] <- dcum(a,b,t)
  }

  freq[13] <- 1
  for (i   in seq(13,2,-1)){
    freq[i] <- freq[i] - freq[i-1]
  }
LeafDistribution <- list("lidf" = freq,"litab" =litab) ##added
return(LeafDistribution) ##added
}



#' dcum function
#' @param a numeric. controls the average leaf slope
#' @param b numeric. controls the distribution's bimodality
#' @param t numeric. angle
#' @return f
#' @export
#'
#' @author 	Joris Timmermans, Christiaan van der Tol  (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
# edit 2017 12 28: change sampling of angles to match with dcum.m
#'
#'
dcum <- function(a,b,theta) {

  ### this function is the same as dcum
  ## SubRoutines
  rd <- pi/180
  if (a >= 1){
    f <- 1 - cos(rd * theta)
  }
  else {
    eps <- 1e-8
    delx <- 1
    x <- 2 * rd * theta
    p <- x
    while (delx >=  eps){
      y <- a * sin(x) + 0.5 * b * sin(2.0 * x)
      dx <- 0.5*(y - x + p)
      x <- x + dx
      delx <- abs(dx)
    }
    f <- (2.0 * y + p) / pi
  }
  return(f) ##added
}


