#' count_k
#'
#' @param nvars
#' @param v
#' @param vmax
#' @param id
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
count_k <- function(nvars, v, vmax, id) {
  i <- id

  # starting at id, set digits which are at its maximum equal to 1
  # first digit that is not at its maximum is incremented
  while (v[i] == vmax[i]) {
    v[i] <- 1
    i <- (i %% nvars) + 1
  }

  v[i] <- (v[i] %% vmax[i]) + 1
  vnew <- v

  return(vnew)
}
