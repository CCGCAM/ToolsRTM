#' Simpson integration
#'
#' @param y must be any vectors (rows, columns),
#' @param x must be any vectors (rows, columns),x  but of the same length and
#' x must be a monotonically increasing series
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @references WV Jan. 2013, for SCOPE 1.40
#'
#' @examples
#'
#'
Sint <- function(y, x) {

  if (all(diff(x) > 0)) {
   # print("x is monotonically increasing")
  } else {
  #  print("x is not monotonically increasing")
  }

  nx <- length(x)

  if (length(x) == 1) {
    x <- t(x)
  }

  if (length(y) == length(x)) {
    y <- t(y)
  }

  step <- x[2:nx] - x[1:nx - 1]

  mean <- 0.5 * (y[1:nx - 1] + y[2:nx])

  ## in matlab retunr as unique value
  int <- sum(mean * step)

  return(int)
}

