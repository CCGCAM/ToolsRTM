#' aggregates MODTRAN data over SCOPE bands by averaging
#'
#' @param atmfile
#' @param SCOPEspec
#'
#' @return
#' @export
#'
#' @examples
aggreg <- function(atmfile, SCOPEspec) {

  # Read .atm file with MODTRAN data
  s <- read.table(atmfile, header = TRUE)
  wlM <- s[, 2]
  T <- s[, 3:20]

  # Extract 6 relevant columns from T
  # 1: <Eso*costts/pi>
  # 3: <rdd>
  # 4: <tss>
  # 5: <tsd>
  # 12: <tssrdd>
  # 16: <La(b)>
  U <- T[, c(1, 3, 4, 5, 12, 16)]

  nwM <- length(wlM)

  nreg <- SCOPEspec$nreg
  streg <- SCOPEspec$start
  enreg <- SCOPEspec$end
  width <- SCOPEspec$res

  # Nr. of bands in each region
  nwreg <- as.integer((enreg - streg) / width) + 1

  off <- integer(nreg)
  for (i in 2:nreg) {
    off[i] <- off[i - 1] + nwreg[i - 1]
  }

  nwS <- sum(nwreg)
  n <- integer(nwS)   # Count of MODTRAN data contributing to a band
  S <- matrix(0, nrow = nwS, ncol = 6)  # Initialize sums

  j <- integer(nreg)  # Band index within regions
  for (iwl in 1:nwM) {
    w <- wlM[iwl]    # MODTRAN wavelength
    for (r in 1:nreg) {
      j[r] <- as.integer(round((w - streg[r]) / width[r])) + 1
      if (j[r] > 0 && j[r] <= nwreg[r]) {     # test if index is in valid range
        k <- j[r] + off[r]                   # SCOPE band index
        S[k, ] <- S[k, ] + U[iwl, ]          # Accumulate from contributing MODTRAN data
        n[k] <- n[k] + 1                     # Increment count
      }
    }
  }

  M <- matrix(0, nrow = nwS, ncol = 6)
  for (i in 1:6) {
    M[, i] <- S[, i] / n                 # Calculate averages per SCOPE band
  }

  return(M)
}
