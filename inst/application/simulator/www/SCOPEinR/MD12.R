 ##
#' MD12 algorithm for the computation of fluorescence yield
#'
#' @param ps
#' @param Ja
#' @param Jms potential e-transport rate reduced for PSII photodamage
#' @param kps
#' @param kf rate constant for fluorescence
#' @param kds
#' @param kDs
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
#'
MD12<- function(ps,Ja,Jms,kps,kf,kds,kDs){

fs1    = ps * (kf / kps) / (1 - Ja / Jms)         # [E/E]   PSII fluorescence yield under CO2-limited conditions

par1   = kps / (kps - kds)                            # [E/E]   empirical parameter in the relationship under light-limited conditions
par2   = par1 * (kf + kDs + kds)/kf                  # [E/E]   empirical parameter in the relationship under light-limited conditions
fs2    = (par1 - ps) / par2                           # [E/E]   PSII fluorescence yield under light-limited conditions

fs     = min(fs1,fs2)                              # [E/E]   PSII fluorescence yield
return(fs)
}
