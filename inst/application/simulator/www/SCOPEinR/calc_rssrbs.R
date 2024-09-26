#' Founction for calculating the rss and rbs values based on the input parameters (SMC, LAI and rbs)
#'
#' @param SMC
#' @param LAI
#' @param rbs
#'
#' @return
#' @export
#'
#' @examples
calc_rssrbs<-function(SMC,LAI,rbs){

rss_rbs<-list()
rss = 11.2 * exp(42 * (0.22 - SMC))
rss_rbs$rss <- rss

rbs = rbs * LAI / 3.3
rss_rbs$rbs <-rbs

return(rss_rbs)

}

