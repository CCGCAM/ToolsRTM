

#' resample function
#'
#' @param center 
#' @param wl 
#' @param fwhm 
#'
#' @return resample
#' @export
#'
#' @examples here an example
#' 
resample_fun<-function(center, wl, fwhm)
{
  a <- dnorm(wl, mean = center, sd = fwhm/2)
  a <- (a-min(a))/(max(a) - min(a))
  return(a)
}
