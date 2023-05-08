#' Corretalion function
#' This function generates a new variable that will be correlated with  x variable. The new variable is fixed by a normal distribution function 
#' @param x numeric. Main variable in a vector format 
#' @param r value of correlation coefficient 
#'
#' @return second variable based on the asigned correlation of the main variable in a vector format 
#' @export
#' @examples here adding examples ....
#' 
#' 
correlatedValue = function(x, r){
  r2 = r**2
  ve = 1-r2
  SD = sqrt(ve)
  e  = rnorm(length(x), mean=0, sd=SD)
  y  = r*x + e
  ##check if some values is negative
  for (i in c(1:length(y))){
    if (y[i] <=0) {
      y[i]=0
    }
  }
  
  return(y)
}

######### Another option could be based on the relationship between Cab-Car
# constants from ANGERS03 Leaf Optical Data
#slope = 0.2234
#intercept = 0.9861
#spread = 4.6839
#grid_var=data.LUT$Cab
# car_lin = slope * grid_var+ intercept
# lower_car = slope / spread * 3 * grid_var
# upper_car = slope * spread / 3 * grid_var + 2 * intercept
# 
# car_noise=rnorm(length(grid_var), mean=3, sd=0.5)+ car_lin
