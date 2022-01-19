# ============================================================================= =
# ToolsRTM
# get_distributionLUT.R
# ============================================================================= =
# Author:
# Carlos Camino
# Copyright 2022/Carlos Camino
# ============================================================================= =
# This Library includes functions dedicated to generating LUTs for PROSAIL model
# ============================================================================= =

#' This function generates distribution of biophysical parameters used as input parameters in PRO4SAIL
#'
#' @param minval list. Defines the minimum value to be set for a list of parameters randomly produced
#' @param maxval list. Defines the maximum value to be set for a list of parameters randomly produced
#' @param nbSamples numeric. Number of samples to be generated
#' @param TypeDistrib list. specify if uniform or Gaussian distribution to be applied. default = Uniform
#' @param Mean_gauss list. mean value for parameters with Gaussian distribution
#' @param Std_gauss list. standard deviation for parameters with Gaussian distribution
#'
#' @return LUT in data frame
#' @importFrom stats runif rnorm sd
#' @importFrom ToolsRTM gauss_byMin_Max
#' @export
get_distributionLUT<-function(minval=NULL,maxval=NULL,nSamples=NULL,TypeDistrib=NULL,Mean_gauss=NULL, Std_gauss=NULL, DepCab=NULL){
  # define InputPROSAIL # 3 random parameters
   inputLUT<-list()

   n_casesNorm=nSamples*2
   set.seed(1256)
   for (i in 1:length(minval)){
      trait <- names(minval)[i]
     
      # if uniform distribution
      if(TypeDistrib[[trait]] == 'Uniform') {
        inputLUT[[trait]] <- stats::runif(nSamples,min = minval[1,trait],max=maxval[1,trait])
         if (names(inputLUT)[i] == 'Car' & DepCab == T){
         inputLUT[[trait]] <- ToolsRTM::correlatedValue(x=inputLUT[['Cab']]/4, r=.8) 
         }
        
       
      }
      # if Gaussian distribution
      else  {
      inputLUT[[trait]] <- ToolsRTM::gauss_byMin_Max(n=nSamples, m=Mean_gauss[1,trait], s=std_gauss[1,trait], lwr=minval[1,trait], upr=maxval[1,trait], nnorm=n_casesNorm)
      if (names(inputLUT)[i] == 'Car' & DepCab == T){
         inputLUT[[trait]] <- ToolsRTM::correlatedValue(x=inputLUT[['Cab']]/4, r=.8) 
      }
      
      }
      
    
   }
   LUT.dataframe <-data.frame(do.call(cbind,inputLUT))
  return(LUT.dataframe)
}