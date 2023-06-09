
#' \code{get.RTM} fourSAIL simulation based on a set of combinations of input parameters
#' 
#'
#' @param LUT.table 
#' @param leaf.model 
#' @param canopy.model 
#' @param option 
#' @param BRDF 
#' @param get.plots 
#'
#' @return
#' @export
#'
#' @examples
get.RTM<-function(LUT.table,leaf.model='fluspect-CX',canopy.model='fourSAIL',
                    option='Foward-Mode', BRDF=T,
                    get.plots = T) {
  
  
  
  
  ##################################################################################
  ### 0.0 Load optipar (optical leaf properties)
  ##################################################################################
  
  if (missing(LUT.table)){
    stop('please be sure to add a LUT table with main inputs of the RT models')
  } 
  
  ##################################################################################
  ### 0.1 Check for plot or not
  ##################################################################################
  
  if (missing(get.plots)){
    get.plots = FALSE
  }
  
  ##################################################################################
  ### 0.2 Check for leaf model
  ##################################################################################
  
  if (missing(leaf.model)){
 
    stop('please be sure to select a leaf model, e.g., Fluspect-B-Cx, Fluspect-B; Liberty,PROSPECT-D and PROSPECT-PRO')
    
  }
  
  
  ##################################################################################
  ### 0.3 Check for canopy model
  ##################################################################################
  
  if (missing(canopy.model)){
    
    stop('please be sure to select a leaf model, e.g., fourSAIL; fourSAIL2; and INFORM')
    
  }
  
  
  
  
}