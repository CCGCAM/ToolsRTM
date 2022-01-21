#' Forward implementation of coupled Radiative Transfer Models.
#'

#' @param PROSPECTversion A leaf RT model to run (see details).
##' @param fourSAILversion A canopy RT model to run (see details).
##' @param inputLUT LUT with all combinations (see details).
##' @param rsoil soil reflectance
##' @param inputLUTGreen_Brown dataframe LUT for Green Vegetation and Brown vegetation
#' @return list of Simulations 
#' @examples 
#' 


runRTM <- function(PROSPECTversion='PRO',fourSAILversion='fourSAIL', inputLUT,rsoil,
                   inputLUTGreen_Brown=NULL){

  ###################################################
  ### run model SAIL with PROSPECT_PRO or PROSPECT_D
  ###################################################
  if (is.null(inputLUTGreen_Brown)){
    message('running fourSAIL model')
    if (PROSPECTversion =='PRO'){
        ToolsRTM::m4SAIL(inputLUT=inputLUT,rsoil=rsoil,PROSPECTversion = 'PRO')
    } else {
        ToolsRTM::m4SAIL(inputLUT=inputLUT,rsoil=rsoil,PROSPECTversion = 'D')
    }
    
  }
  ###################################################
  ### run model SAIL with PROSPECT_PRO or PROSPECT_D
  ###################################################    
  else if (is.null(BrownVegetation) & fourSAILversion == 'fourSAIL2'){
    message('running fourSAIL model')
    if (PROSPECTversion =='PRO'){
      ToolsRTM::m4SAIL(inputLUT=inputLUT,rsoil=rsoil,PROSPECTversion = 'PRO')
    } else {
      ToolsRTM::m4SAIL(inputLUT=inputLUT,rsoil=rsoil,PROSPECTversion = 'D')
    }
    
  }
    
  ###################################################
  ### run model SAIL with PROSPECT_PRO or PROSPECT_D
  ###################################################    
  else {
    message('running fourSAIL model')
      if (PROSPECTversion =='PRO'){
      ToolsRTM::m4SAIL(inputLUT=inputLUT,rsoil=rsoil,PROSPECTversion = 'PRO')
    } else {
      ToolsRTM::m4SAIL(inputLUT=inputLUT,rsoil=rsoil,PROSPECTversion = 'D')
    }
  }
  

}