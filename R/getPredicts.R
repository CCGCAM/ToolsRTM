#' getPredicts
#'
#' @param model a keras model
#' @param type.model the type of  keras model; the options are: 'CNN'; 'Hidden-layers'
#' @param data the dataset in matrix format for predicting depVar
#' @param data.Y  the Y variable: Avaliable options are: 'NULL' in real cases or a vector or matrix or data.frame
#' @param data.LUT the dataset for transformations 
#' @param depVar variable name 
#' @param transf PCA and normalized options are avalaible
#' @param depVar.Transf True or False are the options for DepVar
#' @return
#' @export
#'
#' @examples
getPredicts<-function(model=NULL, type.model='All',data=NULL, data.Y=data.YVal, data.LUT=LUT.to, depVar='Cab', transf=NULL,depVar.Transf=TRUE) {
  
  
  #model=model.3hlayers; data=data.Xval; data.Y=data.YVal; data.LUT=LUT.to; depVar='Cab';transf='normalized';depVar.Transf=FALSE

  if (is.null(transf)){
    stop('PLease insert a transormation for applying: PCA, normalized  ...')
  
  }
  
  if (is.null(depVar.Transf)){
    depVar.Trans=FALSE
  }
  
  if (is.null(depVar)){
    stop('PLease insert the name variable to predict ...')
  }
  
  if (type.model == "CNN" | type.model == "Hidden-layers"){
    cat('get predictions .....')
  } else{
    stop('PLease insert a type model: CNN; Hidden-layers  or All options are avalaible')
  }
  if (is.null(model)){
    stop('PLease insert a model for predicting  ...')
  }
  if (is.null(model)){
    stop('PLease insert a model for predicting  ...')
  }
  if (class(data)[1] != "matrix" | is.null(data)){
    stop('PLease insert a matrix with inputs ...')
  } 
  
  if (class(data.Y) != "numeric"){
   
    data.Y.to<-data.Y[,depVar]
  } else{
    data.Y.to <- data.Y
  }
  
  
  if (type.model == 'Hidden-layers'){
    
    
    pred.model.3hlayers <- predict(model, data)
    
    if (depVar.Transf == TRUE){
      pred.model.3hlayers <- pred.model.3hlayers * abs(diff(range(LUT.to[,depVar]))) + min(LUT.to[,depVar])
    }
    if (is.null(data.Y.to)){
        df.plot<-data.frame(data,predicted.3hlayer=pred.model.3hlayers,predicted.cnn='NA')
    } else {
          df.plot<-data.frame(depVar=data.Y.to,predicted.3hlayer=pred.model.3hlayers,predicted.cnn='NA')
    }
    
    
  } else if (type.model == 'CNN'){
    
    data.cnn <- array_reshape(data, c(nrow(data), ncol(data), 1))
    pred.model.cnn <- predict(model,data.cnn)
    if (depVar.Transf == TRUE){
      pred.model.cnn <- pred.model.cnn * abs(diff(range(LUT.to[,depVar]))) + min(LUT.to[,depVar])
    }
    if (is.null(data.Y.to)){
      df.plot<-data.frame(data,predicted.3hlayer='NA',predicted.cnn=pred.model.cnn)
    } else {
      df.plot<-data.frame(depVar=data.Y.to,predicted.3hlayer='NA',predicted.cnn=pred.model.cnn)
    }
  
    
  } else if (type.model == 'All'){
    
    pred.model.3hlayers <- predict(model, data)
    data.cnn <- array_reshape(data, c(nrow(data), ncol(data), 1))
    pred.model.cnn <- predict(model,data.cnn)
    
    if (depVar.Transf == TRUE){
      pred.model.3hlayers <- pred.model.3hlayers * abs(diff(range(LUT.to[,depVar]))) + min(LUT.to[,depVar])
      pred.model.cnn <- pred.model.cnn * abs(diff(range(LUT.to[,depVar]))) + min(LUT.to[,depVar])
    }
    if (is.null(data.Y.to)){
      df.plot<-data.frame(data,predicted.3hlayer=pred.model.3hlayers,predicted.cnn=pred.model.cnn)
      inputs_depVar= names(df.plot)[grep("pred.", names(df.plot))]
      df.plot$depVar_pred_avg=rowMeans(df.plot[,c(inputs_depVar)], na.rm=TRUE)
      df.plot$depVar_pred_sd<-apply(df.plot[,c(inputs_depVar)], 1, sd)
    } else {
      df.plot<-data.frame(depVar=data.Y.to,predicted.3hlayer=pred.model.3hlayers,predicted.cnn=pred.model.cnn)
      inputs_depVar= names(df.plot)[grep("pred.", names(df.plot))]
      df.plot$depVar_pred_avg=rowMeans(df.plot[,c(inputs_depVar)], na.rm=TRUE)
      df.plot$depVar_pred_sd<-apply(df.plot[,c(inputs_depVar)], 1, sd)
    }
   
  
  }
  
  
  return(df.plot)
  
}







