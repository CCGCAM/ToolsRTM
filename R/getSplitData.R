#' getSplitData for ML models
#'
#' @param data A dataframw with inputs and dep
#' @param depVar name of the variable to predict
#' @param transf a trnasformation method, potions are: 'PCA', 'normalized'
#' @param depVar.Transf a boolean variable for applying transformation in depend variable, options are: TRUE or FALSE
#' @param inputs 
#'
#' @return the training and testing dataset
#' @export
#'
#' @examples
getSplitData<-function(data=NULL, depVar='Cab',inputs=NULL, transf=NULL,depVar.Transf=TRUE) {
  
  if (is.null(transf)){
    stop('PLease insert a transormation for applying: PCA, normalized  ...')
  }
  
  if (is.null(depVar.Transf)){
    depVar.Trans=FALSE
  }
  
  if (is.null(depVar)){
    stop('PLease insert a vector with the variable to predict ...')
  }

  if (is.null(inputs)){
    stop('PLease insert a vector with the inputs  ...')
  }
  if (class(data) != "data.frame" | is.null(data)){
    stop('PLease insert a dataframe with the depend variable and inputs ...')
  } else{
    
   inputs_to<-which(names(data)  %in% inputs)
   inputs_to<-names(data)[inputs_to]
   data<-data[,c(depVar,inputs_to)]
   ## Nplit the data
   ind <- sample(2, nrow(data), replace=TRUE, prob = c(0.8,0.2))
   inputs.to.include = names(data)[-1] ## for rfl bands
   cat('inputs are: ',inputs.to.include)
   # change the data to matrix
   LUT.keras<-data[,c(depVar,inputs.to.include)]
   #skimr::skim(LUT.keras)
   
   ### Clean data
   #lapply(LUT.keras, function(x) sum(is.na(x))) %>% str()
   LUT.keras <- na.omit(LUT.keras)
   names_to<-c(depVar,inputs.to.include)
  }
  
  if (transf == 'normalized'){

    LUT.to <- as.data.frame(lapply(LUT.keras[,names_to], normalize))
    LUT.to <-cbind(LUT.keras[,depVar],LUT.to)
    colnames(LUT.to)<-c(depVar,paste(depVar,'_Tra',sep=''),inputs.to.include)
    LUT.to<-as.matrix(LUT.to)
    head(LUT.to)
    dim(LUT.to)

    data.Xtrain <- LUT.to[ind==1, 3:dim(LUT.to)[2]]
    data.Xval <- LUT.to[ind==2, 3:dim(LUT.to)[2]]
  
    M <- cor(data.Xtrain)
    plot.cor<-corrplot::corrplot(M, method = 'square', order = 'FPC',
                                 type = 'lower',
                                 tl.cex=0.5,tl.col = "black", cl.ratio=0.4,
                                 diag = FALSE)
  
    if (depVar.Transf == TRUE){
      
      data.Ytrain <- round(LUT.to[ind==1, 2],4)
      length(data.Ytrain)
      data.Yval <- round(LUT.to[ind==2, 2],4)
      length(data.Yval)
      
    } else{
      
      data.Ytrain <- round(LUT.to[ind==1, 1],4)
      length(data.Ytrain)
      data.Yval <- round(LUT.to[ind==2, 1],4)
      length(data.Yval)
    
    }
  

    
  } else if (transf == 'PCA' ){
    
  
    LUT.to <-prcomp(LUT.keras[,inputs.to.include], scale = TRUE)
    
    
    plot.pca.1<-factoextra::fviz_eig(LUT.to,)
    print(plot.pca.1)
    
    plot.pca.2<-factoextra::fviz_pca_var(LUT.to,
                                         col.var = "contrib", # Color by contributions to the PC
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         repel = TRUE)     # Avoid text overlapping)

    
    Cumm_pca<-summary(LUT.to)$importance[3,]
    max.pca<-which(Cumm_pca  > 0.99)
    
    LUT.to <-LUT.to$x
    LUT.to<-LUT.to[,1:max.pca[1]]
  
   
    data.Xtrain <- LUT.to[ind==1, 1:dim(LUT.to)[2]]
    data.Xval <- LUT.to[ind==2, 1:dim(LUT.to)[2]]
    
    
    if (depVar.Transf == TRUE){
      
      LUT.to <- as.data.frame(lapply(LUT.keras[,names_to], normalize))
      LUT.to <-cbind(LUT.keras[,depVar],LUT.to)
      colnames(LUT.to)<-c(depVar,paste(depVar,'_Tra',sep=''),inputs.to.include)
      LUT.to<-as.matrix(LUT.to)
      
      data.Ytrain <- round(LUT.to[ind==1, 2],4)
      data.Yval <- round(LUT.to[ind==2, 2],4)
      
    } else {
      
      LUT.to <- as.data.frame(lapply(LUT.keras[,names_to], normalize))
      LUT.to <-cbind(LUT.keras[,depVar],LUT.to)
      
      colnames(LUT.to)<-c(depVar,paste(depVar,'_Tra',sep=''),inputs.to.include)
      LUT.to<-as.matrix(LUT.to)
      
      data.Ytrain <- LUT.to[ind==1, 1]
      length(data.Ytrain)
      data.Yval <- LUT.to[ind==2, 1]
      length(data.Yval)
    
    }
    
    
  }
  split.database<- list("Xtrain" = data.Xtrain,"Ytrain" =data.Ytrain,
                        "XVal" =data.Xval,"YVal" = data.Yval,"LUT.to" = LUT.to)
  

return(split.database)

}
  
  

## Normalization of dataset
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

  

  
 
  