
#' Get LUT for radiative transfer models
#'
#' @param n_inputs  a  number with the legnth for the LUT ...
#' @param nLUT  the number of rows for the LUT
#' @param distribution  the number of rows for the LUT
#' @param setseed  the number of rows for the LUT
#' @param rho  the correlate value for Uniform distribution
#' @param Varnames  the number of rows for the LUT
#' @param MinRage  the number of rows for the LUT
#' @param MaxRange  the number of rows for the LUT
#' @return a dataframe with all parameters
#' @export
#'
#' @examples
#' 
#' 
getCor<-function(n_inputs=NULL, nLUT=100,  distribution = 'Uniform',setseed = 123, rho=NULL,
                         Varnames = NULL,
                         MinRage = NULL,
                         MaxRange = NULL){
  
  if (is.null(rho)){
    message('Please insert r value, only valid for Uniform distribution')
    stop()
  } 
  
  if (is.null(n_inputs)){
    message('Please insert number of variables that you need correlate')
    stop()
  } 
  
  if (is.null(distribution)){
    message('Please indicate the distribution function for all correlated inputs ...')
  } 
  if (is.null(n_inputs)){
    message('Please insert length of the variable')
    stop()
  }
  if (is.null(setseed)){
    set.seed(1234)
  }
  
  if (length(MinRage) != length(MinRage) ){
    message('Min and Max vectors should have same lengths')
    stop()
  }
  if (is.null(MinRage) | is.null(MinRage)){
    message('please some inputs is missing')
    stop()
  }
  #############################################################
  ### same variables
  ### list of functions
  list_distributions = list(function(L)rnorm(L,0,2), #normal
                            function(L)runif(L,0,1), #uniform
                            function(L) qpois(L,7), # poisson
                            function(L) round(qnorm(L,100,10)), ## normal
                            function(L) qnorm(L,-100,1)) ## normal

  n=n_inputs
  n.samples = nLUT
  #############################################################
  
  if ( distribution == 'Normal'){
    message('Generating a Normal distribution for all correlated inputs ...')
    
    A <- matrix(runif(n^2)*2, ncol=n) 
    Sigma <- t(A) %*% A
    print(Sigma)
    mu=(runif(n)*100)

    df.matrix <- MASS::mvrnorm(n.samples, mu = mu, Sigma = Sigma )  # from Mass package
   
    for (i in c(1:dim(df.matrix)[2])){
      
      df.matrix[,i]<-scales::rescale(df.matrix[,i], to = c(MinRage[i], MaxRange[i]))   
      df.matrix[,i]<-abs(jitter(df.matrix[,i], factor=2, amount = NULL))
       
    }
    
    #summary(df.matrix)
    # Calculate kernel density estimate
    #df.matrix.kde <- MASS::kde2d(df.matrix[,1], df.matrix[,n], n = n.samples/0.4)   # from MASS package
    
    # Contour plot overlayed on heat map image of results
    #print(graphics::image(df.matrix.kde))       # from base graphics package
    #print(contour(df.matrix.kde, add = TRUE)  )
    
    df.export<-data.frame(df.matrix)
    if ( is.null(Varnames)){
      colnames(df.export)<-paste('Var_',c(1:n),sep = '')
    } else {
      colnames(df.export)<-Varnames
    }
    M <-cor(df.matrix)
    #print(M)
    

  } 
  
  if ( distribution == 'Uniform'){
    message('Generating a Uniform distribution for all correlated inputs ...')
    norm.cop <- copula::normalCopula(rho,dim=n); 
    df.uniform <- copula::rCopula(n.samples, norm.cop)
    
    ## convert to uniform
    #df.uniform = pnorm(df.uniform) 
    #df.uniform = sapply(1:n, FUN = function(i) list_distributions[[2]](df.uniform[,i]))
    #pCopula(as.matrix(df.uniform),norm.cop)
    
    for (i in c(1:dim(df.uniform)[2])){
      
      df.uniform[,i]<-scales::rescale(df.uniform[,i], to = c(MinRage[i], MaxRange[i]))   
      df.uniform[,i]<-abs(jitter(df.uniform[,i], factor=2, amount = NULL))
      
    }
    
    df.export<-as.data.frame(df.uniform)
    if ( is.null(Varnames)){
      colnames(df.export)<-paste('Var_',c(1:n),sep = '')
    } else {
      colnames(df.export)<-Varnames
    }
    
    # Calculate kernel density estimate
    #mvn.kde <- MASS::kde2d(df.uniform[,1], df.uniform[,n], n = n.samples/0.4)  
    #print(graphics::image(mvn.kde))       # from base graphics package
    #print(contour(mvn.kde, add = TRUE))
    
    M <-cor(df.export)
    #print(M)
  }
  
  LUTdata = list('LUT'=df.export,'Covarianza'=M)
  return(LUTdata)
  
}
