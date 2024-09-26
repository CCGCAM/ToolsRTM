


#' Calculates the layer average and the canopy average of leaf properties per layer, per leaf angle and per leaf azimuth (36)
#'
#' @param canopy
#' @param F_ input matrix (3D)   (nli, nlazi,nl)
#' @param choice   integration method  'angles' : integration over leaf angles
#' 'angles_and_layers' : integration over leaf layers and leaf angles
#' @param Ps  fraction sunlit per layer (nl)
#'
#' @return
#' @export
#' Last update:
#'  -  7   December 2007
#'  - update:   11  February 2008 made modular (Joris Timmermans)
#'  - update:   25 Feb 2013 Wout Verhoef : Propose name change, remove globals and use canopy-structure for input

#' @author 	Christiaan van der Tol (Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
meanleaf <- function(canopy, F_, canopy.choice, Ps) {

  nl <- canopy[['nlayers']]
  nli <- canopy[['nlincl']]
  nlazi <- canopy[['nlazi']]
  lidf <- canopy[['lidf']]


  # create an empty list
  F_list <- list()
  # loop over the third dimension of F_
  for (i in 1:dim(F_)[3]) {
    # extract a 13 x 36 matrix from F_ and add it to the list
    F_list[[i]] <- F_[,,i]
  }

  # Output:
  #   Fout    in case of choice = 'angles': [nl]
  #           in case of choice = 'angles_and_layers': [1]

  Fout <- array(0, dim = c(nli, nlazi,nl))

  switch(canopy.choice,

         # Integration over leaf angles
         'angles' = {
           for (j in 1:nli) {
             Fout[j,,] <- F_[j,,] * lidf[j]
           }


           Fout_mean <- list()
           for (j in 1:nl) {
             Fout_mean[[j]] <- sum(Fout[, , j]) / nlazi

           }
           ## new change
           Fout_vertical <- unlist( Fout_mean[[j]] )
           ## new change
           Fout_vertical <- rep(mean(unlist(Fout_mean)),nl)
         },

         # Integration over layers only
         'layers' = {


           #this is only for a single vector (nlayers)

           #F_nlayer <- apply(F_, c(3), sum)
           #Fout_vertical <- Ps[1:nl] *  F_nlayer / nl
           #return a single value
           Fout_vertical = sum(Ps * (F_) ) / nl

         },

         # Integration over both leaf angles and layers
         'angles_and_layers' = {
           for (j in 1:nli) {
             Fout[j,,] <- F_[j,,] * lidf[j]
           }
           for (j in 1:nl) {
             Fout[,,j] <- Fout[,,j] * Ps[j]
           }
           Fout_mean <- list()
           for (j in 1:nl) {
             Fout_mean[[j]] <- sum(Fout[, , j]) / (nlazi * nl)
           }

           Fout_vertical <- unlist(Fout_mean)
         }
  )

  return(Fout_vertical)
}
