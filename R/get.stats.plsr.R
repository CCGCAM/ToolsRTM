#' Get Statistical Scores for PLSR Model
#'
#' This function calculates statistical scores such as R-squared, RMSE, and MAE
#' for both training and testing datasets based on predictions from a Partial Least Squares
#' Regression (PLSR) model.
#'
#' @param model The trained ML model.
#' @param k The number of components used for prediction.
#' @param train The training dataset.
#' @param test The testing dataset.
#' @param var The variable of interest.
#'
#' @return A data frame containing the statistical scores (R-squared, RMSE, MAE)
#' for both training and testing datasets.
#'
get.stats.plsr <- function(model,k,train,test,var){


  ### Equations staistical scores
  # Function that returns Mean Absolute Error
  MAE <- function(m,o){error<-m - o
  mean(abs(error))}
  RMSE = function(m, o){
    sqrt(mean((m - o)^2))}
  #################

pred.train<-predict(object = model,ncomp = k,newdata=train)
pred.test<-predict(object = model,ncomp = k,newdata=test)

#### Skill scores for training data
r2.train<-round(cor(pred.train,train[,var],use='pairwise.complete.obs')^2,2)
rmse.train<-round(RMSE(pred.train,train[,var]),2)
mae.train<-round(MAE(pred.train,train[,var]),2)
#### Skill scores for testing data
r2.test<-round(cor(pred.test,test[,var],use='pairwise.complete.obs')^2,2)
rmse.test<-round(RMSE(pred.test,test[,var]),2)
mae.test<-round(MAE(pred.test,test[,var]),2)

stats<-data.frame(r2=c(r2.train,r2.test),
                  rmse=c(rmse.train,rmse.test),
                  mae=c(mae.train,mae.test))

return(stats)


}

