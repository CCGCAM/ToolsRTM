
#' getMLmodel with a prefixed configuration
#'
#' @param dataset a dataframe
#' @param depVar name of the variable to predict
#' @param model a ML model. options are: 'CNN','Hidden-layers',
#' @param optimizer the optimizer for the model. options are: 'adam','adadelta','adagrad', 'adamax', 'nadam', 'msprop', 'sgd'
#' @param kfold a boolean variable for applying K-folds. By default is TRUE with n=3
#' @param n.Kfold  number of k-folds By default is 3
#' @param batch.size batch size used for each epoch. By default is 125
#' @param n.epochs  number of epoch. By default is 100
#' @param save.model a boolean variable for saving ML model, options are: TRUE or FALSE. if TRUE, please use path.model to give a folder for the model
#' @param path.model a path for saving the models. By default path.model ='Models'
#' @param prop.split a vector with proportion for spliting the dataset. prop.split =c(0.8,02) will be used as default.
#' @param data.trans a data.transformation method, options are: 'PCA','preProcess',
#' @param method.preProcess the data.transformation method for preProcess, data.transformation are: 'Normalize', 'YeoJohnson','BoxCox', Standarize', 'Center','Scale', and 'PCA'
#' @param depVar.trans a boolean variable for applying data transformation in Y variable, options are: TRUE or FALSE.
#'
#' @return a list with models and plots
#' @export
#'
#' @examples
#'


getMLmodel_withRetrain<-function(dataset=NULL, depVar='Cab',model='CNN',optimizer='adam',
                     kfold=NULL,n.Kfold=3,
                     batch.size=125,n.epochs=10, save.model=T, path.model=NULL,
                     prop.split=c(0.8,0.2),
                     data.trans='preProcess',method.preProcess='Normalize',
                     depVar.trans=FALSE) {

  stopifnot(class(dataset) == 'data.frame')

  stopifnot(model != 'CNN' | model != 'Hidden-layers')

  if (is.null(depVar)){
    stop('DepVar to predict should be indicated.')
  }


  if (!(depVar %in% colnames(dataset))){
    stop('inputs to predict should be in dataset, please check it ...')
  }
  if (is.null(depVar.trans)){
    depVar.trans=FALSE
  }

  if (is.null(kfold)) {
    n.kfold = 3
  } else if (kfold == FALSE){
    n.kfold = 1
  } else if (kfold == TRUE) {
    n.kfold = n.Kfold
  }

  inputs_<-colnames(dataset)
  if(any(inputs_ %in% depVar)){
    #print('DepVar is present')
    ##remove from dat
    inputs_<-inputs_[! inputs_ %in% depVar]
    ## reorder and put DepVar first in the vector.
    inputs_<-c(depVar,inputs_)
  }
  else{
    inputs_<-c(depVar,inputs_)
  }

  #print(inputs_)
  if (save.model == TRUE){
    if (exists('path.model') == FALSE){
      # output folder
      path.model='Models/'
      ifelse(!dir.exists(path.model), dir.create(path.model), FALSE)
      message(paste('model will save in ',path.model,' folder',sep=''))
    } else{
      # output folder
      path.model=path.model
      ifelse(!dir.exists(path.model), dir.create(path.model), FALSE)
      message(paste('model will save in ',path.model,' folder',sep=''))
    }


  }


  ##########################################################################################
  ##### Parameters for the models
  ##########################################################################################

  callbacks = list(callback_early_stopping(monitor = "loss", patience = 75, restore_best_weights = TRUE))

  if (is.null(n.epochs)){
    n.epochs = 100
  } else {
    batch.size = n.epochs
  }

  if (is.null(batch.size)){
    batch.size = 125
  } else {
    batch.size = batch.size
  }

  if (exists('optimizer') == FALSE) {
    optimizer = 'adam'
    opt = optimizer_adam(learning_rate = 0.0001,beta_1 = 0.9, beta_2 = 0.999)
  } else {
    opt = optimizer
  }

  if (optimizer == 'adadelta') {
    opt = optimizer_adadelta(learning_rate = 1, rho = 0.95, epsilon = NULL, decay = 0)

  } else if (optimizer =='adagrad') {
    opt = optimizer_adagrad(learning_rate = 0.01, epsilon = NULL, decay=0)

  } else if (optimizer =='adamax'){
    opt = optimizer_adamax( learning_rate = 0.002, beta_1 = 0.9,beta_2 = 0.999)

  } else if (optimizer =='nadam'){
    opt = optimizer_nadam(learning_rate = 0.002, beta_1 = 0.9,  beta_2 = 0.999, epsilon = NULL, schedule_decay = 0.004)

  } else if (optimizer =='nadam'){
    opt = optimizer_rmsprop(learning_rate = 0.001, rho = 0.9, epsilon = NULL, decay = 0)

  } else if (optimizer =='sgd'){
    opt = optimizer_sgd(learning_rate = 0.01, momentum = 0, decay = 0,  nesterov = FALSE)
  }

  ###########################################################################
  ############ step to improve the model
  ###########################################################################
  ## 1.Reduce your learning rate to a very small number like 0.001 or even 0.0001.
  ## 2.Provide more data.
  ## 3.Set Dropout rates to a number like 0.2. Keep them uniform across the network.
  ## 4.Try decreasing the batch size.
  ###########################################################################

  stats<-list()
  preds.model <- list()
  preds.model.retrain <- list()
  scatters <- list()

  total=n.kfold
  barProgress <- txtProgressBar(min = 1, max = total, style = 3)

  for (i.kfold in c(1:n.kfold)){
    #print(i.kfold)
    setTxtProgressBar(barProgress, i.kfold)
    ##########################################################################################
    ##### Split the Dataset
    ##########################################################################################

    split.data<-ToolsRTM::getSplitData_noMessages(data=dataset[,inputs_], depVar=depVar,inputs=inputs_[-1],
                                       data.trans=data.trans,prop.split=prop.split,method.preProcess=method.preProcess,
                                       depVar.trans=depVar.trans)
    scaler.train <-  split.data[['Scalar.train']]
    ##########################################################################################
    ##### Run the models
    ##########################################################################################

    if (model == 'Hidden-layers') {

      ##############################################################################################################################
      # sequential ML model ---
      ##############################################################################################################################


      data.Xtrain.reshape <- array_reshape(split.data[['Xtrain']], c(nrow(split.data[['Xtrain']]), ncol(split.data[['Xtrain']])))
      dim(data.Xtrain.reshape)
      # Create the configuration for the model
      model.dML <- keras_model_sequential() %>%
        layer_dense(units=64,activation="relu",
                    input_shape=c(dim(data.Xtrain.reshape)[2]))  %>%
        #layer_dropout(rate=0.1) %>%
        layer_dense(units = 32, activation = 'relu') %>%
        layer_dropout(rate=0.1) %>%
        layer_dense(units = 16, activation = 'relu') %>%
        #layer_dropout(rate=0.1) %>%
        layer_dense(units = 1, activation = 'relu')

      # Compile the configuration for the model
      model.dML %>% compile(loss = "mse",
                            optimizer = opt,#'adam',#,get_optimizer(),#"adam", #'sgd' can also be used
                            metrics = list("mean_absolute_error"))
      #model.dML %>% summary()


      # fit the configuration for the model
      history.model.dML<- model.dML %>% fit(data.Xtrain.reshape, split.data[['Ytrain']],
                                            epochs = n.epochs, batch_size = batch.size, verbose=0,shuffle=F,callbacks =callbacks,
                                            validation_split = 0.2)

      # evaluate the configuration for the model
      model.dML %>% evaluate(split.data[['Xval']], split.data[['Yval']])

      ### skill scores
      #stats[['Hidden-layers']] <- model.dML %>% evaluate(split.data[['Xval']], split.data[['Yval']])

      if (depVar.trans == FALSE) {

        df.val<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                       data=split.data[['Xval']], data.trans=data.trans,
                                       data.Y=split.data[['Yval']],
                                       depVar=depVar)
        df.val<- df.val[, colSums(is.na(df.val)) != nrow(df.val)]
        colnames(df.val) <- c(depVar,paste(depVar,'.predicted',sep=''))
      } else {
        scaler.depVar = split.data[['Scalar.Ytrain']]
        df.val<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                       data=split.data[['Xval']], data.trans=data.trans,
                                       data.Y=split.data[['Yval']],
                                       depVar=depVar, scaler.depVar= scaler.depVar)
        df.val <- df.val[, colSums(is.na(df.val)) != nrow(df.val)]
        colnames(df.val) <- c(depVar,paste(depVar,'.predicted',sep=''))
        df.val[,1] <-ToolsRTM::getReverse.trans(preProc=scaler.depVar,data=as.matrix(df.val[,depVar]))

      }

      ### skill scores
      table.stats <- list()
      table.stats['VarDep'] <- depVar
      table.stats['Model'] <- model
      table.stats['Trans'] <- method.preProcess
      table.stats['db'] <-'Testing'
      table.stats['kfold'] <- i.kfold
      table.stats['MAE'] <-  round(MLmetrics::MAE(df.val[,depVar], df.val[,paste(depVar,'.predicted',sep='')]),3)
      table.stats['RMSE'] <-  round(MLmetrics::RMSE(df.val[,depVar], df.val[,paste(depVar,'.predicted',sep='')]),3)
      table.stats['R2'] <-  round(MLmetrics::R2_Score(df.val[,depVar], df.val[,paste(depVar,'.predicted',sep='')]),3)
      table.stats<-data.frame(do.call(cbind,table.stats))
      print(table.stats)

      #############################################################################################################################
      #	Update model wit new predictions  -----
      ##############################################################################################################################

      data.to.retrain <- dataset[,inputs_]
      data.to.retrain <-na.omit(data.to.retrain)


      dim(data.to.retrain)
      split.retrain<-ToolsRTM::getSplitData_noMessages(data=data.to.retrain, depVar=depVar,inputs=inputs.to,
                                            data.trans=data.trans,prop.split=c(0.95,0.05),method.preProcess=method.preProcess,
                                            depVar.trans=depVar.trans)
      scaler.train.retrain <-  split.retrain[['Scalar.train']]

      data.Xtrain.reshape <- array_reshape(split.retrain[['Xtrain']], c(nrow(split.retrain[['Xtrain']]), ncol(split.retrain[['Xtrain']])))
      dim(data.Xtrain.reshape)

      # fit the configuration for the model
      history.model.dML<- model.dML %>% fit(data.Xtrain.reshape, split.retrain[['Ytrain']],
                                            epochs = floor(n.epochs/2), batch_size = batch.size, verbose=0,shuffle=F,callbacks =callbacks,
                                            validation_split = 0.1)

      # evaluate the configuration for the model
      model.dML %>% evaluate(split.retrain[['Xval']], split.retrain[['Yval']])

      ### skill scores
      #stats[['Hidden-layers']] <- model.dML %>% evaluate(split.data[['Xval']], split.data[['Yval']])

      if (depVar.trans == FALSE) {

        df.val.retrain<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                      data=split.data[['Xval']], data.trans=data.trans,
                                      data.Y=split.data[['Yval']],
                                      depVar=depVar)
        df.val.retrain<- df.val.retrain[, colSums(is.na(df.val.retrain)) != nrow(df.val.retrain)]
        colnames(df.val.retrain) <- c(depVar,paste(depVar,'.predicted',sep=''))
      } else {
        scaler.depVar = split.data[['Scalar.Ytrain']]
        df.val.retrain<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                      data=split.data[['Xval']], data.trans=data.trans,
                                      data.Y=split.data[['Yval']],
                                      depVar=depVar, scaler.depVar= scaler.depVar)
        df.val.retrain <- df.val.retrain[, colSums(is.na(df.val.retrain)) != nrow(df.val.retrain)]
        colnames(df.val.retrain) <- c(depVar,paste(depVar,'.predicted',sep=''))
        df.val.retrain[,1] <-ToolsRTM::getReverse.trans(preProc=scaler.depVar,data=as.matrix(df.val.retrain[,depVar]))

      }


      table.stats.r<-list()
      table.stats.r['VarDep'] <- depVar
      table.stats.r['Model'] <- model
      table.stats.r['Trans'] <- method.preProcess
      table.stats.r['db'] <-'Val.retrain'
      table.stats.r['kfold'] <- i.kfold

      table.stats.r['MAE'] <-  round(MLmetrics::MAE(df.val.retrain[,depVar], df.val.retrain[,paste(depVar,'.predicted',sep='')]),3)
      table.stats.r['RMSE'] <-  round(MLmetrics::RMSE(df.val.retrain[,depVar], df.val.retrain[,paste(depVar,'.predicted',sep='')]),3)
      table.stats.r['R2'] <-  round(MLmetrics::R2_Score(df.val.retrain[,depVar], df.val.retrain[,paste(depVar,'.predicted',sep='')]),3)

      table.stats.r<-data.frame(do.call(cbind,table.stats.r))



      # Save the model
      if (save.model == TRUE){
        model.dML %>% save_model_hdf5(paste(path.model,'/model_3hlayers_for_',depVar,'-',i.kfold,'.hdf5',sep=''))
      }

    } else if (model == 'CNN'){

      ##############################################################################################################################
      # CNN ML model ---
      ##############################################################################################################################

      #Reshaping the data for CNN
      ### These dimensions don't look correct; switch ncol() with nrow()

      data.Xtrain.CNN <- array_reshape(split.data[['Xtrain']], c(nrow(split.data[['Xtrain']]), ncol(split.data[['Xtrain']]), 1))
      data.Xval.CNN <- array_reshape(split.data[['Xval']], c(nrow(split.data[['Xval']]), ncol(split.data[['Xval']]), 1))

      # Create the configuration for the model
      dataset.dim=data.Xtrain.CNN
      model.dML <- keras_model_sequential() %>%
        layer_conv_1d(filters=64, kernel_size=4, activation="relu",
                      ### The input shape doesn't look correct; instead of
                      ### `c(ncol(dataTrain_x), nrow(dataTrain_x))` (354, 13)
                      ### I believe you want `dim(dataTest_x)` (13, 1)
                      input_shape=c(ncol(dataset.dim), 1)) %>%
        layer_max_pooling_1d(pool_size=2) %>%
        layer_conv_1d(filters=32, kernel_size=2, activation="relu") %>%
        layer_max_pooling_1d(pool_size=2) %>%
        #layer_dropout(rate=0.4) %>%
        layer_flatten() %>%
        layer_dense(units=16, activation="relu") %>%
        #layer_dropout(rate=0.1) %>%
        layer_dense(units=1, activation="relu")

      # Compile the configuration for the model

      model.dML %>% compile(loss = "mse",
                            optimizer =  opt,#"adam", #'sgd' can also be used
                            metrics = list("mean_absolute_error"))

      #model.dML %>% summary()
      # fit the configuration for the model
      history.model.dML <- model.dML %>% fit(data.Xtrain.CNN, split.data[['Ytrain']],
                                             epochs = n.epochs,batch_size = batch.size, verbose=0,shuffle=F,
                                             callbacks = callbacks,
                                             validation_split = 0.2)
      # evaluate the configuration for the model
      #stats[['CNN-model']] <-model.dML %>% evaluate(data.Xval.CNN, split.data[['Yval']])


      if (depVar.trans == FALSE) {

        df.val<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                      data=split.data[['Xval']], data.trans=data.trans,
                                      data.Y=split.data[['Yval']],
                                      depVar=depVar)
        df.val<- df.val[, colSums(is.na(df.val)) != nrow(df.val)]
        colnames(df.val) <- c(depVar,paste(depVar,'.predicted',sep=''))
      } else {
        scaler.depVar = split.data[['Scalar.Ytrain']]
        df.val<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                      data=split.data[['Xval']], data.trans=data.trans,
                                      data.Y=split.data[['Yval']],
                                      depVar=depVar, scaler.depVar= scaler.depVar)
        df.val <- df.val[, colSums(is.na(df.val)) != nrow(df.val)]
        colnames(df.val) <- c(depVar,paste(depVar,'.predicted',sep=''))
        df.val[,1] <-ToolsRTM::getReverse.trans(preProc=scaler.depVar,data=as.matrix(df.val[,depVar]))

      }

      ### skill scores
      table.stats <- list()
      table.stats['VarDep'] <- depVar
      table.stats['Model'] <- model
      table.stats['Trans'] <- method.preProcess
      table.stats['db'] <-'Testing'
      table.stats['kfold'] <- i.kfold
      table.stats['MAE'] <-  round(MLmetrics::MAE(df.val[,depVar], df.val[,paste(depVar,'.predicted',sep='')]),3)
      table.stats['RMSE'] <-  round(MLmetrics::RMSE(df.val[,depVar], df.val[,paste(depVar,'.predicted',sep='')]),3)
      table.stats['R2'] <-  round(MLmetrics::R2_Score(df.val[,depVar], df.val[,paste(depVar,'.predicted',sep='')]),3)
      table.stats<-data.frame(do.call(cbind,table.stats))
      print(table.stats)
      #############################################################################################################################
      #	Update model wit new predictions  -----
      ##############################################################################################################################

      data.to.retrain <- dataset[,inputs_]
      data.to.retrain <-na.omit(data.to.retrain)
      dim(data.to.retrain)
      split.retrain<-ToolsRTM::getSplitData_noMessages(data=data.to.retrain, depVar=depVar,inputs=inputs.to,
                                            data.trans=data.trans,prop.split=c(0.95,0.05),method.preProcess=method.preProcess,
                                            depVar.trans=depVar.trans)
      scaler.train.retrain <-  split.retrain[['Scalar.train']]

      data.Xtrain.reshape <- array_reshape(split.retrain[['Xtrain']], c(nrow(split.retrain[['Xtrain']]), ncol(split.retrain[['Xtrain']]), 1))
      data.Xval.reshape <- array_reshape(split.retrain[['Xval']], c(nrow(split.retrain[['Xval']]), ncol(split.retrain[['Xval']]), 1))
      dim(data.Xtrain.reshape)

      # fit the configuration for the model
      history.model.dML<- model.dML %>% fit(data.Xtrain.reshape, split.retrain[['Ytrain']],
                                            epochs = floor(n.epochs/2), batch_size = batch.size, verbose=0,shuffle=F,callbacks =callbacks,
                                            validation_split = 0.1)

      # evaluate the configuration for the model
      model.dML %>% evaluate(data.Xval.reshape, split.retrain[['Yval']])

      ### skill scores
      #stats[['Hidden-layers']] <- model.dML %>% evaluate(split.data[['Xval']], split.data[['Yval']])

      if (depVar.trans == FALSE) {

        df.val.retrain<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                              data=split.data[['Xval']], data.trans=data.trans,
                                              data.Y=split.data[['Yval']],
                                              depVar=depVar)
        df.val.retrain<- df.val.retrain[, colSums(is.na(df.val.retrain)) != nrow(df.val.retrain)]
        colnames(df.val.retrain) <- c(depVar,paste(depVar,'.predicted',sep=''))
      } else {
        scaler.depVar = split.data[['Scalar.Ytrain']]
        df.val.retrain<-ToolsRTM::getPredicts(model=model.dML, type.model=model,
                                              data=split.data[['Xval']], data.trans=data.trans,
                                              data.Y=split.data[['Yval']],
                                              depVar=depVar, scaler.depVar= scaler.depVar)
        df.val.retrain <- df.val.retrain[, colSums(is.na(df.val.retrain)) != nrow(df.val.retrain)]
        colnames(df.val.retrain) <- c(depVar,paste(depVar,'.predicted',sep=''))
        df.val.retrain[,1] <-ToolsRTM::getReverse.trans(preProc=scaler.depVar,data=as.matrix(df.val.retrain[,depVar]))

      }

      # skill scores
      table.stats.r<-list()
      table.stats.r['VarDep'] <- depVar
      table.stats.r['Model'] <- model
      table.stats.r['Trans'] <- method.preProcess
      table.stats.r['db'] <-'Val.retrain'
      table.stats.r['kfold'] <- i.kfold

      table.stats.r['MAE'] <-  round(MLmetrics::MAE(df.val.retrain[,depVar], df.val.retrain[,paste(depVar,'.predicted',sep='')]),3)
      table.stats.r['RMSE'] <-  round(MLmetrics::RMSE(df.val.retrain[,depVar], df.val.retrain[,paste(depVar,'.predicted',sep='')]),3)
      table.stats.r['R2'] <-  round(MLmetrics::R2_Score(df.val.retrain[,depVar], df.val.retrain[,paste(depVar,'.predicted',sep='')]),3)

      table.stats.r<-data.frame(do.call(cbind,table.stats.r))



      # Save the model
      if (save.model == TRUE){
        model.dML %>% save_model_hdf5(paste(path.model,'1-model-CNN_for_',depVar,'-',i.kfold,'.hdf5',sep=''))
      }
    }
    ##############################################################################################################################
    # Save table with skill scores for kfolds  ---
    ##############################################################################################################################

    table.stats.to.save <- rbind(table.stats,table.stats.r)
    print(table.stats.to.save)
    stats[[i.kfold]] <-table.stats.to.save

    y.predicted = paste(depVar,'.predicted',sep='')
    preds.model[[i.kfold]] <-df.val.retrain[,c(depVar,y.predicted)]
    preds.model.retrain[[i.kfold]] <-df.val.retrain[,c(depVar,y.predicted)]



    ##############################################################################################################################
    # Plot predicted DepVar by model at Testing dataset  ---
    ##############################################################################################################################

    axis_x<-expr(paste('measured ', !!depVar,sep=''))# axis x
    axis_y<-expr(paste('predicted ', !!depVar,sep=''))# axis y
    max_ <- max(df.val.retrain[,depVar])

    #### scatterplot for ANN model

    r2.SE2<-round(cor(df.val.retrain[,depVar],df.val.retrain[,y.predicted],use='pairwise.complete.obs')^2,2)
    rmse.se<-round(ToolsRTM::RMSE(df.val.retrain[,depVar],df.val.retrain[,y.predicted]),2)
    mylabel.r.se2 = bquote(bold(SE2: r)^2 == .(format(r2.SE2, digits = 3)))

    statsLabel = paste0("r2 = ", round(r2.SE2,2), ", RMSE = ",round(rmse.se,2))

    scatter.model <-  ggplot(df.val.retrain, aes_string(x=depVar, y=y.predicted)) +
      geom_point(alpha=0.1,shape = 16,aes(), size=1.5) +  #geom_smooth(method=lm, formula = 'y ~ x', se=F,lty=2, lwd=1) +
      theme_bw() + xlim(0,max_) + ylim(0,max_)  + ggtitle('ANN model')  +

      scale_color_gradient(low = "#0091ff", high = "#f0650e") +
      theme(legend.position="bottom",
            plot.title = element_text(hjust = 0.5, size=10,face="bold"),
            axis.title = element_text(face="bold", size=10),
            axis.title.x = element_text(face="bold", size=12),
            axis.title.y = element_text(face="bold", size=12),
            axis.text.y=element_text(hjust = 0.5, size=10,face="bold"),
            axis.text.x=element_text(hjust = 0.5, size=10,face="bold"),
            legend.title=element_blank()) +
      labs(title = statsLabel, x=axis_x,y=axis_y, size=8,face="bold") +
      stat_smooth(method = "lm",formula = y ~ x,geom = "smooth",col='#C1CDC1',lty=2,se=T)
    print(scatter.model)

    scatters[[i.kfold]] <- scatter.model



  }

  stats.to.export<-data.frame(do.call(rbind, stats))
  #preds.model.to.export<-data.frame(do.call(cbind, preds.model))
  #colnames(preds.model.to.export) <-c(paste(depVar,'model',method.preProcess,c(1:n.Kfold),sep='.'))

  #preds.model.retrain.to.export<-data.frame(do.call(cbind, preds.model.retrain))
  #colnames(preds.model.retrain.to.export) <-c(paste(depVar,'retrain',method.preProcess,c(1:n.Kfold),sep='.'))

  if (depVar.trans == FALSE) {
    model.outputs<- list('model' = model.dML,'history' =history.model.dML,
                         'stats' = stats.to.export,
                         'Scalar.train' = split.retrain[['Scalar.train']],
                         'Scalar.Ytrain' = NA,
                         'plot.val' =scatters,
                         'plot.cor' = split.retrain[['plot.train']])
  }
  else {
    model.outputs<- list('model' = model.dML,'history' =history.model.dML,
                         'stats' = stats.to.export,
                         'Scalar.train' = split.retrain[['Scalar.train']],
                         'Scalar.Ytrain' = split.retrain[['Scalar.Ytrain']],
                         'plot.val' = scatters,
                         'plot.cor' = split.retrain[['plot.train']])
  }

  return(model.outputs)
  close(barProgress)
}



