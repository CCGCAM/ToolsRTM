#' get.inversion of the plant traits using different machine learning models
#'
#' @param data The data frame containing the variables.
#' @param depVar The dependent variable.
#' @param inputs The independent variables.
#' @param algorithm The type of machine learning model. Options: "PLSR" (Partial Least Squares Regression), "SVM" (Support Vector Machine),
#' "RF" (Random Forest); "NN" (Neural Network); "GB" (Gradient boosting); "Ensemble" (Stacking Ensemble models)
#' Default is "PLSR".
#' @param seed The seed for reproducibility. Default is 123.
#' @param save.model Logical indicating whether to save the trained models. Default is FALSE.
#' @param save.path Path to save the trained models. Required if save_models is TRUE.

#' @return A list containing predictions, statistics, and plots for the specified machine learning model.
#' @export
#'
#' @examples
#' get.inversion(data = my_data, depVar = "Cab", inputs = c("NDVI", "TCARI"), ML = "SVM", seed = 123)

get.inversion <- function(data, depVar, inputs, algorithm='PLSR',seed=123, save.model = FALSE, save.path = NULL) {

  # Set the seed for reproducibility
  if(is.null(seed)) {
    seed <- 123  # Default seed value
    set.seed(seed)
  } else{
    set.seed(seed)
  }

  # Create train-test split indices based on the 'Anth' column
  indices <- caret::createDataPartition(data[, depVar], p = 0.8, list = FALSE)

  # Split the data into training and testing sets
  df.train <- data[indices, ]
  df.test <- data[-indices, ]

  # Force to run PLSR if ML is null or empty
  if(is.null(algorithm) || algorithm == "") {
    algorithm <- "PLSR"
  }

  if(algorithm == "PLSR") {

    print('processing hybrid approach using PLSR ...')

    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))

    # Parallelize tuning process
    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)

    myfolds <- createMultiFolds(df.train[,depVar], k = 10, times = 10)
    control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

    # Train PLS model
    model_ <- train(fmla.n, data = df.train,
                   method = "pls",
                   metric = "RMSE",
                   tuneLength = 20,
                   trControl = control,
                   preProc = c("zv","center","scale"))

    k=model_$bestTune

    parallel::stopCluster(clusters)



  } else if(algorithm == "SVM") {

    print('processing hybrid approach using Support Vector Machine ...')

    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))

    # Reduce sample size for tuning
    rows.r <- sample(nrow(df.train), 500)

    # Parallelize tuning process
    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)

    tobj2 <- e1071::tune.svm(fmla.n, data = df.train[rows.r,], sampling = "fix",
                             gamma = 2^c(-10, -8, -6, -4),  # search space for gamma
                             cost = 2^c(-5, -3, -1, 1),     # search space for cost
                             tunecontrol = tune.control(cross = 5))  # number of cross-validation folds
    cc <- as.numeric(tobj2$best.parameters[2])
    gg <- as.numeric(tobj2$best.parameters[1])

    n.model <-round(dim(df.train)[1]/10,0)
    rows.model <- sample(nrow(df.train), n.model)
    # SVM
    model_ <- svm(fmla.n, kernel = "radial", data = df.train[rows.model, ], gamma = gg, cost = cc,
                     type = "eps-regression", probability = FALSE)

    parallel::stopCluster(clusters)



  } else if(algorithm == "RF") {


    print('processing hybrid approach using Random Forest ...')

    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))
    # Reduce sample size for tuning
    rows.r <- sample(nrow(df.train), 500)

    # Parallelize tuning process
    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)

    # Define tuning grid with reduced search space
    mtry2 <- randomForest::tuneRF(df.train[rows.r, inputs], y = df.train[rows.r, depVar],
                    ntreeTry = ncol(df.train[, inputs])/3, stepFactor = 1.5, improve = 0.01,
                    trace = F, plot = F)
    best.m <- mtry2[mtry2[, 2] == min(mtry2[, 2]), 1]
    metric <- "RMSE"
    tunegrid <- expand.grid(.mtry = best.m)
    n.trees <- ncol(df.train[, inputs])/3

    n.model <-round(dim(df.train)[1]/10,0)
    rows.model <- sample(nrow(df.train), n.model)

    # Define training control
    fit.control <- caret::trainControl(method = "repeatedcv", number = 3,
                 search = "grid", repeats = 3, allowParallel = TRUE)
    # Random Forest
    model_ <- caret::train(fmla.n, data = df.train[rows.model, ], method = "rf", metric = metric,
                           trControl = fit.control, verbose=F, tuneGrid = tunegrid)

    parallel::stopCluster(clusters)


  } else if (algorithm == 'GB'){
    print('processing hybrid approach using Gradient Boosting ...')

    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))

    # Reduce sample size for tuning
    rows.r <- sample(nrow(df.train), 500)

    n.model <- round(dim(df.train)[1] / 10 / 2, 0)
    rows.model <- sample(nrow(df.train), n.model)

    # Parallelize tuning process
    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)

    # Define training control
    fit.control <- caret::trainControl(method="repeatedcv", allowParallel=T,
                                       returnResamp = "all",
                                       savePredictions = "all",
                                       number=3, repeats=3, search="random")
    # Define tuning grid with reduced search space
    tune.grid <- expand.grid(shrinkage = seq(0.1, 1, by = 0.3),
                             interaction.depth = c(1, 5),
                             n.minobsinnode = c(2, 5),
                             n.trees = c(100, 300, 1000))

    ##Gradient Boosting
    model_<- caret::train(fmla.n, data = df.train[rows.model,],  method = "gbm", metric='RMSE',
                         # preProc = c('center', 'scale','BoxCox', 'YeoJohnson', 'expoTrans', 'ica'),
                         trControl = fit.control, tuneGrid =tune.grid,verbose = F)

    parallel::stopCluster(clusters)

  }  else if (algorithm == 'NN'){

    print('processing hybrid approach using a simple Neural Network')

    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))

    # Parallelize tuning process
    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)

    # Reduce sample size for tuning
    rows.r <- sample(nrow(df.train), 500)
    # Define training control
    fit.control <- caret::trainControl(method="repeatedcv", allowParallel=T,
                                       number=3, repeats=3, search="random",
                                       index = createFolds(df.train[rows.r,inputs], 5),
                                       returnResamp = "all",
                                       savePredictions = "all")
    # Define tuning grid with reduced search space
    nnet.grid <- expand.grid(.decay = seq(0,0.1,by=0.01),
                             .size = seq(1,10,by=1))#, .bag=F) for vNNet

    n.model <-round(dim(df.train)[1]/10,0)
    rows.model <- sample(nrow(df.train), n.model)

    ##Neural-Network Model
    model_<- caret::train(fmla.n, data = df.train[rows.model,],
                             method = "nnet", repeats = 1, trControl = fit.control,
                             preProc = c("center", "scale",'BoxCox', 'YeoJohnson'),
                             cross=10,trace=F, ##remove message with Trace=False
                             threshold = 0.3,
                             maxit = 1000, linout = 1,
                             metric='Rsquared', tuneGrid = nnet.grid,
                             verbose = FALSE)

    size<-getElement(model_,"bestTune")$size
    decay<-getElement(model_,"bestTune")$decay

    parallel::stopCluster(clusters)


  }  else if (algorithm == 'Ensemble') {
    print('processing Ensemble approach by stacking 3 models ....')
    print('list of models: SVM,Gradient Boosting and Neural Network ')


    # Parallelize tuning process
    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)

    algorithmList <- c('gbm', #Gradient-boosted machines
                       'svmRadial', #SVM with RBF Kernel
                       'nnet') #neural network

    # Reduce sample size for tuning
    n.model <-round(dim(df.train)[1]/10,0)
    rows.model <- sample(nrow(df.train), n.model)
    ### tuning parameters for each model

    # gmb
    tune.grid.gbm <- expand.grid(shrinkage = seq(0.1, 1, by = 0.3),
                                 interaction.depth = c(1, 5),
                                 n.minobsinnode = c(2, 5),
                                 n.trees = c(100, 300, 1000))
    # svm
    tuneGrid.svm = expand.grid(C = c(2^(1:4)),sigma=c(2^(-4:1)))
    # nnet
    tune.grid.nne <- expand.grid(.decay = seq(0.001,0.2,by=0.01), .size = seq(1,10,by=1))

    ### Ensemble mddels

    models.ensemble=list(gbm=caretEnsemble::caretModelSpec(method="gbm",  #metric='MAE',
                                            preProc = c("center", "scale",'BoxCox', 'YeoJohnson'),
                                            tuneGrid=tune.grid.gbm),
                         svmRadial=caretEnsemble::caretModelSpec(method="svmRadial", # metric='MAE',
                                                  preProc = c("center", "scale",'BoxCox', 'YeoJohnson'),
                                                  tuneGrid=tuneGrid.svm,#tuneLength=10,
                                                  threshold = 0.3),
                         #tuneGrid=tuneGrid.svm),
                         nnet=caretEnsemble::caretModelSpec(method="nnet", #metric='MAE',
                                             preProc = c("center", "scale",'BoxCox', 'YeoJohnson'),
                                             threshold = 0.3,
                                             tuneGrid=tune.grid.nne,
                                             maxit = 1000, linout = 1, trace=F))

    fit.control<- trainControl(method="repeatedcv",
                               number=10,
                               savePredictions=TRUE,
                               #index = createFolds(data.train[,input], 5),
                               repeats=3,
                               search = "random")

    models <- caretEnsemble::caretList(fmla.n, data = df.train[rows.model,],
                                       trControl=fit.control,
                                       verbose=FALSE,
                                       tuneList = models.ensemble,
                                       methodList=algorithmList)
    # Combine Predictions from multiple models
    stackControl <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=3,
                                 index = createFolds(df.train[rows.model,], 5),
                                 savePredictions = "all",
                                 search = "random")

    # Ensemble the predictions of `models` to form a new combined prediction based on glm
    model_ <- caretEnsemble::caretStack(models, method="glm", trControl=stackControl)

    parallel::stopCluster(clusters)

  } else {
    stop("Invalid model type. Choose from 'PLSR', 'SVM', 'RF', 'GB', 'NN' or 'Ensemble.")
  }


  if(save.model) {
    saveRDS(model_, file = file.path(save.path, paste('model-',ML,'.rds', sep='')))
  }

  if (algorithm == 'PLSR'){
    # Prediction
    pred.train <- stats::predict(object = model_, newdata = df.train)
    pred.test <- stats::predict(object = model_, newdata = df.test)

    # Statistics
    stats <- ToolsRTM::get.stats.plsr(model_, k, df.train, df.test, depVar)
    # Plot
    plot.result <- ToolsRTM::get.plot.plsr(model = model_, k, df.train, df.test, depVar)
  } else {
    # Prediction
    pred.train <- stats::predict(object = model_, df.train[, c(depVar, inputs)])
    pred.test <- stats::predict(object = model_, df.test[, c(depVar, inputs)])

    # Statistics
    stats <- ToolsRTM::get.stats(model = model_, train = df.train[, c(depVar, inputs)],
                                 test = df.test[, c(depVar, inputs)], var = depVar)

    # Plot
    plot.result <- ToolsRTM::get.plot.ML(model = model_, df.train[, c(depVar, inputs)],
                                         df.test[, c(depVar, inputs)], depVar)
  }

  results <- list(
    model.label = algorithm,
    model= model_,
    predictions = list(train = pred.train, test = pred.test),
    statistics = stats,
    plot = plot.result
  )

  return(results)
}
