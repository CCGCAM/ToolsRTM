#' get.inversion of the plant traits using Partial Least Squares Regression, Support Vector Machine) and Random Forest
#'
#' @param data The data frame containing the variables.
#' @param depVar The dependent variable.
#' @param inputs The independent variables.
#' @param ML The type of machine learning model. Options: "PLSR" (Partial Least Squares Regression), "SVM" (Support Vector Machine), "RF" (Random Forest). Default is "PLSR".
#' @param seed The seed for reproducibility. Default is 123.
#' @param save.model Logical indicating whether to save the trained models. Default is FALSE.
#' @param save.path Path to save the trained models. Required if save_models is TRUE.

#' @return A list containing predictions, statistics, and plots for the specified machine learning model.
#' @export
#'
#' @examples
#' get.inversion(data = my_data, depVar = "Cab", inputs = c("NDVI", "TCARI"), ML = "SVM", seed = 123)

get.inversion <- function(data, depVar, inputs, ML='PLSR',seed=123, save.model = FALSE, save.path = NULL) {

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
  if(is.null(ML) || ML == "") {
    ML <- "PLSR"
  }

  if(ML == "PLSR") {
    # PLSR
    print("Training PLSR model...")
    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))

    model_ <- pls::plsr(fmla.n,
                       data = df.train, validation = "CV")
    clusters <- makeCluster(detectCores()-1)
    registerDoParallel(clusters)
    pt<- proc.time()

    myfolds <- createMultiFolds(df.train[,depVar], k = 10, times = 10)
    control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")
    # Train PLS model
    set.seed(232)
    model_ <- train(fmla.n, data = df.train,
                   method = "pls",
                   metric = "RMSE",
                   tuneLength = 20,
                   trControl = control,
                   preProc = c("zv","center","scale"))
    stopCluster(clusters)
    proc.time()-pt

    k=model_$bestTune

  } else if(ML == "SVM") {
    # SVM
    print("Training SVM model...")
    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))

    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)
    rows.r <- sample(nrow(df.train), 500)
    tobj2 <- e1071::tune.svm(fmla.n, data = df.train[rows.r,], sampling = "fix",
                             gamma = 2^c(-10, -8, -6, -4),  # search space for gamma
                             cost = 2^c(-5, -3, -1, 1),     # search space for cost
                             tunecontrol = tune.control(cross = 5))  # number of cross-validation folds
    cc <- as.numeric(tobj2$best.parameters[2])
    gg <- as.numeric(tobj2$best.parameters[1])

    n.model <-round(dim(df.train)[1]/10,0)
    rows.model <- sample(nrow(df.train), n.model)
    model_ <- svm(fmla.n, kernel = "radial", data = df.train[rows.model, ], gamma = gg, cost = cc,
                     type = "eps-regression", probability = FALSE)
    parallel::stopCluster(clusters)



  } else if(ML == "RF") {

    # Random Forest
    print("Training RF model...")
    fmla.n <- as.formula(paste(depVar," ~ ", paste(inputs, collapse= "+")))

    clusters <- parallel::makeCluster(detectCores() - 1)
    doParallel::registerDoParallel(clusters)
    rows.r <- sample(nrow(df.train), 500)
    mtry2 <- randomForest::tuneRF(df.train[rows.r, inputs], y = df.train[rows.r, depVar],
                    ntreeTry = ncol(df.train[, inputs])/3, stepFactor = 1.5, improve = 0.01,
                    trace = F, plot = F)
    best.m <- mtry2[mtry2[, 2] == min(mtry2[, 2]), 1]
    metric <- "RMSE"
    tunegrid <- expand.grid(.mtry = best.m)
    n.trees <- ncol(df.train[, inputs])/3
    n.model <-round(dim(df.train)[1]/10,0)
    rows.model <- sample(nrow(df.train), n.model)

    model_ <- caret::train(fmla.n,
                             data = df.train[rows.model, ], method = "rf", metric = metric,
                             trControl = trainControl(method = "repeatedcv", number = 3,
                                                      search = "grid", repeats = 3, allowParallel = TRUE),
                             tuneGrid = tunegrid)
    parallel::stopCluster(clusters)


  } else {
    stop("Invalid model type. Choose from 'PLSR', 'SVM', or 'RF'.")
  }


  if(save.model) {
    saveRDS(model_, file = file.path(save.path, paste('model-',ML,'.rds', sep='')))
  }

  if (ML == 'PLSR'){
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
    model.label = ML,
    model= model_,
    predictions = list(train = pred.train, test = pred.test),
    statistics = stats,
    plot = plot.result
  )

  return(results)
}
