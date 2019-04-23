############################################################################################
##
##    Latest version of the modelS.
##
############################################################################################

getData <- function(df, predictors, targetVariable, treatmentVariable){
  return(cbind(x = df[,predictors], y = df[,targetVariable], ct = df[,treatmentVariable]))
}

#' Training a model according to the "Two Model Approach" (a.k.a. "Separate Model Approach")
#' The underlying techniques rely on the 'caret'-package.
#' Two main parameters, method and trControl, are passed to the 'train'-function of Caret.
#' 
#' @source "Incremental Value Modeling" (Hansotia, 2002)
#'
#' @param X A data frame of predictors.
#' @param y A binary response (numeric) vector.
#' @param ct A binary response (numeric) representing the treatment assignment (coded as 0/1).
#' @param method A string specifying which classification or regression model to use. Possible values are found using names(getModelInfo()). See http://topepo.github.io/caret/train-models-by-tag.html. This should be a method that can handle a multinominal class variable.
#' @param trControl A list of values that define how this function acts. See trainControl and http://topepo.github.io/caret/using-your-own-model-in-train.html. (NOTE: If given, this argument must be named.)
#' 
#' @return A list of two models. One for the treatment group, one for the control group.
tma <- function(x, y, ct, method = "glm", trControl = trainControl(method = "none"), ...){
  if (!is.factor(y)) {y <- factor(y)}
  
  df <- cbind(x, ct, y)
  rownames(df) <- 1:nrow(df)
  
  fml <- as.formula(paste("y ~ ", paste(colnames(x), collapse = "+")))
  
  require(caret)
  mdl.trt <- train(fml, data = df[ct==1,], method = method, trControl = trControl, ...)   # Model for the treatment-group
  mdl.ctrl <- train(fml, data = df[ct==0,], method = method, trControl = trControl, ...)  # Model for the control-group
  
  res <- list(mdl.treatment = mdl.trt,
              mdl.control = mdl.ctrl, 
              method = method)
  class(res) <- "tma"
  
  return(res)   
}

#' Predictions according to the "Two Model Approach" (a.k.a. "Separate Model Approach")
#' The underlying techniques rely on the 'caret'-package.
#' 
#' For each instance in newdata two predictions are made:
#' 1) What is the probability of a person responding when treated?
#' 2) What is the probability of a person responding when not treated (i.e. part of control group)?
#' 
#' @source "Incremental Value Modeling" (Hansotia, 2002)
#'
#' @param object A list of two models. One for the treatment group, one for the control group.
#' @param newdata A data frame containing the values at which predictions are required.
#' 
#' @return A dataframe with predictions for when the instances are treated and for when they are not treated.
predict.tma <- function(object, newdata){
  rownames(newdata) <- 1:nrow(newdata)
  pred.trt <- predict(object$mdl.treatment, newdata, type="prob")
  pred.ctrl <- predict(object$mdl.control, newdata, type="prob")
  return(cbind(pr.y1_ct1 = pred.trt$"1", pr.y1_ct0 = pred.ctrl$"1"))
}

# ----------------------------------------------------------------------------------------------------------------------------

#' Training a model according to the "Dummy Treatment Approach"
#' The underlying techniques rely on the 'caret'-package.
#' Two main parameters, method and trControl, are passed to the 'train'-function of Caret.
#' 
#' @source "The True Lift Model" (Lo, 2002)
#'
#' @param X A data frame of predictors.
#' @param y A binary response (numeric) vector.
#' @param ct A binary response (numeric) representing the treatment assignment (coded as 0/1).
#' @param method A string specifying which classification or regression model to use. Possible values are found using names(getModelInfo()). See http://topepo.github.io/caret/train-models-by-tag.html.
#' @param trControl A list of values that define how this function acts. See trainControl and http://topepo.github.io/caret/using-your-own-model-in-train.html. (NOTE: If given, this argument must be named.)
#' 
#' @return A model
dta <- function(x, y, ct, method = "glm", trControl = trainControl(method = "none"), ...){
  rownames(x) <- 1:nrow(x)
  
  if (!is.factor(y)) {y <- factor(y)}
  
  # Create interaction variables
  xt <- x * ct
  colnames(xt) <- paste("Int", colnames(xt), sep = "_")
  
  # Building our dataframe with the interaction variables
  df <- cbind(x, ct, y, xt)
  
  fml <- as.formula(paste("y ~ ct + ", paste(c(colnames(x), colnames(xt)), collapse = "+")))
  
  require(caret)
  mdl <- train(fml, data = df, method=method, trControl=trControl, ...)
  
  return(mdl)
}

#' Predictions according to the "Dummy Treatment Approach"
#' The underlying techniques rely on the 'caret'-package.
#' 
#' @source "The True Lift Model" (Lo, 2002)
#'
#' @param object A model.
#' @param newdata A data frame containing the values at which predictions are required.
#' 
#' @return A dataframe with predictions for when the instances are treated and for when they are not treated.
predict.dta <- function(object, newdata, y.name = "y", ct.name ="ct"){
  rownames(newdata) <- 1:nrow(newdata)
  
  predictors <- names(newdata)[(names(newdata) != y.name) & (names(newdata) != ct.name)]
  
  xt.trt <- newdata[, predictors] * 1
  colnames(xt.trt) <- paste("Int", colnames(xt.trt), sep = "_")
  df.trt <- cbind(newdata, ct=1, xt.trt)  
  
  xt.ctrl <- newdata[, predictors] * 0
  colnames(xt.ctrl) <- paste("Int", colnames(xt.ctrl), sep = "_")
  df.ctrl <- cbind(newdata, ct=0, xt.ctrl)
  
  pred.trt <- predict(object, df.trt, type="prob")
  pred.ctrl <- predict(object, df.ctrl, type="prob")
  
  return(cbind(pr.y1_ct1 = pred.trt$"1", pr.y1_ct0 = pred.ctrl$"1"))
}

# ----------------------------------------------------------------------------------------------------------------------------

#' Training a model according to "Lai's Approach"
#' The underlying techniques rely on the 'caret'-package.
#' Two main parameters, method and trControl, are passed to the 'train'-function of Caret.
#' 
#' @source "Influential Marketing" (Lai, 2006) and "Mining Truly Responsive Customers Using True Lift Overview" (Kane, 2014)
#'
#' @param X A data frame of predictors.
#' @param y A binary response (numeric) vector.
#' @param ct A binary response (numeric) representing the treatment assignment (coded as 0/1).
#' @param method A string specifying which classification or regression model to use. Possible values are found using names(getModelInfo()). See http://topepo.github.io/caret/train-models-by-tag.html. This should be a method that can handle a multinominal class variable.
#' @param trControl A list of values that define how this function acts. See trainControl and http://topepo.github.io/caret/using-your-own-model-in-train.html. (NOTE: If given, this argument must be named.)
#' 
#' @return A model
lai <- function(x, y, ct, method="gbm", trControl=trainControl(method = "none"), ...){
  rownames(x) <- 1:nrow(x)
  if (!is.factor(y)) {y <- factor(y)}
  
  df <- cbind(x, ct_y = NA)
  
  df[,"ct_y"][y == 1 & ct == 1] <- "TR" # Treated responders
  df[,"ct_y"][y == 0 & ct == 1] <- "TN" # Treated non-responders
  df[,"ct_y"][y == 1 & ct == 0] <- "CR" # Control responders
  df[,"ct_y"][y == 0 & ct == 0] <- "CN" # Control non-responders
  
  fml <- as.formula(paste("ct_y ~ ", paste(colnames(x), collapse = "+")))
  
  require(caret)
  mdl <- train(fml, data = df, method=method, trControl=trControl, ...)
  
  return(mdl)
}

#' Predictions according to "Lai's Approach"
#' The underlying techniques rely on the 'caret'-package.
#' 
#' @source "Influential Marketing" (Lai, 2006) and "Mining Truly Responsive Customers Using True Lift Overview" (Kane, 2014)
#'
#' @param object A model.
#' @param newdata A data frame containing the values at which predictions are required.
#' @param y.nale The variable-name of the target / class variable.
#' @param ct.name The variable-name of the treatment variable.
#' @param generalized Indicates whether a generalized Lai approach should be used or not.
#' 
#' @return A dataframe with predictions for when the instances are treated and for when they are not treated.
predict.lai <- function(object, newdata, y.name = "y", ct.name = "ct", generalized = TRUE){
  prob.C <- prop.table(table(newdata[,ct.name]))[1] # Control
  prob.T <- prop.table(table(newdata[,ct.name]))[2] # Treatment
  
  newdata[,"ct_y"] <- NA
  newdata[,"ct_y"][newdata[,y.name] == 1 & newdata[,ct.name] == 1] <- "TR" # Treated responders
  newdata[,"ct_y"][newdata[,y.name] == 0 & newdata[,ct.name] == 1] <- "TN" # Treated non-responders
  newdata[,"ct_y"][newdata[,y.name] == 1 & newdata[,ct.name] == 0] <- "CR" # Control responders
  newdata[,"ct_y"][newdata[,y.name] == 0 & newdata[,ct.name] == 0] <- "CN" # Control non-responders
  
  pred <- predict(object, newdata, type="prob")

  if(generalized){
    res <- cbind(pr.y1_ct1 = ((pred$TR / prob.T) + (pred$CN / prob.C)), pr.y1_ct0 = ((pred$TN / prob.T) + (pred$CR / prob.C)))
  } else {
    res <- cbind(pr.y1_ct1 = pred$TR + pred$CN, pr.y1_ct0 = pred$TN + pred$CR)
  }
    
  return(res)
}

# ----------------------------------------------------------------------------------------------------------------------------

#' Training a model according to "Pessimistic Uplift Modeling"
#' The underlying techniques rely on the 'caret'-package.
#' Two main parameters, method and trControl, are passed to the 'train'-function of Caret.
#' 
#' @source "Pessimistic Uplift Modeling" (Shaar, 2016)
#'
#' @param X A data frame of predictors.
#' @param y A binary response (numeric) vector.
#' @param ct A binary response (numeric) representing the treatment assignment (coded as 0/1).
#' @param method A string specifying which classification or regression model to use. Possible values are found using names(getModelInfo()). See http://topepo.github.io/caret/train-models-by-tag.html. This should be a method that can handle a multinominal class variable.
#' @param trControl A list of values that define how this function acts. See trainControl and http://topepo.github.io/caret/using-your-own-model-in-train.html. (NOTE: If given, this argument must be named.)
#' 
#' @return A list of three models. One according to Lai's approach. One for the responders, one for the nonresponders.
pessimistic <- function(x,y,ct, method = "glm", trControl=trainControl(method = "none"), ...){
  rownames(x) <- 1:nrow(x)
  if (!is.factor(ct)) {ct <- factor(ct)}
  
  df <- cbind(x, ct, ct_y = NA)
  df[,"ct_y"][y == 1 & ct == 1] <- "POS" # Treated responders
  df[,"ct_y"][y == 0 & ct == 1] <- "NEG" # Treated non-responders
  df[,"ct_y"][y == 1 & ct == 0] <- "NEG" # Control responders
  df[,"ct_y"][y == 0 & ct == 0] <- "POS" # Control non-responders
  
  require(caret)
  fml <- as.formula(paste("ct_y ~ ", paste(colnames(x), collapse = "+"))) # Formula without ct-variable!
  mdl.lai <- train(fml, data = df, method=method, trControl=trControl, ...) # Lai's method
  
  indices.y1 <- which(y == 1)
  fml <- as.formula(paste("ct ~ ", paste(colnames(x), collapse = "+")))
  mdl.y1 <- train(fml, data = df[indices.y1,], method=method, trControl=trControl, ...)
  mdl.y0 <- train(fml, data = df[-indices.y1,], method=method, trControl=trControl, ...)
  
  res <- list(mdl.lai = mdl.lai,
              mdl.y1 = mdl.y1,
              mdl.y0 = mdl.y0,
              method = method)
  
  return(res)
}

#' Predictions according to "Pessimistic Uplift Modeling"
#' The underlying techniques rely on the 'caret'-package.
#' 
#' @source "Pessimistic Uplift Modeling" (Shaar, 2016)
#'
#' @param object A model.
#' @param newdata A data frame containing the values at which predictions are required.
#' @param y.nale The variable-name of the target / class variable.
#' @param ct.name The variable-name of the treatment variable.
#' 
#' @return A dataframe with predictions for when the instances are treated and for when they are not treated.
predict.pessimistic <- function(object, newdata, y.name = "y", ct.name = "ct"){
  newdata[,"ct_y"] <- NA
  newdata[,"ct_y"][newdata[,y.name] == 1 & newdata[,ct.name] == 1] <- "POS" # Treated responders
  newdata[,"ct_y"][newdata[,y.name] == 0 & newdata[,ct.name] == 1] <- "NEG" # Treated non-responders
  newdata[,"ct_y"][newdata[,y.name] == 1 & newdata[,ct.name] == 0] <- "NEG" # Control responders
  newdata[,"ct_y"][newdata[,y.name] == 0 & newdata[,ct.name] == 0] <- "POS" # Control non-responders
  
  pred.lai <- predict(object$mdl.lai, newdata, type="prob")
  pred.lai$POS <- pred.lai$POS * prop.table(table(newdata[,"ct_y"]))["POS"]
  pred.lai$NEG <- pred.lai$NEG * prop.table(table(newdata[,"ct_y"]))["NEG"]
  
  pred.responders <- predict(object$mdl.y1, newdata, type="prob")
  pred.nonresponders <- predict(object$mdl.y0, newdata, type="prob")
  
  pred.reflective = data.frame(Pos=numeric(nrow(newdata)), Neg=numeric(nrow(newdata)))
  
  pred.reflective$Pos <- pred.responders$`1` * prop.table(table(newdata[, y.name], newdata[,ct.name]))[4] +
    ((1 - pred.nonresponders$`1`) * prop.table(table(newdata[, y.name], newdata[,ct.name]))[1])
  
  pred.reflective$Neg <- pred.nonresponders$`1` * prop.table(table(newdata[, y.name], newdata[,ct.name]))[3] +
    ((1 - pred.responders$`1`) * prop.table(table(newdata[, y.name], newdata[,ct.name]))[2])
  
  predictions = data.frame(uplift.lai=numeric(nrow(newdata)),
                           uplift.reflective=numeric(nrow(newdata)),
                           uplift=numeric(nrow(newdata)))
  
  # Pessimistic Uplift = (1/2) * (Uplift Lai + Uplift Reflective)
  # Is stored in pr.y1_ct1 to be compatible with the techniques from the uplift-package.
  res <- cbind(pr.y1_ct1 = (1/2) * ((pred.lai$POS - pred.lai$NEG) + (pred.reflective$Pos - pred.reflective$Neg)), 
               pr.y1_ct0 = rep(0,nrow(newdata)),
               uplift.lai = (pred.lai$POS - pred.lai$NEG),
               uplift.reflective = (pred.reflective$Pos - pred.reflective$Neg))
  
  return(res)
}