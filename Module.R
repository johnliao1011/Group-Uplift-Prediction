#### Models
OmaMethod <- function(Data, y, treatment, interaction = FALSE){
  
  modeFormula <- as.formula(paste( y, ".", sep = "~"))
  
  # RF
  RF.model<-randomForest(modeFormula, data = Data$Train$Origin,ntree=100, mtry =3)  
  
  RF.predT1<-predict(RF.model, Data$Valid$Origin$T1, type="prob")
  RF.predT0<-predict(RF.model, Data$Valid$Origin$T0, type="prob")
  
  RF_Result <- data.frame("T1"= RF.predT1[,"1"], "T0"= RF.predT0[,"1"])
  
  print("RF model: OK")
  
  
  # DT
  DT.model<-rpart(modeFormula, data = Data$Train$Origin, method = "class")
  
  DT.predT1<-predict(DT.model, Data$Valid$Origin$T1, type="prob")
  DT.predT0<-predict(DT.model, Data$Valid$Origin$T0, type="prob")
  
  DT_Result <- data.frame("T1"= DT.predT1[,"1"], "T0"= DT.predT0[,"1"])
  
  print("DT model: OK")
  
  # KNN
  KNN.predT1 <- knn(train = Data$Train$KNN[, -which(colnames(Data$Train$KNN)=="y")], 
                    test = Data$Valid$KNN$T1, 
                    cl = Data$Train$KNN[, "y"]%>% as.character()%>% as.numeric(), 
                    k = 3, algorithm = "kd_tree", prob = TRUE)
  
  KNN.predT0 <- knn(train = Data$Train$KNN[, -which(colnames(Data$Train$KNN)== "y")], 
                    test = Data$Valid$KNN$T0, 
                    cl = Data$Train$KNN[, "y"]%>% as.character()%>% as.numeric(), 
                    k = 3, algorithm = "kd_tree", prob = TRUE)
  
  
  KNN_T1 <-  KnnPrediction(KNN.predT1)
  KNN_T0 <-  KnnPrediction(KNN.predT0)
  
  KNN_Result <- data.frame("T1"= KNN_T1[,"1"], "T0"=  KNN_T0[,"1"])
  
  print("KNN model: OK")
  
  
  # LM
  LM.model<-glm(modeFormula, data = Data$Train$Origin, family = binomial(link='logit'))                              
  
  
  Lm.predT1<-predict(LM.model, Data$Valid$Origin$T1, type="response")
  Lm.predT0<-predict(LM.model, Data$Valid$Origin$T0, type="response")
  
  Lm_Result <- data.frame("T1"= Lm.predT1, "T0"=  Lm.predT0)
  print("LM model: OK")
  
  # LASSO
  cv.fit <- cv.glmnet(x = data.matrix(Data$Train$Lasso[, -1]),                                            
                      y = data.matrix(Data$Train$Origin[ , y]), 
                      alpha = 1,
                      family = "binomial",
                      type.measure = 'mse')
  
  # find best lambda to decide final fitting model
  best.lambda <- cv.fit$lambda.1se
  
  # model summary
  cv.fit$best.vars <- coef(cv.fit, s='lambda.1se')
  
  Lasso.modelT1 <- predict(cv.fit, s = best.lambda, newx = Data$Valid$Lasso$T1[,-1], type = "response")
  Lasso.modelT0 <- predict(cv.fit, s = best.lambda, newx = Data$Valid$Lasso$T0[,-1], type = "response")
  
  Lasso_Result <- data.frame("T1"= Lasso.modelT1 %>% as.numeric(), "T0"=  Lasso.modelT0 %>% as.numeric())
  print("Lasso model: OK")
  
  MODEl <- list("Lm" =LM.model, "Lasso" =cv.fit)
  
  if(interaction == TRUE){
    # LM
    IntLm.form <- interactFormula(data = Data$Train$Origin, y = y, treatment = treatment)
    IntLm.model<-glm(IntLm.form, data = Data$Train$Origin, family = binomial(link='logit'))                              
    
    
    IntLm.predT1<-predict(IntLm.model, Data$Valid$Origin$T1, type="response")
    IntLm.predT0<-predict(IntLm.model, Data$Valid$Origin$T0, type="response")
    
    IntLm_Result <- data.frame("T1"= IntLm.predT1, "T0"=  IntLm.predT0)
    print("Interaction LM model: OK")
    
    # LASSO
    Intcv.fit <- cv.glmnet(x = data.matrix(Data$Train$IntAct[, -1]),                                            
                           y = data.matrix(Data$Train$Origin[ , y]), 
                           alpha = 1,
                           family = "binomial",
                           type.measure = 'mse')
    
    # find best lambda to decide final fitting model
    best.lambda <- Intcv.fit$lambda.1se
    
    # model summary
    Intcv.fit$best.vars <- coef(Intcv.fit, s='lambda.1se')
    
    Lasso.modelT1 <- predict(Intcv.fit, s = best.lambda, newx = Data$Valid$IntAct$T1[,-1], type = "response")
    Lasso.modelT0 <- predict(Intcv.fit, s = best.lambda, newx = Data$Valid$IntAct$T0[,-1], type = "response")
    
    IntLasso_Result <- data.frame("T1"= Lasso.modelT1 %>% as.numeric(), "T0"=  Lasso.modelT0 %>% as.numeric())
    print("Interaction Lasso model: OK")
    
    IntModel <- list("Lm" =IntLm.model, "Lasso" =Intcv.fit)
    
    model_result <- list("RF" = RF_Result, "DT" = DT_Result, "KNN" = KNN_Result, "LM" = Lm_Result, 
                         "Lasso" = Lasso_Result, "IntLm"=IntLm_Result, "IntLasso" =IntLasso_Result,
                         "y" =Data$Valid$y, "ct" = Data$Valid$ct, "model" = MODEl, "Int_model" = IntModel)
    
  }else{
    model_result <- list("RF" = RF_Result, "DT" = DT_Result, "KNN" = KNN_Result, "LM" = Lm_Result, "Lasso" = Lasso_Result, 
                         "y" =Data$Valid$y, "ct" = Data$Valid$ct, "model" = MODEl)
  }
}
TmaMethod <- function(Data, y, treatment, interaction = FALSE){
  
  modeFormula <- as.formula(paste( y, ".", sep = "~"))
  
  ### RF
  # Train T=1
  RF.modelT1<-randomForest(modeFormula, data = Data$Train$Origin$T1,ntree=100, mtry= 3)                                
  print("RF ModelT1: ok")
  
  # Train T=0
  RF.modelT0<-randomForest(modeFormula, data = Data$Train$Origin$T0,ntree=100, mtry= 3)                               
  print("RF ModelT0: ok")
  
  # Valid T=1
  RF.predT1<-predict(RF.modelT1, Data$Valid$Origin[, -which(colnames(Data$Valid$Origin)==treatment)], type="prob")
  print("RF Prediction Treat= 1 :: OK")
  
  # Valid T=0
  RF.predT0<-predict(RF.modelT0, Data$Valid$Origin[, -which(colnames(Data$Valid$Origin)==treatment)], type="prob")
  print("RF Prediction Treat= 0:: OK")
  
  RF_Result <- data.frame("T1"= RF.predT1[,"1"], "T0"= RF.predT0[,"1"])
  
  print("RF model: Finish")
  
  
  ### DT
  # Train T=1
  DT.modelT1<-rpart(modeFormula, data = Data$Train$Origin$T1, method = "class")                                        
  print("DT ModelT1: ok")
  
  # Train T=0
  DT.modelT0<-rpart(modeFormula, data = Data$Train$Origin$T0, method = "class")                                        
  print("DT ModelT0: ok")
  
  # Valid T=1
  DT.predT1<-predict(DT.modelT1, Data$Valid$Origin[, -which(colnames(Data$Valid$Origin)==treatment)], type="prob")
  print("DT Prediction Treat= 1 :: OK")
  
  # Valid T=0
  DT.predT0<-predict(DT.modelT0, Data$Valid$Origin[, -which(colnames(Data$Valid$Origin)==treatment)], type="prob")
  print("DT Prediction Treat= 0 :: OK")
  
  DT_Result <- data.frame("T1"= DT.predT1[,"1"], "T0"= DT.predT0[,"1"])
  
  print("DT model: Finish")
  
  ### KNN
  
  KNN.predT1 <- knn(train = Data$Train$KNN$T1[, -which(colnames(Data$Train$KNN$T1)=="y")], 
                    test = Data$Valid$KNN, 
                    cl = Data$Train$KNN$T1[, "y"]%>% as.character()%>% as.numeric(), 
                    k = 3, algorithm = "kd_tree", prob = TRUE)
  
  print("KNN Prediction Treat= 1 :: OK")
  
  
  KNN.predT0 <- knn(train = Data$Train$KNN$T0[, -which(colnames(Data$Train$KNN$T0)== "y")], 
                    test = Data$Valid$KNN, 
                    cl = Data$Train$KNN$T0[, "y"]%>% as.character()%>% as.numeric(), 
                    k = 3, algorithm = "kd_tree", prob = TRUE)
  
  print("KNN Prediction Treat= 0 :: OK")
  
  KNN_T1 <-  KnnPrediction(KNN.predT1)
  KNN_T0 <-  KnnPrediction(KNN.predT0)
  
  KNN_Result <- data.frame("T1"= KNN_T1[,"1"], "T0"= KNN_T0[,"1"])
  
  print("KNN model: Finish")
  
  ### LM
  # Train T=1
  LM.modelT1<-glm(modeFormula, data = Data$Train$Origin$T1, family = binomial(link='logit'))                   
  print("LM ModelT1: ok")
  
  # Train T=0
  LM.modelT0<-glm(modeFormula, data = Data$Train$Origin$T0, family = binomial(link='logit'))                   
  print("LM ModelT0: ok")
  
  # Valid T=1
  Lm.predT1<-predict(LM.modelT1, Data$Valid$Origin, type="response")
  print("LM Prediction Treat= 1 :: OK")
  
  # Valid T=0
  Lm.predT0<-predict(LM.modelT0, Data$Valid$Origin, type="response")
  print("LM Prediction Treat= 0 :: OK")
  
  Lm_Result <- data.frame("T1"= Lm.predT1, "T0"= Lm.predT0)
  
  print("LM model: Finish")
  
  ### Lasso
  # Train T=1
  cv.fitT1 <- cv.glmnet(x = data.matrix(Data$Train$Lasso$T1),                                                  # Lasso
                        y = data.matrix(Data$Train$Origin$T1[,y]), 
                        alpha = 1,
                        family = "binomial",
                        type.measure = 'mse')
  print("Lasso ModelT1: ok")
  
  
  # Train T=0
  cv.fitT0 <- cv.glmnet(x = data.matrix(Data$Train$Lasso$T0),                                                  # Lasso
                        y = data.matrix(Data$Train$Origin$T0[,y]), 
                        alpha = 1,
                        family = "binomial",
                        type.measure = 'mse')
  print("Lasso ModelT0: ok")
  
  # find best lambda to decide final fitting model
  best.lambdaT1 <- cv.fitT1$lambda.1se
  best.lambdaT0 <- cv.fitT0$lambda.1se
  
  # model summary
  cv.fitT1$best.varsT1 <- coef(cv.fitT1, s='lambda.1se')
  cv.fitT0$best.varsT0 <- coef(cv.fitT0, s='lambda.1se')
  
  # Valid T=1
  Lasso.modelT1 <- predict(cv.fitT1, s = best.lambdaT1, newx = data.matrix(Data$Valid$Lasso), type = "response")
  print("Lasso Prediction Treat= 1 :: OK")
  
  # Valid T=0
  Lasso.modelT0 <- predict(cv.fitT0, s = best.lambdaT0, newx = data.matrix(Data$Valid$Lasso), type = "response")
  print("Lasso Prediction Treat= 0 :: OK")
  
  Lasso_Result <- data.frame("T1"= Lasso.modelT1 %>% as.numeric(), "T0"= Lasso.modelT0 %>% as.numeric())
  
  print("Lasso model: Finish")
  
  
  Method <- list("Lm" =NULL, "Lasso" =NULL)
  MODEl <- list("T1" =Method,"T0" =Method)
  MODEl$T1$Lm <- LM.modelT1
  MODEl$T0$Lm <- LM.modelT0
  
  MODEl$T1$Lasso <- cv.fitT1
  MODEl$T0$Lasso <- cv.fitT0
  
  if(interaction == TRUE){
    
    ### LM
    # Train T=1
    IntLm.modelT1<-glm(y~., data = Data$Train$IntAct$T1, family = binomial(link='logit'))                   
    print("Interaction LM ModelT1: ok")
    
    # Train T=0
    IntLm.modelT0<-glm(y~., data = Data$Train$IntAct$T0, family = binomial(link='logit'))                   
    print("Interaction LM ModelT0: ok")
    
    # Valid T=1
    IntLm.predT1<-predict(IntLm.modelT1, Data$Valid$IntAct, type="response")
    print("Interaction LM Prediction Treat= 1 :: OK")
    
    # Valid T=0
    IntLm.predT0<-predict(IntLm.modelT0, Data$Valid$IntAct, type="response")
    print("Interaction LM Prediction Treat= 0 :: OK")
    
    IntLm_Result <- data.frame("T1"= IntLm.predT1, "T0"= IntLm.predT0)
    
    print("Interaction LM model: Finish")
    
    ### Lasso
    # Train T=1
    Intcv.fitT1 <- cv.glmnet(x = data.matrix(subset(Data$Train$IntAct$T1, select = -y)),                                                  # Lasso
                             y = data.matrix(Data$Train$Origin$T1[,y]), 
                             alpha = 1,
                             family = "binomial",
                             type.measure = 'mse')
    print("Interaction Lasso ModelT1: ok")
    
    
    
    # Train T=0
    Intcv.fitT0 <- cv.glmnet(x = data.matrix(subset(Data$Train$IntAct$T0, select= -y)),                                                  # Lasso
                             y = data.matrix(Data$Train$Origin$T0[,y]), 
                             alpha = 1,
                             family = "binomial",
                             type.measure = 'mse')
    print("Interaction Lasso ModelT0: ok")
    
    # find best lambda to decide final fitting model
    Intbest.lambdaT1 <- Intcv.fitT1$lambda.1se
    Intbest.lambdaT0 <- Intcv.fitT0$lambda.1se
    
    # model summary
    Intcv.fitT1$Intbest.lambdaT1 <- coef(Intcv.fitT1, s='lambda.1se')
    Intcv.fitT0$Intbest.lambdaT0 <- coef(Intcv.fitT0, s='lambda.1se')
    
    # Valid T=1
    Lasso.modelT1 <- predict(Intcv.fitT1, s = Intbest.lambdaT1, newx = data.matrix(subset(Data$Valid$IntAct, select = -y)), type = "response")
    print("Interaction Lasso Prediction Treat= 1 :: OK")
    
    # Valid T=0
    Lasso.modelT0 <- predict(Intcv.fitT0, s = Intbest.lambdaT0, newx = data.matrix(subset(Data$Valid$IntAct, select =-y)), type = "response")
    print("Interaction Lasso Prediction Treat= 0 :: OK")
    
    IntLasso_Result <- data.frame("T1"= Lasso.modelT1 %>% as.numeric(), "T0"= Lasso.modelT0 %>% as.numeric())
    
    print(" Interaction Lasso model: Finish")
    
    # model collection
    
    # Interaction
    IntMethod <- list("Lm" =NULL, "Lasso" =NULL)
    IntModel <- list("T1" =IntMethod,"T0" =IntMethod)
    IntModel$T1$Lm <- IntLm.modelT1
    IntModel$T0$Lm <- IntLm.modelT0
    
    IntModel$T1$Lasso <- Intcv.fitT1
    IntModel$T0$Lasso <- Intcv.fitT0
    
    model_result <- list("RF" = RF_Result, "DT" = DT_Result, "KNN" = KNN_Result, "LM" = Lm_Result, 
                         "Lasso" = Lasso_Result, "IntLm"=IntLm_Result, "IntLasso" = IntLasso_Result,
                         "y" =Data$Valid$y, "ct" = Data$Valid$ct, "model" = MODEl, "Int_model" = IntModel)
    
  }else{
    
    model_result <- list("RF" = RF_Result, "DT" = DT_Result, "KNN" = KNN_Result, "LM" = Lm_Result, "Lasso" = Lasso_Result, 
                         "y" =Data$Valid$y, "ct" = Data$Valid$ct, "model" = MODEl)
  }
  
}
UpliftMethod <- function(train, valid, y, treatment){
  
  train[,y] <- as.numeric(as.character(train[,y]))
  train[,treatment] <- as.numeric(as.character(train[,treatment]))
  
  valid[,y] <- as.numeric(as.character(valid[,y]))
  valid[,treatment] <- as.numeric(as.character(valid[,treatment]))
  
  treatVariable <-  paste0("trt(", paste0(treatment, ")"))
  modeFormula <- as.formula(paste(y,
                                  paste(c(colnames(train[,-which(colnames(train)%in% c(y, treatment))]), 
                                          treatVariable),collapse = "+"),
                                  sep = "~"))
  # RF
  upRF.model<-upliftRF(modeFormula, data = train, mtry=3, ntree=100, split_method="ED", minsplit=200, verbose=TRUE)  
  upRF.pred<-predict(upRF.model, newdata=valid)
  RF_Result <- data.frame("T1"= upRF.pred[,1], "T0"= upRF.pred[,2])
  
  print("UpliftRF model: Finish")
  
  # DT
  upDT.model<-upliftRF(modeFormula, data = train, mtry=ncol(train)-2, ntree=1, bag.fraction = 1, split_method="ED", minsplit=200, verbose=TRUE)
  upDT.pred<-predict(upDT.model, newdata=valid)
  DT_Result <- data.frame("T1"= upDT.pred[,1], "T0"= upDT.pred[,2])
  
  print("UpliftDT model: Finish")
  
  # KNN
  upKNN.model<-upliftKNN(train = train, test =valid, y = train[,y], ct = train[ ,treatment], dist.method = "euclidean", k = 3, agg.method = "majority" )
  KNN_Result <- data.frame("T1"= upKNN.model[,2], "T0"= upKNN.model[,1])
  
  print("UpliftKNN model: Finish")
  
  
  model_result <- list("RF" = RF_Result, "DT" = DT_Result, "KNN" = KNN_Result, 
                       "y" =valid[,y], "ct" = valid[,treatment])
}


#### Modules
SepOmaModule <- function(Train_Ypre1, Train_Ypre0, Valid_Ypre1, Valid_Ypre0, TargetVar, TreatVar, Interaction){
  # transform data
  Data_Ypre1 <- DataTransform(trainData =Train_Ypre1, validData = Valid_Ypre1, OneModel = TRUE, y = TargetVar, treatment = TreatVar,interaction = Interaction)
  Data_Ypre0 <- DataTransform(trainData =Train_Ypre0, validData = Valid_Ypre0, OneModel = TRUE, y = TargetVar, treatment = TreatVar,interaction = Interaction)
  
  # predict separately
  ModelYpre1 <- OmaMethod(Data_Ypre1, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  ModelYpre0 <- OmaMethod(Data_Ypre0, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  
  
  # gathering the result
  
  RF.result <- SepPerform(ResYpre1 = ModelYpre1$RF, ResYpre0 = ModelYpre0$RF,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  DT.result <- SepPerform(ResYpre1 = ModelYpre1$DT, ResYpre0 = ModelYpre0$DT,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  
  KNN.result <- SepPerform(ResYpre1 = ModelYpre1$KNN, ResYpre0 = ModelYpre0$KNN,
                           TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                           TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  LM.result <- SepPerform(ResYpre1 = ModelYpre1$LM, ResYpre0 = ModelYpre0$LM,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  Lasso.result <- SepPerform(ResYpre1 = ModelYpre1$Lasso, ResYpre0 = ModelYpre0$Lasso,
                             TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                             TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  # collection models without interaction
  MODEl <- list("Ypre1" =NULL,"Ypre0" =NULL)
  MODEl$Ypre1 <- ModelYpre1$model
  MODEl$Ypre0 <- ModelYpre0$model
  
  
  if (Interaction == TRUE) {
    IntLm.result <- SepPerform(ResYpre1 = ModelYpre1$IntLm, ResYpre0 = ModelYpre0$IntLm,
                               TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                               TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
    
    
    IntLasso.result <- SepPerform(ResYpre1 = ModelYpre1$IntLasso, ResYpre0 = ModelYpre0$IntLasso,
                                  TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                                  TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
    
    
    
    # collect models with interaction
    Int_model <- MODEl
    Int_model$Ypre1 <- ModelYpre1$Int_model
    Int_model$Ypre0 <- ModelYpre0$Int_model
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "IntLm" = IntLm.result[,"gain_ratio"],
                   "IntLasso" = IntLasso.result[,"gain_ratio"],
                   "model" = MODEl, "Int_model" = Int_model)
    
  }else{
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "model" = MODEl)
  }
  
  return(Result)
}
SepTmaModule <- function(Train_Ypre1, Train_Ypre0, Valid_Ypre1, Valid_Ypre0, TargetVar, TreatVar, Interaction){
  # transform data
  Data_Ypre1 <- DataTransform(trainData =Train_Ypre1, validData = Valid_Ypre1, OneModel = FALSE, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  Data_Ypre0 <- DataTransform(trainData =Train_Ypre0, validData = Valid_Ypre0, OneModel = FALSE, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  
  # predict separately
  ModelYpre1 <- TmaMethod(Data_Ypre1, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  ModelYpre0 <- TmaMethod(Data_Ypre0, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  
  
  # gathering the result
  
  RF.result <- SepPerform(ResYpre1 = ModelYpre1$RF, ResYpre0 = ModelYpre0$RF,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  DT.result <- SepPerform(ResYpre1 = ModelYpre1$DT, ResYpre0 = ModelYpre0$DT,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  
  KNN.result <- SepPerform(ResYpre1 = ModelYpre1$KNN, ResYpre0 = ModelYpre0$KNN,
                           TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                           TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  LM.result <- SepPerform(ResYpre1 = ModelYpre1$LM, ResYpre0 = ModelYpre0$LM,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  Lasso.result <- SepPerform(ResYpre1 = ModelYpre1$Lasso, ResYpre0 = ModelYpre0$Lasso,
                             TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                             TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  MODEl <- list("Ypre1" =NULL,"Ypre0" =NULL)
  MODEl$Ypre1 <- ModelYpre1$model
  MODEl$Ypre0 <- ModelYpre0$model
  
  
  # collection models without interaction
  MODEl <- list("Ypre1" =NULL,"Ypre0" =NULL)
  MODEl$Ypre1 <- ModelYpre1$model
  MODEl$Ypre0 <- ModelYpre0$model
  
  
  if (Interaction == TRUE) {
    IntLm.result <- SepPerform(ResYpre1 = ModelYpre1$IntLm, ResYpre0 = ModelYpre0$IntLm,
                               TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                               TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
    
    
    IntLasso.result <- SepPerform(ResYpre1 = ModelYpre1$IntLasso, ResYpre0 = ModelYpre0$IntLasso,
                                  TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                                  TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
    
    
    
    # collect models with interaction
    Int_model <- MODEl
    Int_model$Ypre1 <- ModelYpre1$Int_model
    Int_model$Ypre0 <- ModelYpre0$Int_model
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "IntLm" = IntLm.result[,"gain_ratio"],
                   "IntLasso" = IntLasso.result[,"gain_ratio"],
                   "model" = MODEl, "Int_model" = Int_model)
    
  }else{
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "model" = MODEl)
  }
  return(Result)
}
SepUpliftModule <- function(Train_Ypre1, Train_Ypre0, Valid_Ypre1, Valid_Ypre0, TargetVar, TreatVar){
  
  # predict separately
  ModelYpre1 <- UpliftMethod(train = Train_Ypre1, valid = Valid_Ypre1, y = TargetVar, treatment = TreatVar)
  ModelYpre0 <- UpliftMethod(train = Train_Ypre0, valid = Valid_Ypre0, y = TargetVar, treatment = TreatVar)
  
  # gathering the result
  
  RF.result <- SepPerform(ResYpre1 = ModelYpre1$RF, ResYpre0 = ModelYpre0$RF,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  DT.result <- SepPerform(ResYpre1 = ModelYpre1$DT, ResYpre0 = ModelYpre0$DT,
                          TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                          TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  
  KNN.result <- SepPerform(ResYpre1 = ModelYpre1$KNN, ResYpre0 = ModelYpre0$KNN,
                           TargetVarYpre1 = ModelYpre1$y, TargetVarYpre0 = ModelYpre0$y,
                           TreatVarYpre1 = ModelYpre1$ct, TreatVarYpre0 = ModelYpre0$ct)
  
  
  Result <- list("RF" = RF.result[, "gain_ratio"],
                 "DT" = DT.result[, "gain_ratio"],
                 "KNN" = KNN.result[, "gain_ratio"])
  
  return(Result)
}


OmaModule <- function(TrainData, ValidData, TargetVar, TreatVar, Interaction){
  # transform data
  train <- NULL
  valid <- NULL
  
  train <- TrainData
  valid <- ValidData
  
  Data_Collect <- DataTransform(trainData =train, validData = valid, OneModel = TRUE, y = TargetVar, treatment = TreatVar,interaction = Interaction)
  
  # predict separately
  Model <- OmaMethod(Data_Collect, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  
  # gathering the result
  
  RF.result <- performance(pr.y1_ct1 = Model$RF$T1, pr.y1_ct0 = Model$RF$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  DT.result <- performance(pr.y1_ct1 = Model$DT$T1, pr.y1_ct0 = Model$DT$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  KNN.result <- performance(pr.y1_ct1 = Model$KNN$T1, pr.y1_ct0 = Model$KNN$T0, 
                            y = Model$y %>% as.character()%>% as.numeric(), 
                            ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  LM.result <- performance(pr.y1_ct1 = Model$LM$T1, pr.y1_ct0 = Model$LM$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  Lasso.result <- performance(pr.y1_ct1 = Model$Lasso$T1, pr.y1_ct0 = Model$Lasso$T0, 
                              y = Model$y %>% as.character()%>% as.numeric(), 
                              ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  if(Interaction == TRUE){
    IntLm.result <- performance(pr.y1_ct1 = Model$IntLm$T1, pr.y1_ct0 = Model$IntLm$T0, 
                                y = Model$y %>% as.character()%>% as.numeric(), 
                                ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
    
    IntLasso.result <- performance(pr.y1_ct1 = Model$IntLasso$T1, pr.y1_ct0 = Model$IntLasso$T0, 
                                   y = Model$y %>% as.character()%>% as.numeric(), 
                                   ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
    
    
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "IntLm" = IntLm.result[,"gain_ratio"],
                   "IntLasso" = IntLasso.result[,"gain_ratio"],
                   "model" = Model$model, "Int_model" = Model$Int_model)
  }else{
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "model" = Model$model)
  }
  
  
  return(Result)
}
TmaModule <- function(TrainData, ValidData, TargetVar, TreatVar, Interaction){
  # transform data
  Data_Collect <- DataTransform(trainData =TrainData, validData = ValidData, OneModel = FALSE, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  
  # predict separately
  Model <- TmaMethod(Data_Collect, y = TargetVar, treatment = TreatVar, interaction = Interaction)
  
  
  # gathering the result
  
  RF.result <- performance(pr.y1_ct1 = Model$RF$T1, pr.y1_ct0 = Model$RF$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  DT.result <- performance(pr.y1_ct1 = Model$DT$T1, pr.y1_ct0 = Model$DT$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  KNN.result <- performance(pr.y1_ct1 = Model$KNN$T1, pr.y1_ct0 = Model$KNN$T0, 
                            y = Model$y %>% as.character()%>% as.numeric(), 
                            ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  LM.result <- performance(pr.y1_ct1 = Model$LM$T1, pr.y1_ct0 = Model$LM$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  Lasso.result <- performance(pr.y1_ct1 = Model$Lasso$T1, pr.y1_ct0 = Model$Lasso$T0, 
                              y = Model$y %>% as.character()%>% as.numeric(), 
                              ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  if(Interaction == TRUE){
    IntLm.result <- performance(pr.y1_ct1 = Model$IntLm$T1, pr.y1_ct0 = Model$IntLm$T0, 
                                y = Model$y %>% as.character()%>% as.numeric(), 
                                ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
    
    IntLasso.result <- performance(pr.y1_ct1 = Model$IntLasso$T1, pr.y1_ct0 = Model$IntLasso$T0, 
                                   y = Model$y %>% as.character()%>% as.numeric(), 
                                   ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
    
    
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "IntLm" = IntLm.result[,"gain_ratio"],
                   "IntLasso" = IntLasso.result[,"gain_ratio"],
                   "model" = Model$model, "Int_model" = Model$Int_model)
  }else{
    
    Result <- list("RF" = RF.result[, "gain_ratio"],
                   "DT" = DT.result[, "gain_ratio"],
                   "KNN" = KNN.result[,"gain_ratio"],
                   "LM" = LM.result[, "gain_ratio"],
                   "Lasso" = Lasso.result[,"gain_ratio"],
                   "model" = Model$model)
  }
  
  
  
  return(Result)
}
UpliftModule <- function(TrainData, ValidData, TargetVar, TreatVar){
  
  # predict separately
  Model <- UpliftMethod(train = TrainData, valid = ValidData, y = TargetVar, treatment = TreatVar)
  
  # gathering the result
  
  RF.result <- performance(pr.y1_ct1 = Model$RF$T1, pr.y1_ct0 = Model$RF$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  DT.result <- performance(pr.y1_ct1 = Model$DT$T1, pr.y1_ct0 = Model$DT$T0, 
                           y = Model$y %>% as.character()%>% as.numeric(), 
                           ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  KNN.result <- performance(pr.y1_ct1 = Model$KNN$T1, pr.y1_ct0 = Model$KNN$T0, 
                            y = Model$y %>% as.character()%>% as.numeric(), 
                            ct = Model$ct %>% as.character()%>% as.numeric(), direction = 1) %>% upliftDecile()
  
  
  
  Result <- list("RF" = RF.result[, "gain_ratio"],
                 "DT" = DT.result[, "gain_ratio"],
                 "KNN" = KNN.result[, "gain_ratio"])
  
  return(Result)
}


#data <- voter.1.1.1.1
#index <- resample.voter.1.1.1.1

#train_dt <- data[index,]
#valid_dt <- data[-index,]

#a <- DataTransform(trainData = train_dt, validData = valid_dt, OneModel = TRUE, y = "MOVED_AD_NUM", treatment = "MESSAGE_A", interaction = TRUE)
#b <- UpliftMethod(train = train_dt, valid = valid_dt, y = "MOVED_AD_NUM", treatment = "MESSAGE_A")
#c <- OmaModule(TrainData = train_dt, ValidData = valid_dt, TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A", Interaction = TRUE)
