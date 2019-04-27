
Group_SepUplift <- function(train, valid, TargetVar, TreatVar, Ypre){
  # separte Ypre =1/0
  trainYpre1 <- subset(train, train[, Ypre] ==1)
  trainYpre0 <- subset(train, train[, Ypre] ==0)
  
  validYpre1 <- subset(valid, valid[, Ypre] ==1)
  validYpre0 <- subset(valid, valid[, Ypre] ==0)
  
  # drop out the Ypre 
  # (for lm and lasso can not input the predictor which only has one level factor)
  # (for dt and rf will automatically drop out the predictor)
  trainYpre1 <- trainYpre1[, -which(colnames(trainYpre1)== Ypre)]
  trainYpre0 <- trainYpre0[, -which(colnames(trainYpre0)== Ypre)]
  
  validYpre1 <- validYpre1[, -which(colnames(validYpre1)== Ypre)]
  validYpre0 <- validYpre0[, -which(colnames(validYpre0)== Ypre)]
  
  print("Ypre Separation :: OK")
  
  OmaResult <- OmaModule(Train_Ypre1 = trainYpre1, Train_Ypre0 = trainYpre0,
                         Valid_Ypre1 = validYpre1, Valid_Ypre0 = validYpre0,
                         TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A")
  
  TmaResult <- TmaModule(Train_Ypre1 = trainYpre1, Train_Ypre0 = trainYpre0,
                         Valid_Ypre1 = validYpre1, Valid_Ypre0 = validYpre0,
                         TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A")
  
  TotalResult <- list("Oma" = OmaResult, "Tma" = TmaResult)
  return(TotalResult)
  
  
}



DataTransform<- function(trainData, validData, OneModel=TRUE, y, treatment){
  
  if (OneModel==TRUE) {
    # One Model Appraoch
    #### DT, RF, Lm
    # Train 
    trainData
    
    # Validation
    # treatment=1
    valid_T1<-validData
    valid_T1[, treatment]<-1
    valid_T1[, treatment]<-factor(valid_T1[, treatment], levels = c(0,1))
    
    print("Validation For treatment= 1 :: Success")
    
    # treatment= 0
    valid_T0<-validData
    valid_T0[, treatment]<-0
    valid_T0[, treatment]<-factor(valid_T0[, treatment], levels = c(0,1))
    
    OriginDataCollect <- list("T0" =valid_T0, "T1"= valid_T1)
    
    print("Validation For treatment= 0 :: Success")
    
    
    #### KNN
    # Train
    factorCol <- names(trainData)[ sapply(trainData, is.factor) ]
    
    # normalized train data (do not normalize the type of factor)
    train_dt.norm<- trainData
    norm.value<-preProcess(trainData[, -which(colnames(trainData) %in% factorCol)], method = c("center", "scale")) 
    train_dt.norm[,-which(colnames(train_dt.norm) %in% factorCol)]<-predict(norm.value, trainData[, -which(colnames(trainData) %in% factorCol)])
    
    # create dummy
    form <- originFormula(train_dt.norm, y = y, treatment = treatment)
    TraindummyData <- model.matrix(form, train_dt.norm)
    TraindummyData <- cbind(as.data.frame(TraindummyData), y=trainData[,y])[,-1]
     
    
    # Validation
    ValidDT <- rbind(valid_T0, valid_T1)
    # normalization_validat
    valid_dt.norm <- ValidDT
    valid_dt.norm[,-which(colnames(valid_dt.norm) %in% factorCol)]<-predict(norm.value, ValidDT[,-which(colnames(ValidDT)%in% factorCol)])
    
    ValiddummyData <- model.matrix(form, valid_dt.norm)
    ValiddummyData <- as.data.frame(ValiddummyData)[,-1]
    
    
    ## treatment =1
    treatDmy <- paste0(treatment, "1")
    valid_dt.norm_T1 <- subset(ValiddummyData, ValiddummyData[,treatDmy]==1)
    
    
    print("KNN Validation For treatment= 1 :: Success")
    
    ## treatment =0
    valid_dt.norm_T0 <- subset(ValiddummyData, ValiddummyData[,treatDmy]==0)
    
    print("KNN Validation For treatment= 0 :: Success")
    
    KNNDataCollect <- list("T0" =valid_dt.norm_T0, "T1"= valid_dt.norm_T1)
    
    print("KNN Data Type Change:: Success")
    
    # Lasso/Lm preprocess
    ## Transform to dummies
    # Train
    Lm.form <- originFormula(trainData, y = y, treatment = treatment)
    
    TrainLmData <- model.matrix(Lm.form, trainData)
    
    # Valit
    ValidLmT1 <- model.matrix(Lm.form, valid_T1)
    
    print("LM/Lasso Validation For treatment= 1 :: Success")
    
    ValidLmT0 <- model.matrix(Lm.form, valid_T0)
    
    print("LM/Lasso Validation For treatment= 0 :: Success")
    
    LassoDataCollect <- list("T0" =ValidLmT0, "T1"= ValidLmT1)
    
    print("LM/Lasso Data Transformation :: Success")
    
    Train <- list("Origin" = trainData, "KNN" =TraindummyData, "Lasso"=TrainLmData)
    Valid <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "y" = validData[, y], "ct" =validData[, treatment])
    Data <- list("Train" =Train, "Valid" = Valid)
    return(Data)
  }else{
    # Two Model Approach
    
    ### DT, RF, LM
    
    # Train
    # Treatment =1
    train_T1<-trainData[ which(trainData[,treatment]==1)  ,-which(colnames(trainData)== treatment)]
    print("Training Data Treatment =1 :: OK")
    
    # Treatment =0
    train_T0<-trainData[ which(trainData[,treatment]==0)  ,-which(colnames(trainData)== treatment)]
    print("Training Data Treatment =0 :: OK")
    
    OriginDataCollect <- list("T0" =train_T0, "T1"= train_T1)
    
    # Valid
    validData
    print("Data Type Change:: Success")
    
    ### KNN
    # Train
    factorCol <- names(trainData)[ sapply(trainData, is.factor) ]
    
    # normalized train data (do not normalize the type of factor)
    train_dt.norm<- trainData
    norm.value<-preProcess(trainData[, -which(colnames(trainData) %in% factorCol)], method = c("center", "scale")) 
    train_dt.norm[,-which(colnames(train_dt.norm) %in% factorCol)]<-predict(norm.value, trainData[, -which(colnames(trainData) %in% factorCol)])
    
    valid_dt.norm<- validData
    valid_dt.norm[,-which(colnames(valid_dt.norm) %in% factorCol)]<-predict(norm.value, validData[, -which(colnames(validData) %in% factorCol)])
    
    
    # create dummy
    form <- originFormula(train_dt.norm, y = y, treatment = treatment)
    
    TraindummyData <- model.matrix(form, train_dt.norm)
    TraindummyDataWithY <- cbind(as.data.frame(TraindummyData), y=trainData[,y])[,-1]
    
    ValiddummyData <- model.matrix(form, valid_dt.norm)
    ValiddummyData <- as.data.frame(ValiddummyData)[,-1]
    
    # treatment =1
    treatDmy <- paste0(treatment, "1")
    train_dt.norm_T1 <- subset(TraindummyDataWithY, TraindummyDataWithY[,treatDmy]==1)
    
    print("KNN Training Data Treatment =1 :: OK")
    
    # treatment =0
    train_dt.norm_T0 <- subset(TraindummyDataWithY, TraindummyDataWithY[,treatDmy]==0)
    
    print("KNN Training Data Treatment =0 :: OK")
    
    KNNDataCollect <- list("T0" =train_dt.norm_T0, "T1"= train_dt.norm_T1)
    
    print("KNN Data Type Change:: Success")
    
    
    ### Lasso
    # Train
    Lm.form <- originFormula(trainData, y = y, treatment = treatment)
    
    dummyTreat <- paste0(treatment,"1")
    TrainLmData <- model.matrix(Lm.form, trainData)
    TrainLmData <- TrainLmData[,-which(colnames(TrainLmData) == "(Intercept)")]
    
    # treatment = 1
    LmTrainT1 <- TrainLmData[which(TrainLmData[,dummyTreat]==1), -which(colnames(TrainLmData)==dummyTreat)]
    
    print("Lm/Lasso Training Data Treatment =1 :: OK")
    
    # treatment = 0
    LmTrainT0 <- TrainLmData[which(TrainLmData[,dummyTreat]==0), -which(colnames(TrainLmData)==dummyTreat)]
    
    LassoDataCollect <- list("T0" =LmTrainT0, "T1"= LmTrainT1)
    
    print("Lm/Lasso Training Data Treatment =0 :: OK")
    # Valid
    
    ValidLmData <- model.matrix(Lm.form, validData)
    ValidLmData <- ValidLmData[,-which(colnames(ValidLmData) %in% c(dummyTreat, "(Intercept)"))]

    print("Lasso Data Transformation :: Success")
    
    Train <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect)
    Valid <- list("Origin" = validData, "KNN" =ValiddummyData, "Lasso"=ValidLmData, "y" = validData[, y], "ct" = validData[, treatment])
    
    Data <- list("Train" =Train, "Valid" = Valid)
    return(Data)
  }
}
SepPerform <- function(ResYpre1, ResYpre0, TargetVarYpre1, TargetVarYpre0, TreatVarYpre1, TreatVarYpre0){
  Ypre1 <- cbind(ResYpre1, y =TargetVarYpre1, ct = TreatVarYpre1)
  Ypre0 <- cbind(ResYpre0, y =TargetVarYpre0, ct = TreatVarYpre0)
  
  WholeData <- rbind(Ypre1, Ypre0)
  
  perf <- performance(WholeData$T1, WholeData$T0,
                      WholeData$y %>% as.character()%>%as.numeric(), 
                      WholeData$ct%>% as.character()%>%as.numeric(), direction = 1)
  
  
  
  result<-upliftDecile(perf)
  
}

OmaMethod <- function(Data, y, treatment){
  
  modeFormula <- as.formula(paste( y, ".", sep = "~"))
  
  # RF
  RF.model<-randomForest(modeFormula, data = Data$Train$Origin,ntree=100, mtry =3)  
  
  RF.predT1<-predict(RF.model, Data$Valid$Origin$T1, type="prob")
  RF.predT0<-predict(RF.model, Data$Valid$Origin$T0, type="prob")
  
  RF_Result <- data.frame("T1"= RF.predT1[,2], "T0"= RF.predT0[,2])
  
  print("RF model: OK")
  
  
  # DT
  DT.model<-rpart(modeFormula, data = Data$Train$Origin, method = "class")
  
  DT.predT1<-predict(DT.model, Data$Valid$Origin$T1, type="prob")
  DT.predT0<-predict(DT.model, Data$Valid$Origin$T0, type="prob")
  
  DT_Result <- data.frame("T1"= DT.predT1[,2], "T0"= DT.predT0[,2])
  
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
  
  model_result <- list("RF" = RF_Result, "DT" = DT_Result, "KNN" = KNN_Result, "LM" = Lm_Result, "Lasso" = Lasso_Result, 
                       "y" =Data$Valid$y, "ct" = Data$Valid$ct, "model" = MODEl)
}
TmaMethod <- function(Data, y, treatment){
  
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
  
  RF_Result <- data.frame("T1"= RF.predT1[,2], "T0"= RF.predT0[,2])
  
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
  
  DT_Result <- data.frame("T1"= DT.predT1[,2], "T0"= DT.predT0[,2])
  
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
  
  model_result <- list("RF" = RF_Result, "DT" = DT_Result, "KNN" = KNN_Result, "LM" = Lm_Result, "Lasso" = Lasso_Result, 
                       "y" =Data$Valid$y, "ct" = Data$Valid$ct, "model" = MODEl)
  
}

OmaModule <- function(Train_Ypre1, Train_Ypre0, Valid_Ypre1, Valid_Ypre0, TargetVar, TreatVar){
  # transform data
  Data_Ypre1 <- DataTransform(trainData =Train_Ypre1, validData = Valid_Ypre1, OneModel = TRUE, y = TargetVar, treatment = TreatVar )
  Data_Ypre0 <- DataTransform(trainData =Train_Ypre0, validData = Valid_Ypre0, OneModel = TRUE, y = TargetVar, treatment = TreatVar )
  
  # predict separately
  ModelYpre1 <- OmaMethod(Data_Ypre1, y = TargetVar, treatment = TreatVar)
  ModelYpre0 <- OmaMethod(Data_Ypre0, y = TargetVar, treatment = TreatVar)
  
  
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
  
  MODEl <- list("Ypre1" =Method,"Ypre0" =Method)
  MODEl$Ypre1 <- ModelYpre1$model
  MODEl$Ypre0 <- ModelYpre0$model
  
  
  Result <- list("RF" = RF.result[, c("gain", "gain_ratio", "gini")],
                 "DT" = DT.result[, c("gain", "gain_ratio", "gini")],
                 "KNN" = KNN.result[, c("gain", "gain_ratio", "gini")],
                 "LM" = LM.result[, c("gain", "gain_ratio", "gini")],
                 "Lasso" = Lasso.result[, c("gain", "gain_ratio", "gini")],
                 "model" = MODEl)
  
  return(Result)
}
TmaModule <- function(Train_Ypre1, Train_Ypre0, Valid_Ypre1, Valid_Ypre0, TargetVar, TreatVar){
  # transform data
  Data_Ypre1 <- DataTransform(trainData =Train_Ypre1, validData = Valid_Ypre1, OneModel = FALSE, y = TargetVar, treatment = TreatVar )
  Data_Ypre0 <- DataTransform(trainData =Train_Ypre0, validData = Valid_Ypre0, OneModel = FALSE, y = TargetVar, treatment = TreatVar )
  
  # predict separately
  ModelYpre1 <- TmaMethod(Data_Ypre1, y = TargetVar, treatment = TreatVar)
  ModelYpre0 <- TmaMethod(Data_Ypre0, y = TargetVar, treatment = TreatVar)
  
  
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
  
  MODEl <- list("Ypre1" =Method,"Ypre0" =Method)
  MODEl$Ypre1 <- ModelYpre1$model
  MODEl$Ypre0 <- ModelYpre0$model
  
  
  Result <- list("RF" = RF.result[, c("gain", "gain_ratio", "gini")],
                 "DT" = DT.result[, c("gain", "gain_ratio", "gini")],
                 "KNN" = KNN.result[, c("gain", "gain_ratio", "gini")],
                 "LM" = LM.result[, c("gain", "gain_ratio", "gini")],
                 "Lasso" = Lasso.result[, c("gain", "gain_ratio", "gini")],
                 "model" = MODEl)
  
  return(Result)
}



data <- voter.1.1.1.1
index <- resample.voter.1.1.1.1[,1]

train_dt <- data[index,]
valid_dt <- data[-index,]

#a <- Group_SepUplift(train = train_dt, valid = valid_dt, TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A", Ypre = "POLITICALC")


#Data_Preprocess <- DataTransform(trainData =train_dt, validData = valid_dt, OneModel = FALSE, y ="MOVED_AD_NUM", treatment = "MESSAGE_A" )
#a <- TmaMethod(Data = Data_Preprocess, y = "MOVED_AD_NUM", treatment = "MESSAGE_A")

#a <- OmaMethod(Data = Data_Preprocess, y = "MOVED_AD_NUM", treatment = "MESSAGE_A")

