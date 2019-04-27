# groupUplift
# Including Two Model Approach, One Model Approach, Uplift version (special split)
# function index including formula, treatment, y, data, index (training index)

group_Uplift <- function(data, index, TreatVariablet, Outcome, Interact =FALSE){
  res_gain<-matrix(nrow = 11, ncol = ncol(index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(index), vector())%>% data.frame()
  
  model_template <- list('model'=NULL, 'gainOrigin' = res_gain, 'gainRatio' = res_gain.ratio, 'gini' = res_gini)
  
  model_approach <- list('RF'= model_template,
                         'DT'= model_template,
                         'KNN'= model_template,
                         'Lm'= model_template,
                         "Lasso" = model_template)
  
  uplift_approach <- list('RF'= model_template,
                          'DT'= model_template,
                          'KNN'= model_template)
  
  model_collector <- list('Oma'=model_approach,
                          'Tma'=model_approach,
                          "Uplift"=uplift_approach)
  
  
  OmaResult <- OneModelApproach(data = data,
                              index = index,
                              treatment = TreatVariablet , y = Outcome)
  
  TmaResult <-  TwoModelApproach(data = data,
                                 index = index, 
                                 treatment = TreatVariablet, y = Outcome)
  
  UpliftResult <- UpliftSpecialApproach(data = data, 
                                        index = index, 
                                        treatment = TreatVariablet, y = Outcome)     
  
  
  model_collector$Oma <- OmaResult
  model_collector$Tma <- TmaResult
  model_collector$Uplift <- UpliftResult
  
  if(Interact == TRUE){
    IntOmaResult <- OneModelApproach(data = data,
                                     index = index,
                                     treatment = TreatVariablet , y = Outcome,interaction = TRUE)
    
    IntTmaResult <-  TwoModelApproach(data = data,
                                      index = index, 
                                      treatment = TreatVariablet, y = Outcome, interaction = TRUE)
    IntUpliftResult <-  UpliftSpecialApproach(data = data,
                                      index = index, 
                                      treatment = TreatVariablet, y = Outcome, interaction = TRUE)
    
    model_collector$Oma$Int$KNN <- IntOmaResult$KNN
    model_collector$Oma$Int$Lm <- IntOmaResult$Lm
    model_collector$Oma$Int$Lasso <- IntOmaResult$Lasso
    
    model_collector$Tma$Int$KNN <- IntTmaResult$KNN
    model_collector$Tma$Int$Lm <- IntTmaResult$Lm
    model_collector$Tma$Int$Lasso <- IntTmaResult$Lasso
    
    model_collector$Uplift$Int$KNN <- IntUpliftResult$KNN
    
  }
  
  
  return(model_collector)
}

OneModelApproach <- function(data, index, treatment, y, interaction = FALSE){
  
  for (i in c(1:ncol(index))) {
    cat(crayon::blue(sprintf('One Model Appraoch:: Boostrap %s start \n', i)))
    
    trainIdx<-index[,i]
    
    # data for y and treatment is factor (Tma and Oma adapt this)
    data[, treatment] <- factor(data[, treatment], levels = c(0,1))
    data[, y] <- factor(data[, y], levels = c(0,1))
    
    train_dtF <- data[trainIdx,]
    valid_dtF <- data[-trainIdx,]
    
    print("Data Type Change:: Success")
    
    ### KNN preprocess
    
    # create the dummy for KNN usage
    # for categorical data (no need for normalizing)
    factorCol <- names(train_dtF)[ sapply(train_dtF, is.factor) ]
    
    # normalized train data (do not normalize the type of factor)
    train_dt.norm<- train_dtF
    norm.value<-preProcess(train_dtF[, -which(colnames(train_dtF) %in% c(factorCol, y))], method = c("center", "scale")) 
    train_dt.norm[,-which(colnames(train_dt.norm) %in% c(factorCol, y))]<-predict(norm.value, train_dtF[, -which(colnames(train_dtF) %in% c(factorCol, y))])
    
    # create dummy
    if (interaction == TRUE) {
      form <- interactFormula(train_dt.norm, y = y, treatment = treatment)
      print("Interaction for KNN :: Success")
      
      dummyData <- model.matrix(form, train_dt.norm)
      dummyDataWithY <- cbind(as.data.frame(dummyData), y=train_dtF[,y])[,-1]
      
      print("KNN Data Type Change:: Success")
      
    }else{
      form <- originFormula(train_dt.norm, y = y, treatment = treatment)
      dummyData <- model.matrix(form, train_dt.norm)
      dummyDataWithY <- cbind(as.data.frame(dummyData), y=train_dtF[,y])[,-1]
      
      print("KNN Data Type Change:: Success")
    }
    
    
    dummyData <- model.matrix(form, train_dt.norm)
    dummyDataWithY <- cbind(as.data.frame(dummyData), y=train_dtF[,y])[,-1]
    
    print("KNN Data Type Change:: Success")
    
    
    ### Lasso/Lm preprocess
    # Transform to dummies
    if (interaction == TRUE) {
      Lm.form <- interactFormula(data, y = y, treatment = treatment)
      print("Interaction for Lasso/LM :: Success")
      
    }else{
      Lm.form <- originFormula(data, y = y, treatment = treatment)
    }
    
    lmData <- model.matrix(Lm.form, data)
    
    print("LM/Lasso Data Transformation :: Success")
    
    
    
    
    
    #### Training Part (One Model for each algorithm)
    modeFormula <- as.formula(paste( y, ".", sep = "~"))
    
    RF.model<-randomForest(modeFormula, data = train_dtF,ntree=100, mtry =3)                                # RF
    print("RF model: OK")
    
    DT.model<-rpart(modeFormula, data = train_dtF, method = "class")                                        # DT
    print("DT model: OK")
    
    
    
    logistTrainData <- cbind(as.data.frame(lmData[trainIdx,-1]), y=train_dtF[,y])
    
    LM.model<-glm(Lm.form, data = train_dtF, family = binomial(link='logit'))                              # LM
    print("LM model: OK")
    
    cv.fit <- cv.glmnet(x = data.matrix(lmData[trainIdx, -1]),                                             # Lasso
                        y = data.matrix(train_dtF[ , y]), 
                        alpha = 1,
                        family = "binomial",
                        type.measure = 'mse')
    
    # find best lambda to decide final fitting model
    best.lambda <- cv.fit$lambda.1se
    
    # model summary
    cv.fit$best.vars <- coef(cv.fit, s='lambda.1se')
    
    print("Lasso model: OK")
    
    
    
    ### Prediction Part (Predict under the treatment =0/1 respectively)
    
    ## Validation Data Preparation
    # treatment=1
    valid_T1<-valid_dtF
    valid_T1[, treatment]<-1
    valid_T1[, treatment]<-factor(valid_T1[, treatment], levels = c(0,1))
    
    print("Validation For treatment= 1 :: Success")
    
    # treatment= 0
    valid_T0<-valid_dtF
    valid_T0[, treatment]<-0
    valid_T0[, treatment]<-factor(valid_T0[, treatment], levels = c(0,1))
    
    print("Validation For treatment= 0 :: Success")
    
    ## Validation Data Preparation for KNN
    
    
    ValidDT <- rbind(valid_T0, valid_T1)
    # normalization_validat
    valid_dt.norm <- ValidDT
    valid_dt.norm[,-which(colnames(valid_dt.norm) %in% c(factorCol, y))]<-predict(norm.value, ValidDT[,-which(colnames(ValidDT)%in% c(factorCol, y))])
    
    ValiddummyData <- model.matrix(form, valid_dt.norm)
    ValiddummyDataWithY <- cbind(as.data.frame(ValiddummyData), y=ValidDT[,y])[,-1]
    
    
    ## treatment =1
    treatDmy <- paste0(treatment, "1")
    valid_dt.norm_T1 <- subset(ValiddummyDataWithY, ValiddummyDataWithY[,treatDmy]==1)
    
    
    print("KNN Validation For treatment= 1 :: Success")
    
    ## treatment =0
    valid_dt.norm_T0 <- subset(ValiddummyDataWithY, ValiddummyDataWithY[,treatDmy]==0)
    
    print("KNN Validation For treatment= 0 :: Success")
    
    
    ### Validation Data Preparation for LM/Lasso
    
    ValidLmT1 <- model.matrix(Lm.form, valid_T1)
    
    print("LM/Lasso Validation For treatment= 1 :: Success")
    
    ValidLmT0 <- model.matrix(Lm.form, valid_T0)
    
    print("LM/Lasso Validation For treatment= 0 :: Success")
    
    # RF
    RF.predT1<-predict(RF.model, valid_T1, type="prob")
    RF.predT0<-predict(RF.model, valid_T0, type="prob")
    
    RFperf <- performance(RF.predT1[,2], RF.predT0[,2], 
                          valid_dtF[, y]%>% as.character()%>%as.numeric(), 
                          valid_dtF[, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    
    
    RF.result<-upliftDecile(RFperf)
    
    model_approach$RF$gainOrigin[(1:length(RF.result$gain)),i] <- RF.result$gain
    model_approach$RF$gainRatio[(1:length(RF.result$gain_ratio)),i] <- RF.result$gain_ratio
    model_approach$RF$gini[(1:length(RF.result$gini)),i] <- RF.result$gini
    
    print("Oma RF :: Success")
    
    
    # DT
    DT.predT1<-predict(DT.model, valid_T1, type="prob")
    DT.predT0<-predict(DT.model, valid_T0, type="prob")
    
    DTperf <- performance(DT.predT1[,2], DT.predT0[,2], 
                          valid_dtF[, y]%>% as.character()%>%as.numeric(), 
                          valid_dtF[, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    DT.result<-upliftDecile(DTperf)
    
    model_approach$DT$gainOrigin[(1:length(DT.result$gain)),i] <- DT.result$gain
    model_approach$DT$gainRatio[(1:length(DT.result$gain_ratio)),i] <- DT.result$gain_ratio
    model_approach$DT$gini[(1:length(DT.result$gini)),i] <- DT.result$gini
    
    print("Oma DT :: Success")
    
    # KNN
    KNN.predT1 <- knn(train = dummyDataWithY[, -which(colnames(dummyDataWithY)=="y")], 
                      test = valid_dt.norm_T1[, -which(colnames(valid_dt.norm_T1)=="y")], 
                      cl = dummyDataWithY[, "y"]%>% as.character()%>% as.numeric(), 
                      k = 3, algorithm = "kd_tree", prob = TRUE)
    
    KNN.predT0 <- knn(train = dummyDataWithY[, -which(colnames(dummyDataWithY)== "y")], 
                      test = valid_dt.norm_T0[, -which(colnames(valid_dt.norm_T0)== "y")], 
                      cl = dummyDataWithY[, "y"]%>% as.character()%>% as.numeric(), 
                      k = 3, algorithm = "kd_tree", prob = TRUE)
    
    
    KNN_T1 <-  KnnPrediction(KNN.predT1)
    KNN_T0 <-  KnnPrediction(KNN.predT0)
    
    
    
    KNNperf <- performance(KNN_T1[,"1"], KNN_T0[,"1"], 
                           valid_dtF[, y]%>% as.character()%>%as.numeric(), 
                           valid_dtF[, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    KNN.result<-upliftDecile(KNNperf)
    
    
    model_approach$KNN$gainOrigin[(1:length(KNN.result$gain)),i] <- KNN.result$gain
    model_approach$KNN$gainRatio[(1:length(KNN.result$gain_ratio)),i] <- KNN.result$gain_ratio
    model_approach$KNN$gini[(1:length(KNN.result$gini)),i] <- KNN.result$gini
    
    
    print("Oma KNN :: Success")
    
    # LM
    # For logistic regression, the data need to contain the y
    Valid.dfT1 <-  cbind(as.data.frame(ValidLmT1), y= valid_dtF[,y])
    Valid.dfT0 <- cbind(as.data.frame(ValidLmT0), y= valid_dtF[,y])
    
    Lm.predT1<-predict(LM.model, valid_T1, type="response")
    Lm.predT0<-predict(LM.model, valid_T0, type="response")
    
    LMperf <- performance(Lm.predT1, Lm.predT0, 
                          valid_dtF[,y]%>% as.character()%>%as.numeric(), 
                          valid_dtF[ ,treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    LM.result<-upliftDecile(LMperf)
    
    model_approach$Lm$gainOrigin[(1:length(LM.result$gain)),i] <- LM.result$gain
    model_approach$Lm$gainRatio[(1:length(LM.result$gain_ratio)),i] <- LM.result$gain_ratio
    model_approach$Lm$gini[(1:length(LM.result$gini)),i] <- LM.result$gini
    
    print("Oma LM :: Success")
    
    # Lasso
    Lasso.modelT1 <- predict(cv.fit, s = best.lambda, newx = ValidLmT1[,-1], type = "response")
    Lasso.modelT0 <- predict(cv.fit, s = best.lambda, newx = ValidLmT0[,-1], type = "response")
    
    
    Lasso.perf <- performance(Lasso.modelT1, Lasso.modelT0, valid_dtF[,y]%>% as.character()%>%as.numeric(), valid_dtF[ ,treatment]%>% as.character()%>%as.numeric(), direction = 1)
    Lasso.result<-upliftDecile(Lasso.perf)
    
    model_approach$Lasso$gainOrigin[(1:length(Lasso.result$gain)),i] <- Lasso.result$gain
    model_approach$Lasso$gainRatio[(1:length(Lasso.result$gain_ratio)),i] <- Lasso.result$gain_ratio
    model_approach$Lasso$gini[(1:length(Lasso.result$gini)),i] <- Lasso.result$gini
    
    print("Oma Lasso :: Success")
    
    
    if(i==1){ 
      model_approach$RF$model <- RF.model
      model_approach$DT$model <- DT.model
      model_approach$KNN$model$T1 <- KNN.predT1
      model_approach$KNN$model$T0 <- KNN.predT0
      model_approach$Lm$model <- LM.model
      model_approach$Lasso$model <- cv.fit
      
    }
    
    
  }
  
  cat(crayon::red('One Model Appraoch:: Boostrap Finish \n\n'))
  
  
  return(model_approach)
  
}

TwoModelApproach <- function(data, index, treatment, y, interaction = FALSE){
  
  for (i in c(1:ncol(index))) {
    cat(crayon::blue(sprintf('Two Model Appraoch:: Boostrap %s start \n', i)))
    
    trainIdx<-index[,i]
    
    # data for y and treatment is factor (Tma and Tma adapt this)
    data[, treatment] <- factor(data[, treatment], levels = c(0,1))
    data[, y] <- factor(data[, y], levels = c(0,1))
    
    train_dtF <- data[trainIdx,]
    valid_dtF <- data[-trainIdx,]
    
    # Treatment =1
    train_T1<-train_dtF[ which(train_dtF[,treatment]==1)  ,-which(colnames(train_dtF)== treatment)]
    print("Training Data Treatment =1 :: OK")
    
    # Treatment =0
    train_T0<-train_dtF[ which(train_dtF[,treatment]==0)  ,-which(colnames(train_dtF)== treatment)]
    print("Training Data Treatment =0 :: OK")
    
    
    print("Data Type Change:: Success")
    
    ### KNN preprocess
    
    # create the dummy for KNN usage
    # for categorical data (no need for normalizing)
    factorCol <- names(train_dtF)[ sapply(train_dtF, is.factor) ]
    
    # normalized train data (do not normalize the type of factor)
    train_dt.norm<- train_dtF
    norm.value<-preProcess(train_dtF[, -which(colnames(train_dtF) %in% c(factorCol, y))], method = c("center", "scale")) 
    train_dt.norm[,-which(colnames(train_dt.norm) %in% c(factorCol, y))]<-predict(norm.value, train_dtF[, -which(colnames(train_dtF) %in% c(factorCol, y))])
    
    valid_dt.norm<- valid_dtF
    valid_dt.norm[,-which(colnames(valid_dt.norm) %in% c(factorCol, y))]<-predict(norm.value, valid_dtF[, -which(colnames(valid_dtF) %in% c(factorCol, y))])
    
    
    # create dummy
    if (interaction == TRUE) {
      form <- interactFormula(train_dt.norm, y = y, treatment = treatment)
      print("Interaction for KNN :: Success")
      
    }else{
      form <- originFormula(train_dt.norm, y = y, treatment = treatment)
    }
    
    
    TraindummyData <- model.matrix(form, train_dt.norm)
    TraindummyDataWithY <- cbind(as.data.frame(TraindummyData), y=train_dtF[,y])[,-1]
    
    ValiddummyData <- model.matrix(form, valid_dt.norm)
    ValiddummyDataWithY <- as.data.frame(ValiddummyData)[,-1]
    
    # treatment =1
    treatDmy <- paste0(treatment, "1")
    train_dt.norm_T1 <- subset(TraindummyDataWithY, TraindummyDataWithY[,treatDmy]==1)
    
    # treatment =0
    train_dt.norm_T0 <- subset(TraindummyDataWithY, TraindummyDataWithY[,treatDmy]==0)
    
    print("KNN Data Type Change:: Success")
    
    
    ### Lasso/Lm preprocess
    # Transform to dummies
    if (interaction == TRUE) {
      Lm.form <- interactFormula(data, y = y, treatment = treatment, treat_include = FALSE)
      print("Interaction for KNN :: Success")
      
      
    }else{
      Lm.form <- originFormula(data, y = y, treatment = treatment)
    }
    
    
    lmData <- model.matrix(Lm.form, data)
    
    LmFullData <-  cbind(as.data.frame(lmData), y=data[,y])
    LmTrain <- LmFullData[trainIdx,]
    dummyTreat <- paste0(treatment,"1")
    
    LmValid <- LmFullData[-trainIdx,-which(colnames(LmFullData) %in% c(dummyTreat, "(Intercept)"))]
    
    
    LmTrainT1 <- LmTrain[which(LmTrain[,dummyTreat]==1), -which(colnames(LmTrain)==dummyTreat)]
    
    print("Lm/Lasso Training Data Treatment =1 :: OK")
    
    LmTrainT0 <- LmTrain[which(LmTrain[,dummyTreat]==0), -which(colnames(LmTrain)==dummyTreat)]
    
    print("Lm/Lasso Training Data Treatment =0 :: OK")
    
    
    print("LM/Lasso Data Transformation :: Success")
    
    
    #### Training Part (Two  Model for each algorithm)
    modeFormula <- as.formula(paste( y, ".", sep = "~"))
    
    RF.modelT1<-randomForest(modeFormula, data = train_T1,ntree=100, mtry= 3)                                # RF (T=1)
    print("RF ModelT1: ok")
    
    RF.modelT0<-randomForest(modeFormula, data = train_T0,ntree=100, mtry= 3)                                # RF (T=0)
    print("RF ModelT0: ok")
    
    
    print("RF model: Finish")
    
    DT.modelT1<-rpart(modeFormula, data = train_T1, method = "class")                                        # DT (T=1)
    print("DT ModelT1: ok")
    
    DT.modelT0<-rpart(modeFormula, data = train_T0, method = "class")                                        # DT (T=0)
    print("DT ModelT0: ok")
    
    print("DT model: Finish")
    
    
    LM.modelT1<-glm(y~., data = LmTrainT1[, -which(colnames(LmTrainT1)%in% c("(Intercept)"))], family = binomial(link='logit'))                    # LM
    print("LM ModelT1: ok")
    
    LM.modelT0<-glm(y~., data = LmTrainT0[, -which(colnames(LmTrainT0)%in% c("(Intercept)"))], family = binomial(link='logit'))                    # LM
    print("LM ModelT0: ok")
    
    print("LM model: Finish")
    
    cv.fitT1 <- cv.glmnet(x = data.matrix(LmTrainT1[, -which(colnames(LmTrainT1)%in% c("(Intercept)", "y"))]),                                                  # Lasso
                          y = data.matrix(LmTrainT1[ , "y"]), 
                          alpha = 1,
                          family = "binomial",
                          type.measure = 'mse')
    print("Lasso ModelT1: ok")
    
    
    
    cv.fitT0 <- cv.glmnet(x = data.matrix(LmTrainT0[, -which(colnames(LmTrainT0)%in% c("(Intercept)", "y"))]),                                                  # Lasso
                          y = data.matrix(LmTrainT0[ , "y"]), 
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
    
    print("Lasso model: Finish")
    
    
    
    
    ### Prediction Part (Predict under the treatment =0/1 respectively)
    
    
    # RF
    RF.predT1<-predict(RF.modelT1, valid_dtF[, -which(colnames(valid_dtF)==treatment)], type="prob")
    print("RF Prediction Treat= 1 :: OK")
    
    RF.predT0<-predict(RF.modelT0, valid_dtF[, -which(colnames(valid_dtF)==treatment)], type="prob")
    print("RF Prediction Treat= 0 :: OK")
    
    RFperf <- performance(RF.predT1[,2], RF.predT0[,2], 
                          valid_dtF[, y]%>% as.character()%>%as.numeric(), 
                          data[-trainIdx, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    RF.result<-upliftDecile(RFperf)
    
    model_approach$RF$gainOrigin[(1:length(RF.result$gain)),i] <- RF.result$gain
    model_approach$RF$gainRatio[(1:length(RF.result$gain_ratio)),i] <- RF.result$gain_ratio
    model_approach$RF$gini[(1:length(RF.result$gini)),i] <- RF.result$gini
    
    print("Tma RF :: Success")
    
    
    # DT
    DT.predT1<-predict(DT.modelT1, valid_dtF[, -which(colnames(valid_dtF)==treatment)], type="prob")
    print("DT Prediction Treat= 1 :: OK")
    
    DT.predT0<-predict(DT.modelT0, valid_dtF[, -which(colnames(valid_dtF)==treatment)], type="prob")
    print("DT Prediction Treat= 0 :: OK")
    
    DTperf <- performance(DT.predT1[,2], DT.predT0[,2], 
                          valid_dtF[, y]%>% as.character()%>%as.numeric(), 
                          data[-trainIdx, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    DT.result<-upliftDecile(DTperf)
    
    model_approach$DT$gainOrigin[(1:length(DT.result$gain)),i] <- DT.result$gain
    model_approach$DT$gainRatio[(1:length(DT.result$gain_ratio)),i] <- DT.result$gain_ratio
    model_approach$DT$gini[(1:length(DT.result$gini)),i] <- DT.result$gini
    
    print("Tma DT :: Success")
    
    # KNN
    KNN.predT1 <- knn(train = train_dt.norm_T1[, -which(colnames(train_dt.norm_T1)=="y")], 
                      test = ValiddummyDataWithY, 
                      cl = train_dt.norm_T1[, "y"]%>% as.character()%>% as.numeric(), 
                      k = 3, algorithm = "kd_tree", prob = TRUE)
    
    print("KNN Prediction Treat= 1 :: OK")
    
    
    KNN.predT0 <- knn(train = train_dt.norm_T0[, -which(colnames(train_dt.norm_T0)== "y")], 
                      test = ValiddummyDataWithY, 
                      cl = train_dt.norm_T0[, "y"]%>% as.character()%>% as.numeric(), 
                      k = 3, algorithm = "kd_tree", prob = TRUE)
    
    print("KNN Prediction Treat= 0 :: OK")
    
    
    KNN_T1 <-  KnnPrediction(KNN.predT1)
    KNN_T0 <-  KnnPrediction(KNN.predT0)
    
    
    
    KNNperf <- performance(KNN_T1[,"1"], KNN_T0[,"1"], 
                           valid_dtF[, y]%>% as.character()%>%as.numeric(), 
                           valid_dtF[, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    KNN.result<-upliftDecile(KNNperf)
    
    
    model_approach$KNN$gainOrigin[(1:length(KNN.result$gain)),i] <- KNN.result$gain
    model_approach$KNN$gainRatio[(1:length(KNN.result$gain_ratio)),i] <- KNN.result$gain_ratio
    model_approach$KNN$gini[(1:length(KNN.result$gini)),i] <- KNN.result$gini
    
    
    print("Tma KNN :: Success")
    
    # LM
    # For logistic regression, the data need to contain the y
    Lm.predT1<-predict(LM.modelT1, LmValid, type="response")
    print("LM Prediction Treat= 1 :: OK")
    
    Lm.predT0<-predict(LM.modelT0, LmValid, type="response")
    print("LM Prediction Treat= 0 :: OK")
    
    LMperf <- performance(Lm.predT1, Lm.predT0, 
                          valid_dtF[,y]%>% as.character()%>%as.numeric(), 
                          data[-trainIdx, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    LM.result<-upliftDecile(LMperf)
    
    model_approach$Lm$gainOrigin[(1:length(LM.result$gain)),i] <- LM.result$gain
    model_approach$Lm$gainRatio[(1:length(LM.result$gain_ratio)),i] <- LM.result$gain_ratio
    model_approach$Lm$gini[(1:length(LM.result$gini)),i] <- LM.result$gini
    
    print("Tma LM :: Success")
    
    # Lasso
    Lasso.modelT1 <- predict(cv.fitT1, s = best.lambdaT1, newx = data.matrix(LmValid[, -which(colnames(LmValid) %in% c("(Intercept)", "y"))]), type = "response")
    print("Lasso Prediction Treat= 1 :: OK")
    
    Lasso.modelT0 <- predict(cv.fitT0, s = best.lambdaT0, newx = data.matrix(LmValid[, -which(colnames(LmValid) %in% c("(Intercept)", "y"))]), type = "response")
    print("Lasso Prediction Treat= 0 :: OK")
    
    
    Lasso.perf <- performance(Lasso.modelT1, Lasso.modelT0, 
                              valid_dtF[,y]%>% as.character()%>%as.numeric(), 
                              data[-trainIdx, treatment]%>% as.character()%>%as.numeric(), direction = 1)
    
    Lasso.result<-upliftDecile(Lasso.perf)
    
    model_approach$Lasso$gainOrigin[(1:length(Lasso.result$gain)),i] <- Lasso.result$gain
    model_approach$Lasso$gainRatio[(1:length(Lasso.result$gain_ratio)),i] <- Lasso.result$gain_ratio
    model_approach$Lasso$gini[(1:length(Lasso.result$gini)),i] <- Lasso.result$gini
    
    print("Tma Lasso :: Success")
    
    
    if(i==1){ 
      model_approach$RF$model$T1 <- RF.modelT1
      model_approach$RF$model$T0 <- RF.modelT0
      model_approach$DT$model$T1 <- DT.modelT1
      model_approach$DT$model$T0 <- DT.modelT0
      model_approach$KNN$model$T1 <- KNN.predT1
      model_approach$KNN$model$T0 <- KNN.predT0
      model_approach$Lm$model$T1 <- LM.modelT1
      model_approach$Lm$model$T0 <- LM.modelT0
      model_approach$Lasso$model$T1 <- cv.fitT1
      model_approach$Lasso$model$T0 <- cv.fitT0
      
    }
    
  }
  
  cat(crayon::red('Two Model Appraoch:: Boostrap Finish \n\n'))
  
  
  return(model_approach)
  
}

UpliftSpecialApproach <- function(data, index, treatment, y, interaction =FALSE){
  
  for (i in c(1:ncol(index))) {
    cat(crayon::blue(sprintf('Uplift Model Appraoch:: Boostrap %s start \n', i)))
    
    if (interaction == FALSE) {
      # uplift model only accept the numeric treatment and y
      data[,treatment] <- as.numeric(as.character(data[,treatment]))
      data[,y] <- as.numeric(as.character(data[,y]))
      
      trainIdx<-index[,i]
      train_dt <- data[ trainIdx,]
      valid_dt <- data[-trainIdx,]
      
      
      #### Training Part
      treatVariable <-  paste0("trt(", paste0(treatment, ")"))
      modeFormula <- as.formula(paste(y,
                                      paste(c(colnames(data[,-which(colnames(data)%in% c(y, treatment))]), 
                                              treatVariable),collapse = "+"),
                                      sep = "~"))
      # RF
      upRF.model<-upliftRF(modeFormula, data = train_dt, mtry=3, ntree=100, split_method="ED", minsplit=200, verbose=TRUE)  
      
      print("UpliftRF model: Finish")
      
      # DT
      upDT.model<-upliftRF(modeFormula, data = train_dt, mtry=ncol(data)-2, ntree=1, bag.fraction = 1, split_method="ED", minsplit=200, verbose=TRUE)
      
      print("UpliftDT model: Finish")
      
      # KNN
      upKNN.model<-upliftKNN(train = train_dt, test =valid_dt, y = train_dt[,y], ct = train_dt[ ,treatment], dist.method = "euclidean", k = 3, agg.method = "majority" )
      
      print("UpliftKNN model: Finish")
      
      if(i==1){ 
        uplift_approach$RF$model <- upRF.model
        uplift_approach$DT$model <- upDT.model
        uplift_approach$KNN$model <- upKNN.model
      }
      
      # RF
      upRF.pred<-predict(upRF.model, newdata=valid_dt)
      upRFperf <- performance(upRF.pred[, 1], upRF.pred[, 2], valid_dt[,y], valid_dt[ ,treatment], direction = 1)
      upRFresult<-upliftDecile(upRFperf)
      
      uplift_approach$RF$gainOrigin[(1:length(upRFresult$gain)),i] <- upRFresult$gain
      uplift_approach$RF$gainRatio[(1:length(upRFresult$gain_ratio)),i] <- upRFresult$gain_ratio
      uplift_approach$RF$gini[(1:length(upRFresult$gini)),i] <- upRFresult$gini
      
      print("UpliftRF :: Success")
      
      # DT
      upDT.pred<-predict(upDT.model, newdata=valid_dt)
      upDTperf <- performance(upDT.pred[, 1], upDT.pred[, 2], valid_dt[,y], valid_dt[ ,treatment], direction = 1)
      upDTresult<-upliftDecile(upDTperf)
      
      uplift_approach$DT$gainOrigin[(1:length(upDTresult$gain)),i] <- upDTresult$gain
      uplift_approach$DT$gainRatio[(1:length(upDTresult$gain_ratio)),i] <- upDTresult$gain_ratio
      uplift_approach$DT$gini[(1:length(upDTresult$gini)),i] <- upDTresult$gini
      
      print("UpliftDT :: Success")
      
      # KNN
      upKNNperf<-performance(upKNN.model[,2],upKNN.model[,1], valid_dt[,y], valid_dt[ ,treatment], direction = 1)
      upKNNresult<-upliftDecile(upKNNperf)
      
      uplift_approach$KNN$gainOrigin[(1:length(upKNNresult$gain)),i] <- upKNNresult$gain
      uplift_approach$KNN$gainRatio[(1:length(upKNNresult$gain_ratio)),i] <- upKNNresult$gain_ratio
      uplift_approach$KNN$gini[(1:length(upKNNresult$gini)),i] <- upKNNresult$gini
      
      print("UpliftKNN :: Success")
    }else{
      trainIdx<-index[,i]
      train_dt <- data[ trainIdx,]
      valid_dt <- data[-trainIdx,]
      
      factorCol <- names(train_dt)[ sapply(train_dt, is.factor) ]
      
      # normalized train data (do not normalize the type of factor)
      train_dt.norm<- train_dt
      norm.value<-preProcess(train_dt[, -which(colnames(train_dt) %in% c(factorCol, y))], method = c("center", "scale")) 
      train_dt.norm[,-which(colnames(train_dt.norm) %in% c(factorCol, y))]<-predict(norm.value, train_dt[, -which(colnames(train_dt) %in% c(factorCol, y))])
      
      valid_dt.norm<- valid_dt
      valid_dt.norm[,-which(colnames(valid_dt.norm) %in% c(factorCol, y))]<-predict(norm.value, valid_dt[, -which(colnames(valid_dt) %in% c(factorCol, y))])
      
      
      # create dummy
      form <- interactFormula(train_dt.norm, y = y, treatment = treatment)
      print("Interaction for KNN :: Success")
      
      TraindummyData <- model.matrix(form, train_dt.norm)
      TraindummyDataWithY <- as.data.frame(TraindummyData)[,-1]
      
      ValiddummyData <- model.matrix(form, valid_dt.norm)
      ValiddummyDataWithY <- as.data.frame(ValiddummyData)[,-1]
      
      print("KNN Data Transformation :: Success")
      
      upKNN.model<-upliftKNN(train = TraindummyDataWithY, test =ValiddummyDataWithY, 
                             y = train_dt[,y]%>% as.character()%>% as.numeric(), 
                             ct = train_dt[ ,treatment]%>% as.character()%>% as.numeric(), 
                             dist.method = "euclidean", k = 3, agg.method = "majority" )
      
      print("UpliftKNN model: Finish")
      
      upKNNperf<-performance(upKNN.model[,2],upKNN.model[,1], valid_dt[,y]%>% as.character()%>% as.numeric(), 
                             valid_dt[ ,treatment]%>% as.character()%>% as.numeric(), direction = 1)
      upKNNresult<-upliftDecile(upKNNperf)
      
      if(i==1){ 
        uplift_approach$KNN$model <- upKNN.model
      }
      
      uplift_approach$KNN$gainOrigin[(1:length(upKNNresult$gain)),i] <- upKNNresult$gain
      uplift_approach$KNN$gainRatio[(1:length(upKNNresult$gain_ratio)),i] <- upKNNresult$gain_ratio
      uplift_approach$KNN$gini[(1:length(upKNNresult$gini)),i] <- upKNNresult$gini
      
      
      print("UpliftKNN :: Success")
    }
  }
  
  cat(crayon::red('Uplift Model Appraoch:: Boostrap Finish \n\n'))
  
  return(uplift_approach)
}

# example
# No Interaction
#OmaTest <- OneModelApproach(data = voter.1.1.1.1, 
#                            index = resample.result, 
#                            treatment = "MESSAGE_A", y = "MOVED_AD_NUM")

#TmaResult <-  TwoModelApproach(data = voter.1.1.1.1,
#                               index = resample.result, 
#                               treatment = "MESSAGE_A", y = "MOVED_AD_NUM")

#UpliftResult <- UpliftSpecialApproach(data = voter.1.1.1.1, index = resample.result, treatment = "MESSAGE_A", y = "MOVED_AD_NUM")



#OmaTest <- OneModelApproach(data = voter.1.1.1.1, 
#                            index = resample.result, treatment = "MESSAGE_A", y = "MOVED_AD_NUM", 
#                            interaction = TRUE)




