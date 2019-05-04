# TargetVar [Factor]: Outcome Variable
# TreatVar  [Factor]: Treatment Variable
# Ypre      [Factor]: Ypre Variable
# data      [Data.Frame]: Data for analyzing
# index     [Data.Frame]: Data for training index in ncol (# of bootstrap)

#### Group Uplift Prediction
Sep_Group_Uplift <- function(data, index, TargetVar, TreatVar, Ypre){
  
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(index), vector())%>% data.frame()
  model_template <- list('gainRatio' = res_gain.ratio)
  model_approach <- list('RF'= model_template,
                         'DT'= model_template,
                         'KNN'= model_template,
                         'Lm'= model_template,
                         "Lasso" = model_template,
                         "model" = NULL)
  
  uplift_approach <- list('RF'= model_template,
                          'DT'= model_template,
                          'KNN'= model_template)
  
  model_collector <- list('Oma'=model_approach,
                          'Tma'=model_approach,
                          "Uplift"=uplift_approach)
  
  for (i in c(1:ncol(index))) {
    trainIdx<-index[,i]
    
    # data for y and treatment is factor (Tma and Oma adapt this)
    data[, Ypre] <- factor(data[, Ypre], levels = c(0,1))
    data[, TreatVar] <- factor(data[, TreatVar], levels = c(0,1))
    data[, TargetVar] <- factor(data[, TargetVar], levels = c(0,1))
    
    train <- data[trainIdx,]
    valid <- data[-trainIdx,]
    
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
    
    cat(crayon::blue(sprintf('One Model Appraoch:: Boostrap %s start \n', i)))
    
    OmaResult <- SepOmaModule(Train_Ypre1 = trainYpre1, Train_Ypre0 = trainYpre0,
                              Valid_Ypre1 = validYpre1, Valid_Ypre0 = validYpre0,
                              TargetVar = TargetVar, TreatVar = TreatVar)
    
    model_collector$Oma$RF$gainRatio[(1:length(OmaResult$RF)),i] <- OmaResult$RF
    model_collector$Oma$DT$gainRatio[(1:length(OmaResult$DT)),i] <- OmaResult$DT
    model_collector$Oma$KNN$gainRatio[(1:length(OmaResult$KNN)),i] <- OmaResult$KNN
    model_collector$Oma$Lm$gainRatio[(1:length(OmaResult$LM)),i] <- OmaResult$LM
    model_collector$Oma$Lasso$gainRatio[(1:length(OmaResult$Lasso)),i] <- OmaResult$Lasso
    
    cat(crayon::blue(sprintf('Two Model Appraoch:: Boostrap %s start \n', i)))
    
    TmaResult <- SepTmaModule(Train_Ypre1 = trainYpre1, Train_Ypre0 = trainYpre0,
                              Valid_Ypre1 = validYpre1, Valid_Ypre0 = validYpre0,
                              TargetVar = TargetVar, TreatVar = TreatVar)
    
    model_collector$Tma$RF$gainRatio[(1:length(TmaResult$RF)),i] <- TmaResult$RF
    model_collector$Tma$DT$gainRatio[(1:length(TmaResult$DT)),i] <- TmaResult$DT
    model_collector$Tma$KNN$gainRatio[(1:length(TmaResult$KNN)),i] <- TmaResult$KNN
    model_collector$Tma$Lm$gainRatio[(1:length(TmaResult$LM)),i] <- TmaResult$LM
    model_collector$Tma$Lasso$gainRatio[(1:length(TmaResult$Lasso)),i] <- TmaResult$Lasso
    
    UpliftResult <- SepUpliftModule(Train_Ypre1 = trainYpre1, Train_Ypre0 = trainYpre0,
                                    Valid_Ypre1 = validYpre1, Valid_Ypre0 = validYpre0,
                                    TargetVar = TargetVar, TreatVar = TreatVar)
    
    model_collector$Uplift$RF$gainRatio[(1:length(UpliftResult$RF)),i] <- UpliftResult$RF
    model_collector$Uplift$DT$gainRatio[(1:length(UpliftResult$DT)),i] <- UpliftResult$DT
    model_collector$Uplift$KNN$gainRatio[(1:length(UpliftResult$KNN)),i] <- UpliftResult$KNN
    model_collector$Uplift$Lm$gainRatio[(1:length(UpliftResult$LM)),i] <- UpliftResult$LM
    model_collector$Uplift$Lasso$gainRatio[(1:length(UpliftResult$Lasso)),i] <- UpliftResult$Lasso
    
    if(i ==1){
      model_collector$Oma$model <- OmaResult$model
      model_collector$Tma$model <- TmaResult$model
      
    }
  }
  
  cat(crayon::red('Boostrap Finish \n\n'))
  return(model_collector)
}

Group_Uplift <- function(data, index, TargetVar, TreatVar, interaction = FALSE){
  
  if(interaction == TRUE){
    res_gain.ratio<-matrix(nrow = 11, ncol = ncol(index), vector())%>% data.frame()
    model_template <- list('gainRatio' = res_gain.ratio)
    model_approach <- list('RF'= model_template,
                           'DT'= model_template,
                           'KNN'= model_template,
                           'Lm'= model_template,
                           "Lasso" = model_template,
                           "IntLm" = model_template,
                           "IntLasso" = model_template,
                           "model" = list("NoInt" =NULL, "Int" =NULL))
    
    uplift_approach <- list('RF'= model_template,
                            'DT'= model_template,
                            'KNN'= model_template)
    
    model_collector <- list('Oma'=model_approach,
                            'Tma'=model_approach,
                            "Uplift"=uplift_approach)
  }else{
    res_gain.ratio<-matrix(nrow = 11, ncol = ncol(index), vector())%>% data.frame()
    model_template <- list('gainRatio' = res_gain.ratio)
    model_approach <- list('RF'= model_template,
                           'DT'= model_template,
                           'KNN'= model_template,
                           'Lm'= model_template,
                           "Lasso" = model_template,
                           "model" = NULL)
    
    uplift_approach <- list('RF'= model_template,
                            'DT'= model_template,
                            'KNN'= model_template)
    
    model_collector <- list('Oma'=model_approach,
                            'Tma'=model_approach,
                            "Uplift"=uplift_approach)
  }
  
  for (i in c(1:ncol(index))) {
    trainIdx<-index[,i]
    
    # data for y and treatment is factor (Tma and Oma adapt this)
    data[, TreatVar] <- factor(data[, TreatVar], levels = c(0,1))
    data[, TargetVar] <- factor(data[, TargetVar], levels = c(0,1))
    
    train <- data[trainIdx,]
    valid <- data[-trainIdx,]
    
    print("Ypre Separation :: OK")
    
    cat(crayon::blue(sprintf('One Model Appraoch:: Boostrap %s start \n', i)))
    
    OmaResult <- OmaModule(TrainData = train,ValidData = valid, TargetVar = TargetVar, TreatVar = TreatVar, Interaction = interaction)
    
    model_collector$Oma$RF$gainRatio[(1:length(OmaResult$RF)),i] <- OmaResult$RF
    model_collector$Oma$DT$gainRatio[(1:length(OmaResult$DT)),i] <- OmaResult$DT
    model_collector$Oma$KNN$gainRatio[(1:length(OmaResult$KNN)),i] <- OmaResult$KNN
    model_collector$Oma$Lm$gainRatio[(1:length(OmaResult$LM)),i] <- OmaResult$LM
    model_collector$Oma$Lasso$gainRatio[(1:length(OmaResult$Lasso)),i] <- OmaResult$Lasso
    
    cat(crayon::blue(sprintf('Two Model Appraoch:: Boostrap %s start \n', i)))
    
    TmaResult <-  TmaModule(TrainData = train,ValidData = valid, TargetVar = TargetVar, TreatVar = TreatVar, Interaction = interaction)
    
    model_collector$Tma$RF$gainRatio[(1:length(TmaResult$RF)),i] <- TmaResult$RF
    model_collector$Tma$DT$gainRatio[(1:length(TmaResult$DT)),i] <- TmaResult$DT
    model_collector$Tma$KNN$gainRatio[(1:length(TmaResult$KNN)),i] <- TmaResult$KNN
    model_collector$Tma$Lm$gainRatio[(1:length(TmaResult$LM)),i] <- TmaResult$LM
    model_collector$Tma$Lasso$gainRatio[(1:length(TmaResult$Lasso)),i] <- TmaResult$Lasso
    
    if(interaction == TRUE){
      model_collector$Oma$IntLm$gainRatio[(1:length(OmaResult$IntLm)),i] <- OmaResult$IntLm
      model_collector$Oma$IntLasso$gainRatio[(1:length(OmaResult$IntLasso)),i] <- OmaResult$IntLasso
      
      model_collector$Tma$IntLm$gainRatio[(1:length(TmaResult$IntLm)),i] <- TmaResult$IntLm
      model_collector$Tma$IntLasso$gainRatio[(1:length(TmaResult$IntLasso)),i] <- TmaResult$IntLasso
    }
    
    #UpliftResult <- UpliftModule(TrainData = train,ValidData = valid, TargetVar = TargetVar, TreatVar = TreatVar, Interaction = interaction)
    
    #model_collector$Uplift$RF$gainRatio[(1:length(UpliftResult$RF)),i] <- UpliftResult$RF
    #model_collector$Uplift$DT$gainRatio[(1:length(UpliftResult$DT)),i] <- UpliftResult$DT
    #model_collector$Uplift$KNN$gainRatio[(1:length(UpliftResult$KNN)),i] <- UpliftResult$KNN
    #model_collector$Uplift$Lm$gainRatio[(1:length(UpliftResult$LM)),i] <- UpliftResult$LM
    #model_collector$Uplift$Lasso$gainRatio[(1:length(UpliftResult$Lasso)),i] <- UpliftResult$Lasso
    
    if(i ==1){
      model_collector$Oma$model$NoInt <- OmaResult$model
      model_collector$Tma$model$NoInt <- TmaResult$model
      
      model_collector$Oma$model$Int <- OmaResult$Int_model
      model_collector$Tma$model$Int <- TmaResult$Int_model
      
    }
  }
  
  cat(crayon::red('Boostrap Finish \n\n'))
  return(model_collector)
}

#### Function
# trainData [Data.Frame]: Training Data
# validData [Data.Frame]: Validation Data
# OneModel  [Boolean]: Transform the data for One Model Approahc (True)/Two Model Approach (FALSE)
DataTransform<- function(trainData, validData, OneModel=TRUE, y, treatment, interaction =FALSE){
  
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
    
    # Valid
    ValidLmT1 <- model.matrix(Lm.form, valid_T1)
    
    print("LM/Lasso Validation For treatment= 1 :: Success")
    
    ValidLmT0 <- model.matrix(Lm.form, valid_T0)
    
    print("LM/Lasso Validation For treatment= 0 :: Success")
    
    LassoDataCollect <- list("T0" =ValidLmT0, "T1"= ValidLmT1)
    
    print("LM/Lasso Data Transformation :: Success")
    
    
    
    if (interaction == TRUE) {
      Lm.form <- interactFormula(data, y = y, treatment = treatment)
      print("Interaction for Lasso/LM :: Start")
      
      # Train
      IntTrainData <- model.matrix(Lm.form, trainData)
      
      # Valid
      # Treatment =1
      IntValidLmT1 <- model.matrix(Lm.form, valid_T1)
      
      print("LM/Lasso Validation For treatment= 1 :: Success")
      
      # Treatment =0
      IntValidLmT0 <- model.matrix(Lm.form, valid_T0)
      
      IntDataCollect <- list("T0" =IntValidLmT0, "T1"= IntValidLmT1)
      
      print("LM/Lasso Validation For treatment= 0 :: Success")
      
      print("LM/Lasso Data Transformation :: Success")
      
      Train <- list("Origin" = trainData, "KNN" =TraindummyData, "Lasso"=TrainLmData, "IntAct" = IntTrainData)
      Valid <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "IntAct"=IntDataCollect,
                    "y" = validData[, y], "ct" =validData[, treatment])
      Data <- list("Train" =Train, "Valid" = Valid)
    }else{
      Train <- list("Origin" = trainData, "KNN" =TraindummyData, "Lasso"=TrainLmData)
      Valid <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "y" = validData[, y], "ct" =validData[, treatment])
      Data <- list("Train" =Train, "Valid" = Valid)
    }
    
    
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
    
    
    
    
    
    
    
    
    
    #########
    ##########
    ############
    ########
    if (interaction == TRUE) {
      Lm.form <- interactFormula(data, y = y, treatment = treatment)
      print("Interaction for Lasso/LM :: Start")
      
      # Data Convert to dummy
      TrainData <- model.matrix(Lm.form, trainData)
      ValidData <- model.matrix(Lm.form, validData)
      
      # Paste with y
      IntTrainData <-  cbind(as.data.frame(TrainData), y=trainData[,y])
      IntValidData <-  cbind(as.data.frame(ValidData), y=validData[,y])
      
      dummyTreat <- paste0(treatment,"1")
      
      IntValidData <- IntValidData[,-which(colnames(IntValidData) %in% c(dummyTreat, "(Intercept)"))]
      
      # treatment =1
      IntTrainT1 <- IntTrainData[which(IntTrainData[,dummyTreat]==1), -which(colnames(IntTrainData)%in% c(dummyTreat, "(Intercept)"))]
      
      print("Lm/Lasso Training Data Treatment =1 :: OK")
      
      # treatment =2
      IntTrainT0 <- IntTrainData[which(IntTrainData[,dummyTreat]==0), -which(colnames(IntTrainData)%in% c(dummyTreat, "(Intercept)"))]
      
      print("Lm/Lasso Training Data Treatment =0 :: OK")
      IntDataCollect <- list("T0" =IntTrainT0, "T1"= IntTrainT1)
      
      
      print("LM/Lasso Data Transformation :: Success")
      
      
      Train <- list("Origin" = trainData, "KNN" =TraindummyData, "Lasso"=TrainLmData, "IntAct" = IntTrainData)
      Valid <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "IntAct"=IntDataCollect,
                    "y" = validData[, y], "ct" =validData[, treatment])
      
      
      Train <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "IntAct"= IntDataCollect)
      Valid <- list("Origin" = validData, "KNN" =ValiddummyData, "Lasso"=ValidLmData, "IntAct"=IntValidData,
                    "y" = validData[, y], "ct" = validData[, treatment])
      
      Data <- list("Train" =Train, "Valid" = Valid)
    }else{
      Train <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect)
      Valid <- list("Origin" = validData, "KNN" =ValiddummyData, "Lasso"=ValidLmData, "y" = validData[, y], "ct" = validData[, treatment])
      Data <- list("Train" =Train, "Valid" = Valid)
    }
    
    return(Data)
  }
}

# ResYpre1       [Data.Frame]: Result of Probability under T=1/0 given the Ypre =0
# ResYpre0       [Data.Frame]: Result of Probability under T=1/0 given the Ypre =1
# TreatVarYpre1  [string]: Treatment Variable given the Ypre =1
# TreatVarYpre0  [string]: Treatment Variable given the Ypre =0
# TargetVarYpre1 [string]: Outcome Variable given the Ypre =1
# TargetVarYpre0 [string]: Outcome Variable given the Ypre =0


SepPerform <- function(ResYpre1, ResYpre0, TargetVarYpre1, TargetVarYpre0, TreatVarYpre1, TreatVarYpre0){
  Ypre1 <- cbind(ResYpre1, y =TargetVarYpre1, ct = TreatVarYpre1)
  Ypre0 <- cbind(ResYpre0, y =TargetVarYpre0, ct = TreatVarYpre0)
  
  WholeData <- rbind(Ypre1, Ypre0)
  
  perf <- performance(WholeData$T1, WholeData$T0,
                      WholeData$y %>% as.character()%>%as.numeric(), 
                      WholeData$ct%>% as.character()%>%as.numeric(), direction = 1)
  
  
  
  result<-upliftDecile(perf)
  
}

### decile result
upliftDecile <- function(data){
  experiment<-data.frame("k"=c(0,seq(1,nrow(data),by = 1)), "n_t"=c(0,data[,2]),"n_c"=c(0,data[,3]),
                         "n_t.y1"=c(0,data[,4]), "n_c.y1"= c(0,data[,5]), 
                         "r_t.y1"=c(0,data[,6]), "r_c.y1"=c(0,data[,7]) ,"u_k"=c(0,data[,8]),
                         "cum.n_t"= c(0,cumsum(data[,2])),"cum.n_c"= c(0,cumsum(data[,3])),
                         "cum.n_t.y1"= c(0,cumsum(data[,4])),"cum.n_c.y1"= c(0,cumsum(data[,5])))%>%
    mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
    mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
    mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
    mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
    mutate(gain_ratio = gain/gain[length(k)])%>%
    mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
  
  return(experiment)
}

#KNN prediction and probability collector
KnnPrediction <- function(data){
  
  # collect the classification and the probability together
  data=data.frame(Prediction = data%>% as.numeric(),
                  Prob = attr(data, "prob"))%>%
    mutate(PredictionCounter = 1 - Prediction,
           ProbCounter = 1- Prob)
  
  # create a new data frame for storing
  collectDT <- as.data.frame(matrix(nrow = nrow(data), ncol = 2))
  colnames(collectDT) <- c("1", "0")
  
  for (i in c(1:nrow(data))) {
    
    if (data[i,"Prediction"] ==1) {
      collectDT[i,"1"] <- data[i,"Prob"]
      collectDT[i,"0"] <- data[i,"ProbCounter"]
    }else{
      collectDT[i,"0"] <- data[i,"Prob"]
      collectDT[i,"1"] <- data[i,"ProbCounter"]
    }
    
  }
  return(collectDT)
}


# formula with/without interaction
# y             [string]: Outcome Variable
# treatment     [string]: Treatment Variable
# treat_include [Boolean]: Include the treatment variable in equation or not (Two Model Apprach-> treat_include =FALSE)
interactFormula <- function(data, y, treatment, treat_include=TRUE){
  
  # first create the formuala without interaction ex. y~ a+b+c
  # create the interactino term                   ex. treat*a + treat*b + treat*c
  # combine the two part into one formula         ex. y~ a + b + c + treat*a + treat*b + treat*c
  
  if(treat_include == TRUE){
    paste(paste(y, paste(colnames(data[, -which(colnames(data) ==y)]), collapse = "+"), sep = "~"), 
          paste(treatment, c(colnames(data[, -which(colnames(data) %in% c(y,treatment))])), collapse ="+", sep = "*")
          , sep = "+") %>%
      as.formula()%>%
      return()
  }else{
    paste(paste(y, paste(colnames(data[, -which(colnames(data) %in% c(treatment, y))]), collapse = "+"), sep = "~"), 
          paste(treatment, c(colnames(data[, -which(colnames(data) %in% c(y,treatment))])), collapse ="+", sep = "*")
          , sep = "+") %>%
      as.formula()%>%
      return()
  }
  
  
  
}
originFormula <- function(data, y, treatment, treat_include=TRUE){
  
  if (treat_include==TRUE) {
    paste(paste(y, paste(colnames(data[, -which(colnames(data) ==y)]), collapse = "+"), sep = "~")) %>%
      as.formula()%>%
      return()
  }else{
    paste(paste(y, paste(colnames(data[, -which(colnames(data) %in% c(y, treatment))]), collapse = "+"), sep = "~")) %>%
      as.formula()%>%
      return()
  }
  
}

#interactFormula(voter.1.1.1.1, y = "MOVED_AD_NUM", treatment = "MESSAGE_A", treat_include = FALSE)
#originFormula(voter.1.1.1.1, y ="MOVED_AD_NUM", treatment ="MESSAGE_A", treat_include = FALSE)




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
    IntLm.form <- interactFormula(data, y = y, treatment = treatment)
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
                         "Lasso" = Lasso_Result, "IntLm"=IntLm_Result, "IntLassao" =IntLasso_Result,
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
SepOmaModule <- function(Train_Ypre1, Train_Ypre0, Valid_Ypre1, Valid_Ypre0, TargetVar, TreatVar){
  # transform data
  Data_Ypre1 <- DataTransform(trainData =Train_Ypre1, validData = Valid_Ypre1, OneModel = TRUE, y = TargetVar, treatment = TreatVar,interaction = FALSE)
  Data_Ypre0 <- DataTransform(trainData =Train_Ypre0, validData = Valid_Ypre0, OneModel = TRUE, y = TargetVar, treatment = TreatVar,interaction = FALSE)
  
  # predict separately
  ModelYpre1 <- OmaMethod(Data_Ypre1, y = TargetVar, treatment = TreatVar, interaction = FALSE)
  ModelYpre0 <- OmaMethod(Data_Ypre0, y = TargetVar, treatment = TreatVar, interaction = FALSE)
  
  
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
  
  
  Result <- list("RF" = RF.result[, "gain_ratio"],
                 "DT" = DT.result[, "gain_ratio"],
                 "KNN" = KNN.result[,"gain_ratio"],
                 "LM" = LM.result[, "gain_ratio"],
                 "Lasso" = Lasso.result[,"gain_ratio"],
                 "model" = MODEl)
  
  return(Result)
}
SepTmaModule <- function(Train_Ypre1, Train_Ypre0, Valid_Ypre1, Valid_Ypre0, TargetVar, TreatVar){
  # transform data
  Data_Ypre1 <- DataTransform(trainData =Train_Ypre1, validData = Valid_Ypre1, OneModel = FALSE, y = TargetVar, treatment = TreatVar, interaction = FALSE)
  Data_Ypre0 <- DataTransform(trainData =Train_Ypre0, validData = Valid_Ypre0, OneModel = FALSE, y = TargetVar, treatment = TreatVar, interaction = FALSE)
  
  # predict separately
  ModelYpre1 <- TmaMethod(Data_Ypre1, y = TargetVar, treatment = TreatVar, interaction = FALSE)
  ModelYpre0 <- TmaMethod(Data_Ypre0, y = TargetVar, treatment = TreatVar, interaction = FALSE)
  
  
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
  
  
  Result <- list("RF" = RF.result[, "gain_ratio"],
                 "DT" = DT.result[, "gain_ratio"],
                 "KNN" = KNN.result[, "gain_ratio"],
                 "LM" = LM.result[,"gain_ratio"],
                 "Lasso" = Lasso.result[,"gain_ratio"],
                 "model" = MODEl)
  
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
  Data_Collect <- DataTransform(trainData =TrainData, validData = ValidData, OneModel = TRUE, y = TargetVar, treatment = TreatVar,interaction = Interaction)
  
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
    
    IntLasso.result <- performance(pr.y1_ct1 = Model$IntLassao$T1, pr.y1_ct0 = Model$IntLassao$T0, 
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
#index <- resample.voter.1.1.1.1[,1]

#train_dt <- data[index,]
#valid_dt <- data[-index,]

#a <- DataTransform(trainData = train_dt, validData = valid_dt, OneModel = TRUE, y = "MOVED_AD_NUM", treatment = "MESSAGE_A", interaction = TRUE)
#b <- UpliftMethod(train = train_dt, valid = valid_dt, y = "MOVED_AD_NUM", treatment = "MESSAGE_A")
#c <- OmaModule(TrainData = train_dt, ValidData = valid_dt, TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A", Interaction = TRUE)
#d <- Group_Uplift(data = voter.1.1.1.1, index = resample.voter.1.1.1.1[,1:2], TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A", interaction = TRUE)
