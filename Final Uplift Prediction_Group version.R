# TargetVar [Factor]: Outcome Variable
# TreatVar  [Factor]: Treatment Variable
# Ypre      [Factor]: Ypre Variable
# data      [Data.Frame]: Data for analyzing
# index     [Data.Frame]: Data for training index in ncol (# of bootstrap)

#### Group Uplift Prediction
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
    Data <- Null
    trainIdx<-index[,i]
    
    # data for y and treatment is factor (Tma and Oma adapt this)
    Data <- data
    Data[, TreatVar] <- factor(Data[, TreatVar], levels = c(0,1))
    Data[, TargetVar] <- factor(Data[, TargetVar], levels = c(0,1))
    
    train <- Data[trainIdx,]
    valid <- Data[-trainIdx,]
    
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

#data <- voter.1.1.1.1
#index <- resample.voter.1.1.1.1

#train_dt <- data[index,]
#valid_dt <- data[-index,]

#a <- DataTransform(trainData = train_dt, validData = valid_dt, OneModel = TRUE, y = "MOVED_AD_NUM", treatment = "MESSAGE_A", interaction = TRUE)
#b <- UpliftMethod(train = train_dt, valid = valid_dt, y = "MOVED_AD_NUM", treatment = "MESSAGE_A")
#c <- OmaModule(TrainData = train_dt, ValidData = valid_dt, TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A", Interaction = TRUE)
#d <- Group_Uplift(data = voter.1.1.1.1, index = resample.voter.1.1.1.1, TargetVar = "MOVED_AD_NUM", TreatVar = "MESSAGE_A", interaction = TRUE)
