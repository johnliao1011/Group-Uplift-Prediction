library(R.utils)
library(dplyr)
library(data.table)
library(ggplot2)
library(FNN)
library(caret)
library(e1071)
library(forcats)
library(penalized)
library(randomForest)
library(tables)
library(uplift)
library(parallel)
library(rpart)
library(doParallel)
library(crayon)
library(glmnet)
readline(prompt = crayon::cyan('Please insert the number of cores you would like to run by parallelling: ')) %>% 
  registerDoParallel(cores = as.integer(.))


### Bootstrap index

# treatment     [vector]: Treatment variable
# response      [vector]: Response variable
# n_booststrap  [numerical]: Number of bootstrap for experiment

bootstrap.dt<-function(treatment, response, n_booststrap){
  t1_y1<-which(treatment==1 & response==1)%>% as.matrix()
  t1_y0<-which(treatment==1 & response==0)%>% as.matrix()
  t0_y1<-which(treatment==0 & response==1)%>% as.matrix()
  t0_y0<-which(treatment==0 & response==0)%>% as.matrix()
  
  sample_t1_y1<-sample(t1_y1, length(t1_y1)*0.6)
  sample_t1_y0<-sample(t1_y0, length(t1_y0)*0.6)
  sample_t0_y1<-sample(t0_y1, length(t0_y1)*0.6)
  sample_t0_y0<-sample(t0_y0, length(t0_y0)*0.6)
  
  index<-c(sample_t1_y1, sample_t1_y0, sample_t0_y1, sample_t0_y0)
  
  num_bootstraps<-n_booststrap
  resample_dt<-replicate(num_bootstraps, index)
  
  return(resample_dt)
}        


### Transform data into required training and validation for Oma and Tma (Option: Including the interaction predictors)

# trainData   [Data.Frame]: Training Data
# validData   [Data.Frame]: Validation Data
# OneModel    [Boolean]: Transform the data for One Model Approahc (True)/Two Model Approach (FALSE)
# treatment   [String]: Treatment variable
# interaction [Boolean]: Including the interaction terms or not
DataTransform<- function(trainData, validData, OneModel=TRUE, y, treatment, interaction =FALSE){
  
  train <- NULL
  valid <- NULL
  
  if (OneModel==TRUE) {
    # One Model Appraoch
    #### DT, RF, Lm
    # Train 
    train <- trainData
    
    # Validation
    # treatment=1
    valid <- validData
    valid_T1 <- valid
    valid_T1[, treatment]<-1
    valid_T1[, treatment]<-factor(valid_T1[, treatment], levels = c(0,1))
    
    print("Validation For treatment= 1 :: Success")
    
    # treatment= 0
    valid_T0<-valid
    valid_T0[, treatment]<-0
    valid_T0[, treatment]<-factor(valid_T0[, treatment], levels = c(0,1))
    
    OriginDataCollect <- list("T0" =valid_T0, "T1"= valid_T1)
    
    print("Validation For treatment= 0 :: Success")
    
    
    #### KNN
    # Train
    factorCol <- names(train)[ sapply(train, is.factor) ]
    
    # normalized train data (do not normalize the type of factor)
    train_dt.norm<- train
    norm.value<-preProcess(train[, -which(colnames(train) %in% factorCol)], method = c("center", "scale")) 
    train_dt.norm[,-which(colnames(train_dt.norm) %in% factorCol)]<-predict(norm.value, train[, -which(colnames(train) %in% factorCol)])
    
    # create dummy
    form <- originFormula(train_dt.norm, y = y, treatment = treatment)
    TraindummyData <- model.matrix(form, train_dt.norm)
    TraindummyData <- cbind(as.data.frame(TraindummyData), y=train[,y])[,-1]
    
    
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
    Lm.form <- originFormula(train, y = y, treatment = treatment)
    
    TrainLmData <- model.matrix(Lm.form, train)
    
    # Valid
    ValidLmT1 <- model.matrix(Lm.form, valid_T1)
    
    print("LM/Lasso Validation For treatment= 1 :: Success")
    
    ValidLmT0 <- model.matrix(Lm.form, valid_T0)
    
    print("LM/Lasso Validation For treatment= 0 :: Success")
    
    LassoDataCollect <- list("T0" =ValidLmT0, "T1"= ValidLmT1)
    
    print("LM/Lasso Data Transformation :: Success")
    
    
    
    if (interaction == TRUE) {
      Lm.form <- interactFormula(data = valid, y = y, treatment = treatment)
      print("Interaction for Lasso/LM :: Start")
      
      # Train
      IntTrainData <- model.matrix(Lm.form, train)
      
      # Valid
      # Treatment =1
      IntValidLmT1 <- model.matrix(Lm.form, valid_T1)
      
      print("LM/Lasso Validation For treatment= 1 :: Success")
      
      # Treatment =0
      IntValidLmT0 <- model.matrix(Lm.form, valid_T0)
      
      IntDataCollect <- list("T0" =IntValidLmT0, "T1"= IntValidLmT1)
      
      print("LM/Lasso Validation For treatment= 0 :: Success")
      
      print("LM/Lasso Data Transformation :: Success")
      
      Train <- list("Origin" = train, "KNN" =TraindummyData, "Lasso"=TrainLmData, "IntAct" = IntTrainData)
      Valid <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "IntAct"=IntDataCollect,
                    "y" = valid[, y], "ct" =valid[, treatment])
      Data <- list("Train" =Train, "Valid" = Valid)
    }else{
      Train <- list("Origin" = train, "KNN" =TraindummyData, "Lasso"=TrainLmData)
      Valid <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "y" = valid[, y], "ct" =valid[, treatment])
      Data <- list("Train" =Train, "Valid" = Valid)
    }
    
    
    return(Data)
    
  }else{
    # Two Model Approach
    
    ### DT, RF, LM
    train <- trainData
    # Train
    # Treatment =1
    train_T1<-train[ which(train[,treatment]==1)  ,-which(colnames(train)== treatment)]
    print("Training Data Treatment =1 :: OK")
    
    # Treatment =0
    train_T0<-train[ which(train[,treatment]==0)  ,-which(colnames(train)== treatment)]
    print("Training Data Treatment =0 :: OK")
    
    OriginDataCollect <- list("T0" =train_T0, "T1"= train_T1)
    
    # Valid
    valid <- validData
    print("Data Type Change:: Success")
    
    ### KNN
    # Train
    factorCol <- names(train)[ sapply(train, is.factor) ]
    
    # normalized train data (do not normalize the type of factor)
    train_dt.norm<- train
    norm.value<-preProcess(train[, -which(colnames(train) %in% factorCol)], method = c("center", "scale")) 
    train_dt.norm[,-which(colnames(train_dt.norm) %in% factorCol)]<-predict(norm.value, train[, -which(colnames(train) %in% factorCol)])
    
    valid_dt.norm<- valid
    valid_dt.norm[,-which(colnames(valid_dt.norm) %in% factorCol)]<-predict(norm.value, valid[, -which(colnames(valid) %in% factorCol)])
    
    
    # create dummy
    form <- originFormula(train_dt.norm, y = y, treatment = treatment)
    
    TraindummyData <- model.matrix(form, train_dt.norm)
    TraindummyDataWithY <- cbind(as.data.frame(TraindummyData), y=train[,y])[,-1]
    
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
    Lm.form <- originFormula(train, y = y, treatment = treatment)
    
    dummyTreat <- paste0(treatment,"1")
    TrainLmData <- model.matrix(Lm.form, train)
    TrainLmData <- TrainLmData[,-which(colnames(TrainLmData) == "(Intercept)")]
    
    # treatment = 1
    LmTrainT1 <- TrainLmData[which(TrainLmData[,dummyTreat]==1), -which(colnames(TrainLmData)==dummyTreat)]
    
    print("Lm/Lasso Training Data Treatment =1 :: OK")
    
    # treatment = 0
    LmTrainT0 <- TrainLmData[which(TrainLmData[,dummyTreat]==0), -which(colnames(TrainLmData)==dummyTreat)]
    
    LassoDataCollect <- list("T0" =LmTrainT0, "T1"= LmTrainT1)
    
    print("Lm/Lasso Training Data Treatment =0 :: OK")
    # Valid
    
    ValidLmData <- model.matrix(Lm.form, valid)
    ValidLmData <- ValidLmData[,-which(colnames(ValidLmData) %in% c(dummyTreat, "(Intercept)"))]
    
    print("Lasso Data Transformation :: Success")
    
    
    if (interaction == TRUE) {
      Lm.form <- interactFormula(data = train, y = y, treatment = treatment)
      print("Interaction for Lasso/LM :: Start")
      
      # Data Convert to dummy
      TrainData <- model.matrix(Lm.form, train)
      ValidData <- model.matrix(Lm.form, valid)
      
      # Paste with y
      IntTrainData <-  cbind(as.data.frame(TrainData), y=train[,y])
      IntValidData <-  cbind(as.data.frame(ValidData), y=valid[,y])
      
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
      
      
      Train <- list("Origin" = train, "KNN" =TraindummyData, "Lasso"=TrainLmData, "IntAct" = IntTrainData)
      Valid <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "IntAct"=IntDataCollect,
                    "y" = valid[, y], "ct" =valid[, treatment])
      
      
      Train <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect, "IntAct"= IntDataCollect)
      Valid <- list("Origin" = valid, "KNN" =ValiddummyData, "Lasso"=ValidLmData, "IntAct"=IntValidData,
                    "y" = valid[, y], "ct" = valid[, treatment])
      
      Data <- list("Train" =Train, "Valid" = Valid)
    }else{
      Train <- list("Origin" = OriginDataCollect, "KNN" = KNNDataCollect, "Lasso"= LassoDataCollect)
      Valid <- list("Origin" = valid, "KNN" =ValiddummyData, "Lasso"=ValidLmData, "y" = valid[, y], "ct" = valid[, treatment])
      Data <- list("Train" =Train, "Valid" = Valid)
    }
    
    return(Data)
  }
}


### Performance for modelling separately using Ypre

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


### formula with/without interaction
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






#data <- voter.1.1.1.1
#index <- resample.voter.1.1.1.1[,1]

#train_dt <- data[index,]
#valid_dt <- data[-index,]

#a <- DataTransform(trainData = train_dt, validData = valid_dt, OneModel = TRUE, y = "MOVED_AD_NUM", treatment = "MESSAGE_A", interaction = TRUE)