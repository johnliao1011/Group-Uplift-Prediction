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


#### functional model
#plot_resample_line<-function(sample_i) {
  lines(x = seq(0,100, by=10), y = sample_i, type = "l" , col=rgb(1, 0.0, 0.0, 0.01))
  return(mean(sample_i))
}


plot_bootstrap<-function(dat, main_title, ylim_range){
  if(is.null(ncol(dat))==TRUE ){
    
    # only one group of observations
    plot(x = seq(0,100, by=100/(length(dat)-1)), y=dat, type="b",col="gray" ,main=main_title,
         ylab="Cumulative Incremental Gains", xlab="Proportion of population targeted (%)", ylim=ylim_range)
    
    lines(seq(0,100,100/(length(dat)-1)), seq(0,1,1/(length(dat)-1)), type = "l", col="black")
  } else{
    
    # multiple groups of observations
    plot(x = seq(0,100, by=100/(nrow(dat)-1)), y=dat[, 1], type="l", lwd=0,main=main_title, 
         ylab="Cumulative Incremental Gains", xlab="Proportion of population targeted (%)", ylim=ylim_range)
    for (i  in c(1:ncol(dat))) {
      lines(x = seq(0,100, by=100/(nrow(dat)-1)), y= dat[, i] , type="b",col="gray")
    }
    
    #conf_97.5 <- apply(dat, 1, function(x)quantile(x, .975))
    #conf_2.5 <- apply(dat, 1, function(x) quantile(x, .025))
    mean_gain<-apply(dat, 1, mean)
    
    #lines(seq(0,100,100/(nrow(dat)-1)), conf_2.5, type = "l", col="black", lty="dashed", lwd=1)
    #lines(seq(0,100,100/(nrow(dat)-1)), conf_97.5, type = "l", col="black", lty="dashed", lwd=1)
    lines(seq(0,100,100/(nrow(dat)-1)), mean_gain, type = "l", col="tomato", lwd=1)
    lines(seq(0,100,100/(nrow(dat)-1)), seq(0,1,1/(nrow(dat)-1)), type = "l", col="black")
  }  
}
                                # plot for bootstrap



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


# formula with interaction
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

#interactFormula(voter.1.1.1.1, y = "MOVED_AD_NUM", treatment = "MESSAGE_A", treat_include = FALSE)


# formula without interaction
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

#originFormula(voter.1.1.1.1, y ="MOVED_AD_NUM", treatment ="MESSAGE_A", treat_include = FALSE)










#plot_bootstrap<-function(dat, main_title, ylim_range){
#  plot(x = seq(0,100, by=10), y=dat[, 1], type="l", lwd=0,main=main_title, 
#       ylab="Cumulative Incremental Gains", xlab="Proportion of population targeted (%)", ylim=ylim_range)
#  for (i  in c(1:ncol(dat))) {
#    lines(x = seq(0,100, by=10), y= dat[, i] , type="b",col="gray")
#  }
  
#  conf_97.5 <- apply(dat, 1, function(x)quantile(x, .975))
#  conf_2.5 <- apply(dat, 1, function(x) quantile(x, .025))
#  mean_gain<-apply(dat, 1, mean)
  
#  lines(seq(0,100,10), conf_2.5, type = "l", col="black", lty="dashed", lwd=2)
#  lines(seq(0,100,10), conf_97.5, type = "l", col="black", lty="dashed", lwd=2)
#  lines(seq(0,100,10), mean_gain, type = "l", col="tomato", lwd=3)
#  lines(seq(0,100,10), seq(0,1,0.1), type = "l", col="black")
#}                    # plot for bootstrap

plot_bootstrap_gini<-function(dat, main_title, ylim){
  colnames(dat)<- c(paste("ex", seq(1,ncol(dat), by = 1), sep = "."))
  rownames(dat)<- c(paste("gini", seq(0, 100, by = 10), sep="_"))
  dat[-1,]%>%
    t()%>%
    boxplot(main=main_title, ylim=ylim)
}                     # plot for gini
uplift.dt.size<-function(dat, treatment, response, time_of_size){
  t1_y1<-dat[sample(which(treatment==1 & response==1), length(which(treatment==1 & response==1))*time_of_size),]
  t1_y0<-dat[sample(which(treatment==1 & response==0), length(which(treatment==1 & response==0))*time_of_size),]
  t0_y1<-dat[sample(which(treatment==0 & response==1), length(which(treatment==0 & response==1))*time_of_size),]
  t0_y0<-dat[sample(which(treatment==0 & response==0), length(which(treatment==0 & response==0))*time_of_size),]
  
  new_dt<-t1_y1%>%
    rbind(.,t1_y0 )%>%
    rbind(., t0_y1)%>%
    rbind(., t0_y0) 
  
  size<-matrix(c(t1_y1%>%nrow(),t1_y0%>%nrow(), t0_y1%>%nrow(), t0_y0%>%nrow() ), 
               nrow = 4, ncol = 1, dimnames = list(c("t1_y1", "t1_y0", "t0_y1", "t0_y0")))
  
  res<-list(new_dt = new_dt, size= size)
  return(res)
}         # tune the size of data (larger/smaller)
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
}                # resemple the index number from the data


#### model function

####### Simple model without bootstrap
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
tma.dtree <- function(x, y, ct){
  if (!is.factor(y)) {y <- factor(y)}
  
  df <- cbind(x, ct, y)
  rownames(df) <- 1:nrow(df)
  
  fml <- as.formula(paste("y ~ ", paste(colnames(x), collapse = "+")))
  
  require(caret)
  mdl.trt <- rpart(fml, data = df[ct==1,], method = "class")   # Model for the treatment-group
  mdl.ctrl <- rpart(fml, data = df[ct==0,], method = "class")  # Model for the control-group
  
  res <- list(mdl.treatment = mdl.trt,
              mdl.control = mdl.ctrl)
  class(res) <- "tma"
  
  return(res)   
}
tma.RF <- function(x, y, ct){
  if (!is.factor(y)) {y <- factor(y)}
  
  df <- cbind(x, ct, y)
  rownames(df) <- 1:nrow(df)
  
  fml <- as.formula(paste("y ~ ", paste(colnames(x), collapse = "+")))
  
  require(caret)
  mdl.trt <- randomForest(fml, data = df[ct==1,], ntree=100)   # Model for the treatment-group
  mdl.ctrl <- randomForest(fml, data = df[ct==0,], ntree=100)  # Model for the control-group
  
  res <- list(mdl.treatment = mdl.trt,
              mdl.control = mdl.ctrl)
  class(res) <- "tma"
  
  return(res)   
}



# knn will use ->tma(train_dt[,-c(1,10)], train_dt$MOVED_AD_NUM, train_dt$MESSAGE_A,method = "knn", tuneGrid = data.frame(k = 3))

predict.tma <- function(object, newdata){
  rownames(newdata) <- 1:nrow(newdata)
  pred.trt <- predict(object$mdl.treatment, newdata, type="prob")
  pred.ctrl <- predict(object$mdl.control, newdata, type="prob")
  return(cbind(pr.y1_ct1 = pred.trt$"1", pr.y1_ct0 = pred.ctrl$"1"))
}
predict.tma.dtree <- function(object, newdata){
  rownames(newdata) <- 1:nrow(newdata)
  pred.trt <- predict(object[1], newdata, type="prob")
  pred.ctrl <- predict(object[2], newdata, type="prob")
  return(cbind(pr.y1_ct1 = pred.trt[[1]][,2], pr.y1_ct0 = pred.ctrl[[1]][,2]))
}
predict.tma.RF <- function(object, newdata){
  rownames(newdata) <- 1:nrow(newdata)
  pred.trt <- predict(object[1], newdata, type="prob")
  pred.ctrl <- predict(object[2], newdata, type="prob")
  return(cbind(pr.y1_ct1 = pred.trt[[1]][,2], pr.y1_ct0 = pred.ctrl[[1]][,2]))
}



####### One model approach - Directly modeling
bootstrap.uplift<-function(resample_index,dat,treatment, treat_result, uplift_formula, pre_treat ){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  res_gain.ratio_all<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  #res_gain.ratio_ypre.1<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  #res_gain.ratio_ypre.0<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  for (i in c(1:ncol(resample_index))) {
    cat("uplift.RF: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-dat[index,]
    valid_dt<-dat[-index,]
    
    upRF.fit<-upliftRF(uplift_formula, data = train_dt, mtry=3, ntree=100, split_method="ED", minsplit=200, verbose=TRUE)
    upRF.pred<-predict(upRF.fit, newdata=valid_dt)
    perf <- performance(upRF.pred[, 1], upRF.pred[, 2], valid_dt[,treat_result], valid_dt[ ,treatment], direction = 1)
    
    # calculating the incremental gains
    experiment<-upliftDecile(perf)
    
    # separate
    #a<- cbind(y_pre =valid_dt[, pre_treat], upRF.pred)
    #a_y1 <- a %>% as.data.frame() %>% filter(y_pre == 1)
    #a_y0 <- a %>% as.data.frame() %>% filter(y_pre == 0)
    
    #y_1.dt <- subset(valid_dt, valid_dt[, pre_treat]==1)
    #y_0.dt <- subset(valid_dt, valid_dt[, pre_treat]==0)
    
    #y1_perf <- performance(a_y1$pr.y1_ct1, a_y1$pr.y1_ct0, y_1.dt[,treat_result] %>% as.numeric(), y_1.dt[ ,treatment] %>% as.numeric(), direction = 1)
    #y0_perf <- performance(a_y0$pr.y1_ct1, a_y0$pr.y1_ct0, y_0.dt[,treat_result] %>% as.numeric(), y_0.dt[ ,treatment] %>% as.numeric(), direction = 1)
    
    #y1_exp <- upliftDecile(y1_perf)
    #y0_exp<- upliftDecile(y0_perf)
    
   
    res_gain.ratio_all[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    #res_gain.ratio_ypre.1[,i][1:length(y1_exp$gain_ratio)]<- y1_exp$gain_ratio
    #res_gain.ratio_ypre.0[,i][1:length(y0_exp$gain_ratio)]<- y0_exp$gain_ratio
    
    #res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    #res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    
    
  }
  cat("uplift.RF: Finish.",date(),"\n")
  
  list(gain.ratio_all = res_gain.ratio_all
       #gain.ratio_ypre.1 = res_gain.ratio_ypre.1,
       #gain.ratio_ypre.0 = res_gain.ratio_ypre.0
       
       
       #gini=res_gini,
       #gain=res,
       ) %>%
    return()
}

bootstrap.decision.tree<-function(resample_index,dat,treatment, treat_result, uplift_formula ){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  for (i in c(1:ncol(resample_index))) {
    cat("uplift.DT: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-dat[index,]
    valid_dt<-dat[-index,]
    
    upRF.fit<-upliftRF(uplift_formula, data = train_dt, mtry=ncol(dat)-2, ntree=1, bag.fraction = 1, split_method="ED", minsplit=200, verbose=TRUE)
    upRF.pred<-predict(upRF.fit, newdata=valid_dt)
    perf <- performance(upRF.pred[, 1], upRF.pred[, 2], valid_dt[,treat_result], valid_dt[ ,treatment], direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("uplift.DT: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}
bootstrap.upliftKNN<-function(resample_index,dat,treatment, treat_result ){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  for (i in c(1:ncol(resample_index))) {
    cat("uplift.KNN: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-dat[index,]
    valid_dt<-dat[-index,]
    
    upKNN.fit<-upliftKNN(train = train_dt, test =valid_dt, y = train_dt[,treat_result], ct = train_dt[ ,treatment], dist.method = "euclidean", k = 4, agg.method = "majority" )
    perf<-performance(upKNN.fit[,2],upKNN.fit[,1], valid_dt[,treat_result], valid_dt[ ,treatment], direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("uplift.KNN: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}

bootstrap.knn<-function(resample_index,dat,treatment, treat_result){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  dat[,treat_result]<-factor(dat[,treat_result])
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  dmy <- dummyVars(" ~ .", data = df[,-ncol(df)])
  df2 <- data.frame(predict(dmy, newdata = df))%>%
    cbind(., y=df$y)
  
  for (i in c(1:ncol(resample_index))) {
    cat("oma.knn: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df2[index,]
    valid_dt<-df2[-index,]
    
    # normalization_training
    train_dt.norm<-train_dt
    
    norm.value<-preProcess(train_dt[,-ncol(train_dt)], method = c("center", "scale")) 
    train_dt.norm[,-ncol(train_dt.norm)]<-predict(norm.value, train_dt[,-ncol(train_dt)])
    
    ### treatment=1
    valid_dt$ct.0<-0
    valid_dt$ct.1<-1
    
    # normalization_validat_1
    valid_dt.norm_1<-valid_dt
    valid_dt.norm_1[,-ncol(valid_dt.norm_1)]<-predict(norm.value, valid_dt[,-ncol(valid_dt)])
    
    model.fit <- train(y ~ ., data = train_dt.norm, method = "knn", tuneGrid = data.frame(k=3))
    model.pred<- predict(model.fit,newdata = valid_dt.norm_1, type="prob" )
    
    
    ### treatment=0
    valid_dt$ct.0<-1
    valid_dt$ct.1<-0
    
    # normalization_validaton_0
    valid_dt.norm_0<-valid_dt
    valid_dt.norm_0[,-ncol(valid_dt.norm_0)]<-predict(norm.value, valid_dt[,-ncol(valid_dt)])
    
    model.pred_0<- predict(model.fit,newdata = valid_dt.norm_0, type="prob" )
    
    perf <- performance(model.pred$`1`, model.pred_0$`1`, valid_dt[,"y"]%>% as.character()%>%as.numeric(), df[-index,"ct"]%>% as.character()%>%as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("oma.knn: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}
bootstrap.lm<-function(resample_index,dat,treatment, treat_result){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()

  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  for (i in c(1:ncol(resample_index))) {
    cat("oma.lm: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
    model.fit<-glm(y~., data = train_dt, family = binomial(link='logit'))
    
    valid_dt_2<-valid_dt
    valid_dt_2$ct<-1
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred<-predict(model.fit, valid_dt_2, type="response")
    
    valid_dt_2$ct<-0
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred_0<-predict(model.fit, valid_dt_2, type="response")
    
    perf <- performance(model.pred, model.pred_0, valid_dt[,"y"]%>% as.character()%>%as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>%as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("oma.lm: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}
bootstrap.dtree<-function(resample_index,dat,treatment, treat_result){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()

  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  for (i in c(1:ncol(resample_index))) {
    cat("oma.dtree: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
    model.fit<-rpart(y~., data = train_dt, method = "class")
    
    valid_dt_2<-valid_dt
    valid_dt_2$ct<-1
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred<-predict(model.fit, valid_dt_2, type="prob")
    
    valid_dt_2$ct<-0
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred_0<-predict(model.fit, valid_dt_2, type="prob")
    

    perf <- performance(model.pred[,2], model.pred_0[,2], valid_dt[,"y"]%>% as.character()%>%as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>%as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("oma.dtree: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}
bootstrap.oma.RF<-function(resample_index,dat,treatment, treat_result){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()

  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  for (i in c(1:ncol(resample_index))) {
    cat("oma.RF: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
    model.fit<-randomForest(y~., data = train_dt,ntree=100)
    
    valid_dt_2<-valid_dt
    valid_dt_2$ct<-1
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred<-predict(model.fit, valid_dt_2, type="prob")
    
    valid_dt_2$ct<-0
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred_0<-predict(model.fit, valid_dt_2, type="prob")
    
    
    perf <- performance(model.pred[,2], model.pred_0[,2], valid_dt[,"y"]%>% as.character()%>%as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>%as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("oma.RF: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}

bootstrap.lasso.lm<-function(resample_index,dat,treatment, treat_result){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  for (i in c(1:ncol(resample_index))) {
    cat("oma.lm: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
    model.fit<-train(y~., data = train_dt, method="glmnet", family ="binomial", tuneGrid=expand.grid(.alpha=1,.lambda=seq(0, 100, by = 0.1)), preProcess = c("center","scale"))
       
    
    valid_dt_2<-valid_dt
    valid_dt_2$ct<-1
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred<-predict(model.fit, valid_dt, type="prob") 
    
    valid_dt_2$ct<-0
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    model.pred_0<-predict(model.fit, valid_dt, type="prob") 
    
    perf <- performance(model.pred$`1`, model.pred_0$`1`, valid_dt[,"y"]%>% as.character()%>%as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>%as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("oma.lm: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}



####### Two model approach - Indirectly modeling
bootstrap.tma.lm<-function(resample_index,dat,treatment, treat_result ){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  for (i in c(1:ncol(resample_index))) {
    cat("tma.lm: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
    model.fit.trt<-glm(y~.,  subset(train_dt, train_dt$ct==1, select = -ct), family = binomial(link='logit'))
    model.fit.ctrl<-glm(y~.,  subset(train_dt, train_dt$ct==0, select = -ct), family = binomial(link='logit'))
    
    model.pred.trt<-predict(model.fit.trt, valid_dt, type="response")
    model.pred.ctrl<-predict(model.fit.ctrl, valid_dt, type="response")
    
    perf <- performance(model.pred.trt, model.pred.ctrl, valid_dt[,"y"]%>% as.character()%>% as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>% as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("tma.lm: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}
bootstrap.tma.lasso.lm<-function(resample_index,dat,treatment, treat_result ){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  for (i in c(1:ncol(resample_index))) {
    cat("tma.lm: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
  
    model.fit.trt<-train(y~., data = subset(train_dt, train_dt$ct==1, select = -ct), method="glmnet", family ="binomial", tuneGrid=expand.grid(.alpha=1,.lambda=seq(0, 100, by = 0.1)), preProcess = c("center","scale"))
    model.fit.ctrl<-train(y~., data = subset(train_dt, train_dt$ct==0, select = -ct), method="glmnet", family ="binomial", tuneGrid=expand.grid(.alpha=1,.lambda=seq(0, 100, by = 0.1)), preProcess = c("center","scale"))
    
    model.pred.trt<-predict(model.fit.trt, valid_dt, type="prob")
    model.pred.ctrl<-predict(model.fit.ctrl, valid_dt, type="prob")
    
    perf <- performance(model.pred.trt$`1`, model.pred.ctrl$`1`, valid_dt[,"y"]%>% as.character()%>% as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>% as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("tma.lm: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}

bootstrap.tma.knn<-function(resample_index,dat,treatment, treat_result){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  dmy <- dummyVars(" ~ .", data = df[,-ncol(df)])
  df2 <- data.frame(predict(dmy, newdata = df))%>%
    cbind(., y=df$y)
  for (i in c(1:ncol(resample_index))) {
    cat("tma.knn: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df2[index,]
    valid_dt<-df2[-index,]
    
    # normalization_training
    train_dt.norm<-train_dt
    valid_dt.norm<-valid_dt
    
    
    norm.value<-preProcess(train_dt[,-ncol(train_dt)], method = c("center", "scale")) 
    train_dt.norm[,-ncol(train_dt.norm)]<-predict(norm.value, train_dt[,-ncol(train_dt)])
    valid_dt.norm[,-ncol(valid_dt.norm)]<-predict(norm.value, valid_dt[,-ncol(valid_dt)])
    
    model.fit.trt <- train(y ~ ., subset(train_dt.norm,train_dt.norm$ct.1>0,                       # choosing ct1 =1 (after nomalization the value is 0.999 and -0.999)
                                     select = -c(ct.0, ct.1) ), method = "knn", tuneGrid = data.frame(k=3))
    
    model.fit.ctrl <- train(y ~ ., subset(train_dt.norm,train_dt.norm$ct.0 >0,                       # choosing ct1 =1 (after nomalization the value is 0.999 and -0.999)
                                     select = -c(ct.0, ct.1) ), method = "knn", tuneGrid = data.frame(k=3))
    
    
    model.pred.trt<- predict(model.fit.trt,newdata = valid_dt.norm, type="prob" )
    model.pred.ctrl<- predict(model.fit.ctrl,newdata = valid_dt.norm, type="prob" )
    
    
    perf <- performance(model.pred.trt[,2], model.pred.ctrl[,2], valid_dt[,"y"]%>% as.character()%>%as.numeric(), df[-index,"ct"]%>% as.character()%>%as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("tma.knn: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}
bootstrap.tma.dtree<-function(resample_index,dat,treatment, treat_result ){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  for (i in c(1:ncol(resample_index))) {
    cat("tma.dtree: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
    model.fit.trt<-rpart(y~., subset(train_dt, train_dt$ct==1, select = -ct), method = "class")
    model.fit.ctrl<-rpart(y~., subset(train_dt, train_dt$ct==0, select = -ct), method = "class")
    
    model.pred.trt<-predict(model.fit.trt, valid_dt, type="prob")
    model.pred.ctrl<-predict(model.fit.ctrl, valid_dt, type="prob")
    
    perf <- performance(model.pred.trt[, 2], model.pred.ctrl[, 2], valid_dt[,"y"]%>% as.character()%>% as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>% as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("tma.dtree: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}
bootstrap.tma.RF<-function(resample_index,dat,treatment, treat_result ){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  for (i in c(1:ncol(resample_index))) {
    cat("tma.RF: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    
    model.fit.trt<-randomForest(y~., subset(train_dt, train_dt$ct==1, select = -ct),ntree=100,mtry=3)
    model.fit.ctrl<-randomForest(y~., subset(train_dt, train_dt$ct==0, select = -ct),ntree=100,mtry=3)
    
    
    model.pred.trt<-predict(model.fit.trt, newdata=valid_dt, type="prob") 
    model.pred.ctrl<-predict(model.fit.ctrl, newdata=valid_dt, type="prob") 
    
    perf <- performance(model.pred.trt[, 2], model.pred.ctrl[, 2], valid_dt[,"y"]%>% as.character()%>% as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>% as.numeric(), direction = 1)
    
    
    
    # calculating the incremental gains
    experiment<-data.frame("k"=c(0,seq(1,nrow(perf),by = 1)), "n_t"=c(0,perf[,2]),"n_c"=c(0,perf[,3]),
                           "n_t.y1"=c(0,perf[,4]), "n_c.y1"= c(0,perf[,5]), 
                           "r_t.y1"=c(0,perf[,6]), "r_c.y1"=c(0,perf[,7]) ,"u_k"=c(0,perf[,8]),
                           "cum.n_t"= c(0,cumsum(perf[,2])),"cum.n_c"= c(0,cumsum(perf[,3])),
                           "cum.n_t.y1"= c(0,cumsum(perf[,4])),"cum.n_c.y1"= c(0,cumsum(perf[,5])))%>%
      mutate(cum.r_t.y1 = (cum.n_t.y1/cum.n_t)%>% replace(., is.na(.),0))%>%
      mutate(cum.r_c.y1 = (cum.n_c.y1/cum.n_c)%>% replace(., is.na(.),0))%>%
      mutate(U_k = cum.r_t.y1 - cum.r_c.y1 )%>%
      mutate(gain = U_k*(cum.n_t + cum.n_c))%>%
      mutate(gain_ratio = gain/gain[length(k)])%>%
      mutate(gini=cumsum(U_k/U_k[length(U_k)] - cum.n_t/cum.n_t[length(cum.n_t)]))
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("tma.RF: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}




resample.result<-bootstrap.dt( treatment = voter.1.1.1.1$MESSAGE_A, response = voter.1.1.1.1$POLITICALC, n_booststrap = 2)


bootstrap.result<-bootstrap.knn(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")
bootstrap.result<-bootstrap.lm(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")
bootstrap.result<-bootstrap.dtree(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")
bootstrap.result<-bootstrap.upliftKNN(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")
bootstrap.result<-bootstrap.tma.knn(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")
bootstrap.result<-bootstrap.tma.dtree(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")
bootstrap.result<-bootstrap.tma.RF(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")
bootstrap.result<-bootstrap.oma.RF(resample_index = resample.result, dat =voter.e5, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")


bootstrap.result<-bootstrap.lasso.lm(resample_index = resample.result, dat =voter.1.1.1.1, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")





cv.fit <- cv.glmnet(x = as.matrix(train[, x]), 
                    y = as.matrix(train[, y]), 
                    alpha = alpha,
                    family = "gaussian",
                    type.measure = 'mse')

# find best lambda to decide final fitting model
best.lambda <- cv.fit$lambda.1se

# forecast numbers
pred <- predict(cv.fit, s = best.lambda, newx = as.matrix(predictor[,x])) %>%
  as.vector()

# fitted values
fit.val <- predict(cv.fit, s = best.lambda, newx = as.matrix(train[,x])) %>%
  as.vector()

# model summary
cv.fit$best.vars <- coef(cv.fit, s='lambda.1se')






bootstrap.glm<-function(resample_index,dat,treatment, treat_result){
  
  # store the result
  # create the empty column to store the value
  # gradually fill in the value
  
  
  res_gain<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gain.ratio<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  res_gini<-matrix(nrow = 11, ncol = ncol(resample_index), vector())%>% data.frame()
  
  df<-getData(df =  dat, predictors = colnames(dat[,-which(colnames(dat)%in% c(treatment, treat_result))]),targetVariable =treat_result,treatmentVariable = treatment )
  df$y<-factor(df$y, levels = c(0,1))
  df$ct<-factor(df$ct, levels = c(0, 1))
  
  for (i in c(1:ncol(resample_index))) {
    cat("oma.lm: starting.",date(),"\n")
    
    index<-resample_index[,i]
    train_dt<-df[index,]
    valid_dt<-df[-index,]
    

    cv.fit <- cv.glmnet(x = data.matrix(train_dt[, -which(colnames(train_dt) == "y")]), 
                        y = data.matrix(train_dt[, "y"]), 
                        alpha = 1,
                        family = "binomial",
                        type.measure = 'mse')
    
    best.lambda <- cv.fit$lambda.1se
    
    
    
    valid_dt_2<-valid_dt
    valid_dt_2$ct<-1
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))

    model.pred <- predict(cv.fit, s = best.lambda, newx = data.matrix(valid_dt_2[,-which(colnames(valid_dt_2)== "y")]), type = "response")
    
    valid_dt_2$ct<-0
    valid_dt_2$ct<-factor(valid_dt_2$ct, levels = c(0,1))
    
    model.pred_0 <- predict(cv.fit, s = best.lambda, newx = data.matrix(valid_dt_2[,-which(colnames(valid_dt_2)== "y")]), type = "response")
    
    perf <- performance(model.pred, model.pred_0, valid_dt[,"y"]%>% as.character()%>%as.numeric(), valid_dt[ ,"ct"]%>% as.character()%>%as.numeric(), direction = 1)
    
    # calculating the incremental gains
    experiment<-upliftDecile(perf)
    
    res_gain[,i][1:length(experiment$gain_ratio)]<- experiment$gain
    res_gain.ratio[,i][1:length(experiment$gain_ratio)]<- experiment$gain_ratio
    res_gini[,i][1:length(experiment$gain_ratio)]<- experiment$gini
    
    
  }
  cat("oma.lm: Finish.",date(),"\n")
  
  list(gain=res_gain,
       gain.ratio = res_gain.ratio,
       gini=res_gini)%>%
    return()
}


bootstrap.result<-bootstrap.glm(resample_index = resample.result, dat =voter.1.1.1.1, treatment = "MESSAGE_A", treat_result = "MOVED_AD_NUM")


library(glmnet)
