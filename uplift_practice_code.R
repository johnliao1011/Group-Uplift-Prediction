setwd("C:/Users/User/Desktop/研究所/碩二上/論文")

library("uplift")

voter.df<-read.csv("Voter-Persuasion.csv")

voter.df$MOVED_AD_NUM<-ifelse(voter.df$MOVED_AD=="Y", 1,0)

set.seed(1)

#partition
train.index<-sample(c(1:dim(voter.df)[1]), dim(voter.df)[1]*0.6)
train.df<-voter.df[train.index,]
valid.df<-voter.df[-train.index,]

# apply UpliftRF
upRF.fit<-upliftRF(MOVED_AD_NUM~AGE+NH_WHITE+COMM_PT+H_F1+REG_DAYS+PR_PELIG+E_PELIG+POLITICALC+trt(MESSAGE_A), data = train.df, mtry=3, ntree=100, split_method="KL", minsplit=200, verbose=TRUE)

upRF.pred<-predict(upRF.fit, newdata=valid.df)
result<-data.frame(upRF.pred, "uplift"=upRF.pred[,1]-upRF.pred[,2])
overall.uplift<-mean(result$uplift)

varImportance(upRF.fit)
#---------------------------------------

# subset into set 1, 2, 3
set_1<-subset(voter.df, voter.df$SET_NO==1)

upRF.pred.1<-predict(upRF.fit, newdata=set_1)
result.1<-data.frame(upRF.pred.1, "uplift"=upRF.pred.1[,1]-upRF.pred.1[,2])
set.1.uplift<-mean(result.1$uplift)

#---------------------------------------

set_2<-subset(voter.df, voter.df$SET_NO==2)

upRF.pred.2<-predict(upRF.fit, newdata=set_2)
result.2<-data.frame(upRF.pred.2, "uplift"=upRF.pred.2[,1]-upRF.pred.2[,2])
set.2.uplift<-mean(result.2$uplift)

#----------------------------------------
set_3<-subset(voter.df, voter.df$SET_NO==3)

upRF.pred.3<-predict(upRF.fit, newdata=set_3)
result.3<-data.frame(upRF.pred.3, "uplift"=upRF.pred.3[,1]-upRF.pred.3[,2])
set.3.uplift<-mean(result.3$uplift)

#------------------------------------------------------------------------------
diff.set.result<-data.frame(
  Object=c("Set_1", "Set_2", "Set_3", "Overall"),
  n= c(dim(set_1)[1],dim(set_2)[1],dim(set_3)[1],dim(voter.df)[1] ),
  uplift=c(set.1.uplift, set.2.uplift, set.3.uplift, overall.uplift)
)

#--------------------------------------------------------------------------------

#gender
male<-subset(voter.df, voter.df$GENDER_M==1)
female<-subset(voter.df, voter.df$GENDER_F==1)

#-----------------------------------------------
upRF.pred.male<-predict(upRF.fit, newdata=male)
result.male<-data.frame(upRF.pred.male, "uplift"=upRF.pred.male[,1]-upRF.pred.male[,2])
male.uplift<-mean(result.male$uplift)

upRF.pred.female<-predict(upRF.fit, newdata=female)
result.female<-data.frame(upRF.pred.female, "uplift"=upRF.pred.female[,1]-upRF.pred.female[,2])
female.uplift<-mean(result.female$uplift)
#------------------------------------------------
diff.gender.result<-data.frame(
  Gender=c("Male", "Female", "Overall"),
  n= c(dim(male)[1],dim(female)[1], dim(voter.df)[1] ),
  uplift=c(male.uplift, female.uplift, overall.uplift))
