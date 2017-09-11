
####loading all packages####
library(mice)
library(VIM)
library(ParallelForest)
library(randomForest)
library(ggplot2)
library(dplyr)
library(GGally)
library(e1071)
library(glmnet)
library(randomForest)
library(earth)
library(mgcv)
library(bartMachine)
library(gbm)





                                                      ########## DATA CLEANING AND EDA #######################


####reading input file#####
list.files()
cbec2012<-read.csv("2012_public_use_data_aug2016.csv",header = T,stringsAsFactors = F)



length(sub)



####defining variables to keep and converting to factor and numeric accordingly####


num_var0<-c('BASEMNT','BOILP','CDD65','COOLP','COPIERN','DHBTU','DHOTBTU','EDSEAT','ELHTBTU','EVAPP','FKHTBTU','HDD65','HEATP',
            'HTPHP','MFHTBTU','NELVTR','NESLTR','OTHTP','PKGHP','PRNTRN','RCACP',
            'SLFCNP','SQFT','STHWP','WKHRS','XRAYN','NGHTBTU','NFLOOR')
fact_var0<-c('BLDSHP','BLRDUCT','BOILER','CENDIV','CHILLR','CHLDUCT','COHT1','DCWDUCT','DHAIR','DHDUCT','DHFNCL','DHINDC','DHMANU','DHOTH'
             ,'DHPKG','DHRAD','ECN','ELHT1','EVAPCL','FKHT1','FREESTN','GLSSPC','HPHAIR','HPHBKUP','HPHDUAL','HPHGRD','HPHMINI','HPHPKG'
             ,'HPHROOM','HPHSPLT','HPHVRF','HT1','HTLS50','HTPMPC','HTPMPH','HTPOOL','HTRCHLR'
             ,'HWRDHT','LNHRPC','LOHRPC','MAINCL','MAINHT','NWMNHT','OTCLEQ','OTDUCT','OTHT1','OTHTEQ'
             ,'OTPIU','OTSTRP','PBA','PKGHT','PKGHTP','PKGHTTYP','PKGPIU','PUBCLIM','RCAC','RDHTNF','REGION','RFCNS','SLFCON','SOHT1'
             ,'SOUSED','STHT1','STHW','WHRECOV','WLCNS','WOHT1','ELUSED','ELHT2','ELCOOL','ELWATR','ELCOOK','ELMANU','NGGENR','FKGENR',
             'TVVIDEO', 'YRCONC','PRGENR','WOGENR','COGENR','SOGENR','OTGENR','GENUSE','NGHT1','NFLOOR','SERVERC','RFGCOMP','RFGCOMP')
num_var1<-c('ACWNWP','DHCKBTU','DHHTBTU','FURNP','PCTERMN','LAPTPN','NWKER')
fact_var1<-c('ACWNWL','DHCOOK','DHHT1','FURNAC','PCTRMC','LAPTPC','NWKERC')




data1<-cbec2012[cbec2012$REGION=='1',]
##in YRCON there is an observation '995'. This refers to 'buildings before 1946. We need to drop this column
##'NFLOOR','SERVERN','LODGRM','NELVTR','NESLTR','RFGCOMPN','TVVIDEON' needs to be recategorised



col_tokeep<-c(num_var0,num_var1,fact_var0,fact_var1)


table(data1$YRCON)

data1[c(fact_var0,fact_var1,imputed_cl)]<-lapply(data1[c(fact_var0,fact_var1,imputed_cl)], function(x)(as.character(x)))


####Data Transformation####

##Some perenatge values were used in conjunction with character. Wherever 'No'/'2' set to 0
i<-1
for( i in 1:length(num_var1))
{
  num_var11<-num_var1[i]
  fact_var11<-fact_var1[i]
  print(fact_var11)
  print(table(data1[,num_var11]>0,data1[,fact_var11],useNA = 'always'))
}###Based on tabular output 'ACWNL',DHHT1,'FURNAC' can be transforemed in case of discrepancy#
data1$ACWNWL

data1$ACWNWP[ifelse(data1$ACWNWL=='2' & !is.na(data1$ACWNWL),T,F)]<-rep(0,sum(data1$ACWNWL=='2',na.rm = T))
table(data1$ACWNWL,data1$ACWNWP>0,useNA = 'always')


data1$DHHTBTU[ifelse(data1$DHHT1=='2' & !is.na(data1$DHHT1),T,F)]<-rep(0,sum(data1$DHHT1=='2',na.rm = T))
table(data1$DHHTBTU>0,data1$DHHT1,useNA = 'always')

data1$FURNP[ifelse(data1$FURNAC=='2' & !is.na(data1$FURNAC),T,F)]<-rep(0,sum(data1$FURNAC=='2',na.rm = T))
table(data1$FURNP>0,data1$FURNAC,useNA = 'always')

##Finding all imputed columns
imputed_cl<-colnames(cbec2012)[grep('^Z',colnames(cbec2012))]
sub<-substr(imputed_cl,2,length(imputed_cl))

imputed_cl<-imputed_cl[sub %in% c(fact_var1,fact_var0)]
sub<-sub[sub %in% c(fact_var1,fact_var0)]

data1<-data1[,colnames(data1) %in% c(col_tokeep,imputed_cl)]


##Removing variables with heavy missing values(>20%)
miss<-sapply(data1,function(x)(sum(is.na(x))))
drop_col<-names(miss)[miss>0.2*nrow(data1)]
data2<-data1[,!colnames(data1)%in% drop_col]##dropping column with >20% missing value
miss<-sapply(data2,function(x)(sum(is.na(x))))
miss1<-miss[names(miss) %in% c(fact_var0,fact_var1) & miss>0]## analyzing missing values for variables that to keep

# FREESTN  BLDSHP  GLSSPC   ELHT1   ELHT2  FURNAC   PKGHT  BOILER    STHW  HTPMPH  SLFCON  OTHTEQ  MAINHT  ELCOOL    RCAC 
# 126     126     126      21      21      44      44      44      44      44      44      44      44      21     144 
# CHILLR  HTPMPC  ACWNWL  EVAPCL  OTCLEQ  MAINCL  RDHTNF  ELWATR  ELCOOK  ELMANU  PCTRMC  LAPTPC SERVERC  LOHRPC  LNHRPC 
# 144     144     144     144     144     144      46      21      21      21      31      31      31      38      50 

##As per survey input, missing 'freestn' is 'No'
data2$FREESTN[is.na(data2$FREESTN)]<-'0'


##sum(substr(imputed_cl,2,length(imputed_cl))==sub)
#sapply(cbec2012[cbec2012$REGION=='1',colnames(cbec2012) %in% imputed_cl],function(x)(table(x)))


## We will do imputation later. lets first select top predictors for our model
data2<-data2[,colnames(data2) %in% col_tokeep]
data2<-data2[complete.cases(data2),]




##Convertying character to factor
str(data2)
fact_convert<-colnames(data2)[colnames(data2) %in% c(fact_var0,fact_var1)]
data2[fact_convert]<-lapply(data2[fact_convert], function(x)(as.factor(x)))
str(data2)


#####variable selection on fact_var0,num_var1,num_var1 variables based on rf_model####
##before imputing variable further lets fit a random forest and find out best 15-20 predictors

#####variable selection on fact_var0,num_var0,num_var1 variables
data3<-data2[complete.cases(data2),]
data3<-data3[data3$ELHTBTU>0,!colnames(data3) %in% c(fact_var1,'REGION')]

data3$logELHTBTU<-log(data3$ELHTBTU)###removing cases of ELHTBTU==0
summary(data3$logELHTBTU)
summary(data3$ELHTBTU)
qqnorm(data3$logELHTBTU)##normal data
qqnorm(data3$ELHTBTU)##highly skerwed data



set.seed(101)
rf<-randomForest(logELHTBTU~.-ELHTBTU,data=data3,ntree=1000,importance=T)
varImpPlot(rf)
varimp<-data.frame(rf$importance)
varimp<-varimp[order(varimp$X.IncMSE,decreasing = T),]
varimp$var<-row.names(varimp)
dropcol<-varimp$var[varimp$X.IncMSE<0]

dropcol<-dropcol[-5]
#"ELCOOL"  "RCAC"    "SERVERC" "RDHTNF"  "TVVIDEO" "CENDIV" 
#"EVAPCL" "SOUSED" "CDD65"  "WLCNS"  
data3<-data3[,!colnames(data3) %in% dropcol]
cbind(table(cbec2012$ELHTBTU>1,cbec2012$ELHT1),table(cbec2012$ELHTBTU>1,cbec2012$ELHT2))

mean(rf$rsq)##0.35
mean(sqrt(rf$mse))
mean(data3$EUI)

write.csv(varimp,'varimp2.csv')




data3<-data2[complete.cases(data2),]
data3<-data3[data3$ELHTBTU>0,!colnames(data3) %in% c(num_var1,'REGION')]

data3$logELHTBTU<-log(data3$ELHTBTU)###removing cases of ELHTBTU==0
summary(data3$logELHTBTU)
summary(data3$ELHTBTU)
qqnorm(data3$logELHTBTU)##normal data
qqnorm(data3$ELHTBTU)##highly skerwed data



set.seed(101)
rf<-randomForest(logELHTBTU~.-ELHTBTU,data=data3,ntree=1000,importance=T)
varImpPlot(rf)
varimp<-data.frame(rf$importance)
varimp<-varimp[order(varimp$X.IncMSE,decreasing = T),]
varimp$var<-row.names(varimp)
dropcol<-varimp$var[varimp$X.IncMSE<0]
dropcol## making sure 'glsspc' comes singinificant
#"RCAC"    "ELMANU"  "TVVIDEO" "CENDIV"  "HDD65"   "WLCNS"
#"ELCOOL" "SOUSED" "CHILLR" "RDHTNF"
#"OTCLEQ"
data3<-data3[,!colnames(data3) %in% dropcol]

#cbind(table(cbec2012$ELHTBTU>1,cbec2012$ELHT1),table(cbec2012$ELHTBTU>1,cbec2012$ELHT2))

write.csv(varimp,'varimp2.csv')

mean(rf$rsq)##0.35
mean(sqrt(rf$mse))

##Final columns to be further considered and all methods,exploratory data analysis  need to be tested
#"PBA"        "FREESTN"    "SQFT"       "WLCNS"      "RFCNS"      "BLDSHP"     "GLSSPC"     "NFLOOR"     "YRCON"     
#[10] "WKHRS"      "NWKERC"     "HT1"        "ELUSED"     "ELHT1"      "ELHT2"      "HEATP"      "FURNAC"     "PKGHT"     
#[19] "BOILER"     "STHW"       "HTPMPH"     "SLFCON"     "OTHTEQ"     "MAINHT"     "COOLP"      "HTPMPC"     "ACWNWL"    
#[28] "MAINCL"     "ELWATR"     "ELCOOK"     "PCTRMC"     "LAPTPC"     "PRNTRN"     "SERVERC"    "LOHRPC"     "LNHRPC"    
#[37] "HDD65"      "MFHTBTU"    "ELHTBTU"    "PUBCLIM"    "logELHTBTU"

final_col<-c(colnames(data3),'HDD65')




###We will go with fact_var1,num_var0,fact_var0 variable sit keeps hypothised variables
#However, this further needs to be investigated 
fact_convert<-colnames(data1)[colnames(data1) %in% c(fact_var0,fact_var1)]
data1[fact_convert]<-lapply(data1[fact_convert], function(x)(as.factor(x)))

data_clean<- data1[complete.cases(data1$ELHTBTU),colnames(data1) %in% final_col]
data_clean<- data_clean[data_clean$ELHTBTU>0,]
## ELHTBTU=0 can be predicted be looking at ELHT1 and ELHT2. Both are equal 0 when ELHTBTU=0
# data1%>%group_by(PBA)%>%summarise(mean=mean(EUI),sd=sd(EUI),count=n())
# data1%>%group_by(PBA)%>%summarise(median=median(EUI),IQR=IQR(EUI),count=n())
# 
# table(data1$ELHT1,data1$ELHT2)
# cbind(table(data1$ELHTBTU,data1$ELHT1),table(data1$ELHTBTU,data1$ELHT2))

str(data_clean1)



####checking missing values pattern#####
md.pattern(data_clean)
aggr_plot <- aggr(data_clean, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data_clean), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

table(data1$NWKER,useNA = 'always')

data_clean$FREESTN<-ifelse(is.na(data_clean$FREESTN),'0',data_clean$FREESTN) ##As per survey input, missing 'freestn' is 'No'
data_clean$FREESTN<-as.factor(data_clean$FREESTN)

boxplot(data_clean$logELHTBTU~data_clean$FREESTN)
data_clean$logELHTBTU<-log(data_clean$ELHTBTU)## defining log(ELHTBTU)
par(mfrow=c(2,2))
par(mar=c(2,3,2,3))
hist(data_clean$ELHTBTU,main='histogram of response=ELHTBTU',cex.lab=0.8,cex.main=0.8,cex.axis=0.5)
qqnorm(data_clean$ELHTBTU,main='normal plot of response=ELHTBTU',cex.lab=0.8,cex.main=0.8,cex.axis=0.5)
hist(data_clean$logELHTBTU,main='histogram of transfr. resp=log(ELHTBTU)',cex.lab=0.8,cex.main=0.8,cex.axis=0.5)
qqnorm(data_clean$logELHTBTU,main='normal plot of transfr. resp=log(ELHTBTU)',cex.lab=0.8,cex.main=0.8,cex.axis=0.5)
summary(data_clean$ELHTBTU)
summary(data_clean$logELHTBTU)



#########detecting outlier- PBA vs log(ELHTBTU) and preparing final dataset#####
par(mfrow=c(1,1))
data_clean$EUI<-data_clean$ELHTBTU/data_clean$SQFT
boxplot(data_clean$EUI~data_clean$PBA,main='Boxplot of ELHTBTU/SQFT vs PBA',xlab='PBA Categories',ylab='ELHTBTU/SQFT',cex.lab=0.8,cex.main=0.8)## we see there are some outliers in each segment that can be removed

summary<-data_clean %>% group_by(PBA)%>%summarise(min=quantile(EUI)[2]-1.5*IQR(EUI),max=quantile(EUI)[4]+1.5*IQR(EUI))

data_clean1<-merge(x=data_clean,y=summary,by='PBA')
data_clean1<-data_clean1%>%filter(EUI>min,EUI<max)
sapply(data_clean1, function(x)(sum(is.na(x))))



num_var<-colnames(data_clean1)[sapply(data_clean1,is.numeric)]
num_var
num_var<-num_var[!num_var %in% c("EUI","min","max","ELHTBTU" )]

summary(data_clean1[num_var])

###These three variables are coming non-linear
data_clean1$logMFHTBTU<-log(data_clean1$MFHTBTU+0.001)
data_clean1$logSQFT<-log(data_clean1$SQFT+0.001)
num_var<-num_var[!num_var %in% c("SQFT","MFHTBTU")]
num_var<-c(num_var,'logSQFT','logMFHTBTU')

ggpairs(data_clean1[num_var])
boxplot(data_clean1$logELHTBTU ~data_clean1$NWKERC,main='boxplot of logEHTBTU vs no of employee category'  )###this appears a better predictor
##logMHTBTU and logSQRT are highly correlated. We will remove this in fitting linear models
##CDD65 and HDD65 are correlated
cor(data_clean1$logELHTBTU,log(data_clean1$HDD65))## will drop hdd5 as no impovement over CDD65
par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(2,2))
hist(data_clean1$SQFT,main='histogram of response=SQFT')
qqnorm(data_clean1$SQFT,main='normal plot of response=SQFT')
hist(data_clean1$logSQFT,main='histogram of transformed response=log(SQFT)')
qqnorm(data_clean1$logSQFT,main='normal plot of transformed response=log(SQFT)')

colnames(data_clean1)
fact_var<-colnames(data_clean1)[sapply(data_clean1, function(x)(is.factor(x)))]
varImpPlot(rf)
par(mar=c(2,2,2,2))
par(mfrow=c(4,4))
for ( i in fact_var)
{
  
  plot(data_clean1$logELHTBTU~data_clean1[,i],main=paste(' Boxplot of logELHTBTU vs',i),cex.main=0.8
       ,cex.axis=0.8,cex.lab=0.8)
}

table(data_clean1$PCTRMC,data_clean1$LAPTPC,useNA='always')
##drop 'SLFCON','HTPMPC','ELWATR','HT1','ELUSED', based on box plot
cor(data_clean1$logELHTBTU,data_clean1$CDD65)
colnames(data_clean1)
data_clean1$GLSSPC<-as.character(data_clean1$GLSSPC)
data_clean1$GLSSPC1<-ifelse(data_clean1$GLSSPC %in% c('1','2'),'1','2')
data_clean1$GLSSPC1<-ifelse(is.na(data_clean1$GLSSPC) ,NA,data_clean1$GLSSPC)
data_clean1$GLSSPC1<-as.factor(data_clean1$GLSSPC1)
###Final dataset
drop_col<-c('EUI','min','max','CDD65','SLFCON','HTPMPC','ELWATR','HT1',
            'ELUSED','SQFT','ELHTBTU','MFHTBTU','RFGCOMP','OTHTEQ','ELHT1','ELHT2','BOILER','FREESTN','GLSSPC','BLDSHP1')


final_data<-data_clean1[,!colnames(data_clean1) %in% drop_col]
sapply(final_data,function(x)(sum(is.na(x))))
final_data<-final_data[complete.cases(final_data),]


####defining cross-validation group####
set.seed(101)
grp<-sample(1:nrow(final_data),10)## we will use this for cross-validation
















                                                    ##################### FUNCTIONS ##################
#####function for insample,outsample and rsquare###########
insample_rmse<-function(mod,df=train,response="logELHTBTU")
{
  sqrt(mean((predict(mod)-df[response])^2))
}

outsample_rmse<-function(mod,df=test,response="logELHTBTU")
{
  sqrt(mean((predict(mod,newdata=df)-df[response])^2))
}

Rsquare<-function(mod,df=train,response="logELHTBTU")
  
{
  mean(sum((predict(mod,newdata=df)-mean(df[,response]))^2))/mean(sum((df[,response]-mean(df[,response]))^2))
  #mean((predict(pruned_rpart,newdata=test)-mean(test[,"Yield"]))^2)/mean((test[,"Yield"]-mean(test[,"Yield"]))^2)
}

####This function makes sure that levels are uniform at both train and test set####
uniform_level<-function(training=train,testing=test)
{
  for(attr in colnames(training))
  {
    if (is.factor(training[[attr]]))
    {
      new.levels <- setdiff(levels(training[[attr]]), levels(testing[[attr]]))
      if ( length(new.levels) == 0 )
      { #print(paste(attr, '- no new levels')) 
      }
      else
      {
        print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
        levels(testing[[attr]]) <- union(levels(testing[[attr]]), levels(training[[attr]]))
      }
    }
  }
  return(list(training,testing))
}





#####Some transformations based on errors observed while running models#####

# final_data<-final_data[!final_data$NWKERC=='1',] ###this creates problem while predictng test
# final_data<-final_data[!final_data$MAINCL=='7',] ###this creates problem while predictng test
# final_data<-final_data[!final_data$LOHRPC=='5',] ###this creates problem while predictng test
# final_data<-final_data[!final_data$PBA=='11',] ###this creates problem while predictng test
# final_data<-final_data[!final_data$NFLOOR=='13',] ###this creates problem while predictng test
# final_data<-final_data[,!colnames(final_data) %in% 'EVAPCL']
# set.seed(101)
# grp<-sample(rep_len(c(1:10),nrow(final_data)))## we will use this for cross-validation










                                                    

                                                    ################# MODELS ####################################

####linear regression####
index<-2
rmse_cv<-NULL
rsq_cv<-NULL
for(index in 1:10)##cv_run
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  lm_cv<-lm(logELHTBTU~.,data=train)
  rmse_cv[index]<-outsample_rmse(lm_cv)
  rsq_cv[index]<-Rsquare(lm_cv)
  print(index)
}

mean(rsq_cv)
mean(rmse_cv)

mod_lm<-lm(logELHTBTU~.,data=final_data)
pred_lm<-mod_lm$fitted.values##final linear model
summary(mod_lm)

# > mean(rsq_cv)
# [1] 0.8393223
# > mean(rmse_cv)
# [1] 1.305363
# > summary(mod_lm)






####stepwise linear regression####

index<-2
rmse_cv<-NULL
rsq_cv<-NULL
for(index in 1:10)
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  step_cv<-step(lm(logELHTBTU~.,data=train),direction = 'both',trace=0)
  rmse_cv[index]<-outsample_rmse(step_cv)
  rsq_cv[index]<-Rsquare(step_cv)
  print(index)
}

mean(rsq_cv)
mean(rmse_cv)



mod_step<-step(lm(logELHTBTU~.,data=final_data),direction = 'both',trace=0)
pred_step<-mod_step$fitted.values
summary(mod_step)







####regularised linear regression#####


##finding best parameters
x <- model.matrix(logELHTBTU ~ 0 + . , data=train)
model_glm0 = cv.glmnet(x,y=train$logELHTBTU,
                       family='gaussian',alpha=0,type.measure = 'mse')
model_glm0.5  = cv.glmnet(x,y=train$logELHTBTU,
                          family='gaussian',alpha=0.5,type.measure = 'mse')
model_glm1  = cv.glmnet(x,y=train$logELHTBTU,
                        family='gaussian',alpha=1,type.measure = 'mse')

model_glm0$
  
  
  index<-2
rmse_cv<-NULL
rsq_cv<-NULL
for(index in 1:10)##cv_run
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  x <- model.matrix(logELHTBTU ~ 0 + . , data=train)
  glm_cv = cv.glmnet(x,y=train$logELHTBTU,
                     family='gaussian',alpha=1,type.measure = 'mse')
  newX=model.matrix(logELHTBTU ~ 0 + . , data=test)
  predtrain<-predict(glm_cv,newx = x,s=glm_cv$lambda.min,alpha=1)
  predtest<-predict(glm_cv,newx = newX,s=glm_cv$lambda.min,alpha=1)
  rmse_cv[index]<-sqrt(mean((test$logELHTBTU-predtest)^2))
  rsq_cv[index]<-sum((predtrain-mean(train$logELHTBTU))^2)/sum((train$logELHTBTU-mean(train$logELHTBTU))^2)
  print(index)
}

mean(rsq_cv)
mean(rmse_cv)

#model_glm0---  0.618,1.155
#model_glm0.5-- 0.622,1.12
#model_glm0---- 0.63  1.12

x <- model.matrix(logELHTBTU ~ 0 + . , data=final_data)
glm_cv = cv.glmnet(x,y=final_data$logELHTBTU,
                   family='gaussian',alpha=0.5,type.measure = 'mse')
pred_reg<-predict(glm_cv,newx = x,s=glm_cv$lambda.min,alpha=0.5)
coef(glm_cv)





####Fitting generalized additive model####

fact<-colnames(final_data)[sapply(final_data,is.factor)]
num<-colnames(final_data)[!colnames(final_data) %in% fact]
p<-'logELHTBTU~'
for ( j in colnames(final_data))
{
  if(is.factor(final_data[,j])) {
    p<-paste(p,'+',j)
  }
  else {
    p<-paste(p,'+ s(',j,')')
  }
}  


model_gam<-gam(logELHTBTU~ + PBA + RFCNS + BLDSHP + NFLOOR + YRCONC + s( WKHRS ) + NWKERC + s( HEATP ) + FURNAC + PKGHT + 
                 STHW + HTPMPH + MAINHT + s( COOLP ) + ACWNWL + MAINCL + ELCOOK + PCTRMC + LAPTPC + s( PRNTRN ) + SERVERC + 
                 LOHRPC + LNHRPC + PUBCLIM  + s( logMFHTBTU ) + s( logSQFT ) + GLSSPC1,data=final_data)

summary(model_gam)

##removing these factors as these are not significant...RFCNS,BLDSHP,FURNAC,maincl,,serverc,pubclim,wkhrs,logMFHTBTU
###trying these variable as linear.....heatp,coolp,prntrn,
model_gam<-gam(logELHTBTU~ + PBA  + NFLOOR + YRCONC  + NWKERC + s( HEATP )  + PKGHT + 
                 STHW + HTPMPH + MAINHT +  COOLP  + ACWNWL  + ELCOOK + PCTRMC + LAPTPC 
               + s( logSQFT ) + GLSSPC1,data=final_data)
pred_gam<-model_gam$fitted.values
summary(model_gam)

index<-1
rmse_cv<-NULL
rsq_cv<-NULL
for(index in 1:10)
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  gam_cv<-gam(logELHTBTU~ + PBA  + NFLOOR + YRCONC  + NWKERC + s( HEATP )  + PKGHT + 
                STHW + HTPMPH + MAINHT +  COOLP  + ACWNWL  + ELCOOK + PCTRMC + LAPTPC 
              + s( logSQFT ) + GLSSPC1,data=train)
  rmse_cv[index]<-outsample_rmse(gam_cv)
  rsq_cv[index]<-Rsquare(gam_cv)
}

mean(rmse_cv)
mean(rsq_cv)
# > mean(rmse_cv)
# [1] 1.119008
# > mean(rsq_cv)
# [1] 0.8217865








#####RandomForest########
ntre<-100; mtr<-3
mtr1<-NULL
ntre1<-NULL
rmse_cv<-NULL
i<-1
for (ntre in c(400,600,1000))
{for (mtr in seq(2,27,2))
{
  rf<-randomForest(logELHTBTU~.,data=final_data,ntree=ntre,mtry=mtr)
  mtr1[i]<-mtr
  ntre1[i]<-ntre
  rsq_cv[i]<-Rsquare(rf,df=final_data)
  rmse_cv[i]<-insample_rmse(rf,df=final_data)
  i<-i+1
  print(i)
}
}

df<-cbind(mtr1,ntre1,rsq_cv,rmse_cv)
df
##Performing 10CV based on best hyperparameters chosen
rmse_cv<-NULL
rsq_cv<-NULL
for(index in 1:10)
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  rf<-randomForest(logELHTBTU~.,data=train,ntree=600,mtry=16)
  rmse_cv[index]<-outsample_rmse(rf,df=test)
  rsq_cv[index]<-Rsquare(rf)
}

mean(rmse_cv)
mean(rsq_cv)
# > mean(rmse_cv)
# [1] 1.179291
# > mean(rsq_cv)
# [1] 0.7016746

rf<-randomForest(logELHTBTU~.,data=final_data,ntree=600,mtry=16)
pred_rf<-rf$predicted
varImpPlot(rf)
summary(rf)
# > mean(rmse_cv)
# [1] 1.179291
# > mean(rsq_cv)
# [1] 0.7016746



######Fitting MARS model#######
library(earth)


penalty_term<-NULL
degree_term<-NULL
nk_term<-NULL
d<-1
i<-1
n=5
pen=0.01
for( d in 1:2)##finding best parameters
{
  for(pen in seq(from=0.01,to=0.20,by=0.04))
  {for (n in seq(from=5,to=55,by=5) )
  {  
    mars_cv<-earth(x=final_data[,!colnames(final_data) %in% 'logELHTBTU'],y=final_data$logELHTBTU,pmethod='cv',nfold=10,degree=d,newvar.penalty=pen,nk=n)
    
    
    penalty_term[i]<-pen
    degree_term[i]<-d
    nk_term[i]<-n
    rmse_cv[i]<-insample_rmse(mars_cv,df=final_data)
    rsq_cv[i]<-Rsquare(mars_cv,df=final_data)
    print(i)
    i<-i+1
  }
    
  }
}


rmse_cv<-NULL
rsq_cv<-NULL
for(index in 1:10)
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  mars_cv<-earth(x=train[,!colnames(train) %in% 'logELHTBTU'],y=train$logELHTBTU,pmethod='cv',nfold=10,degree=2,newvar.penalty=0.01,nk=50)
  rmse_cv[index]<-outsample_rmse(mars_cv)
  rsq_cv[index]<-Rsquare(mars_cv)
}

mean(rmse_cv)
mean(rsq_cv)

mars_cv<-earth(x=train[,!colnames(train) %in% 'logELHTBTU'],y=train$logELHTBTU,pmethod='cv',nfold=10,degree=2,newvar.penalty=0.01,nk=50)
pred_mars<-mars_cv$fitted.values







#####SUpport Vector Machine#####
library(e1071)
svm_tune1 <- tune( svm,logELHTBTU~.,data=final_data
                   , ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)),kernel='radial')
summary(svm_tune1)

svm_tune2 <- tune( svm,logELHTBTU~.,data=final_data
                   , ranges=list(cost=10^(-1:2), degree=c(1,2,3)),kernel='polynomial')
summary(svm_tune2)

svm_tune3 <- tune( svm,logELHTBTU~.,data=final_data
                   , ranges=list(cost=10^(-1:2)),kernel='linear')
summary(svm_tune3)

svm_tune1
svm_tune2
svm_tune3

##Choosing polynomial kernel with degree=2, cost=100 
rmse_cv<-NULL
rsq_cv<-NULL
for(index in 1:10)
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  svm_cv<-svm(logELHTBTU~.,data=train,cost=100,degree=2,kernel='polynomial')
  rmse_cv[index]<-outsample_rmse(svm_cv)
  rsq_cv[index]<-Rsquare(svm_cv)
}

mean(rmse_cv)
mean(rsq_cv)
# > mean(rmse_cv)
# [1] 1.106906
# > mean(rsq_cv)
# [1] 0.8442396
# > summary(svm_cv)

svm_cv<-svm(logELHTBTU~.,data=final_data,cost=100,degree=2,kernel='polynomial')
pred_svm<-svm_cv$fitted.values

summary(svm_cv)






####Fitting BART model#####
library(rJava)
options(java.parameters='-Xmx5000m')
library(bartMachine)
set_bart_machine_num_cores(4)



burn<-100
qt<-0.75
prior<-1
i<-1
print(paste('burn', 'qt', 'prior', 'round(rmse_cv[i],3)','round(rsq_cv[i],3))'))
for( burn in c(100,250))
{
  for( qt in c(0.75,0.99))
  {
    for( prior in c(1,2))
    {
      bart<-bartMachine(X=final_data[,!colnames(final_data) %in% 'logELHTBTU'],
                        y=final_data$logELHTBTU,num_trees = 100,num_burn_in = burn,q=qt,k=prior,verbose=F)
      predfinal_data<-'predict'(bart_cv,new_data=final_data[,!colnames(final_data) %in% 'logELHTBTU'])
      rmse_cv[i]<-sqrt(mean((predfinal_data-final_data$logELHTBTU)^2))
      rsq_cv[i]<-sum((predfinal_data-mean(final_data$logELHTBTU))^2)/
        sum((final_data$logELHTBTU-mean(final_data$logELHTBTU))^2)
      print(paste(burn, qt, prior, round(rmse_cv[i],3),round(rsq_cv[i],3)))
      i<-i+1
    }
  }
}

# [1] "100 0.75 1 0.733 0.748"
# [1] "100 0.75 2 0.733 0.748"
# [1] "100 0.99 1 0.733 0.748"
# [1] "100 0.99 2 0.733 0.748"
# [1] "250 0.75 1 0.733 0.748"
# [1] "250 0.75 2 0.733 0.748"
# [1] "250 0.99 1 0.733 0.748"
# [1] "250 0.99 2 0.733 0.748"

##No change in performance with parameter. we wil use burn=100, qt=0.75,prior=1


rmse_cv<-NULL
rsq_cv<-NULL
index<-1
for(index in 1:10)
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  bart_cv<-bartMachine(X=train[,!colnames(train) %in% 'logELHTBTU'],
                       y=train$logELHTBTU,num_trees = 100,num_burn_in = 100,q=0.75,k=1,verbose=F)
  predtrain<-'predict'(bart_cv,new_data=train[,!colnames(train) %in% 'logELHTBTU'])
  predtest<-'predict'(bart_cv,new_data=test[,!colnames(test) %in% 'logELHTBTU'])
  rmse_cv[index]<-sqrt(mean((predtest-test$logELHTBTU)^2))
  rsq_cv[index]<-mean(sum((bart_cv$y_hat_train-mean(train$logELHTBTU))^2))/
    mean(sum((train$logELHTBTU-mean(train$logELHTBTU))^2))
  
}

mean(rmse_cv)
mean(rsq_cv)
# > mean(rmse_cv)
# [1] 1.09351
# > mean(rsq_cv)
# [1] 0.7998522


mod_bart<-bartMachine(X=final_data[,!colnames(final_data) %in% 'logELHTBTU'],
                      y=final_data$logELHTBTU,num_trees = 100,num_burn_in = 100,q=0.75,k=1,verbose=F)
investigate_var_importance(mod_bart, num_replicates_for_avg = 20,num_var_plot = 15)






####Fit gradient boosted trees####
library(gbm)
i<-1

ntr1<-NULL
bag_fr1<-NULL
depth1<-NULL
shrink1<-NULL
ntr1<-NULL
bag_fr<-NULL
depth<-NULL
shrink<-NULL

for (ntr in c(300,500))
{for(bag_fr in c(0.7,0.9,1))
{ for(depth in c(1,2))
{ for (shrink in c(0.001,0.01,0.05,0.1))
{
  mod_gbm<-gbm(logELHTBTU~.,distribution = 'gaussian',interaction.depth = depth,
               shrinkage = shrink,cv.folds = 1,data=final_data,n.trees = ntr, bag.fraction = bag_fr)
  ntr1[i]<-ntr
  bag_fr1[i]<-bag_fr
  depth1[i]<-depth
  shrink1[i]<-shrink
  rmse[i]<-mean(mod_gbm$train.error)
  rsq[i]<-mean(sum((mod_gbm$fit-mean(final_data$logELHTBTU))^2))/
    mean(sum((final_data$logELHTBTU-mean(final_data$logELHTBTU))^2))
  print(paste(ntr,'   ',bag_fr,'   ',depth,'   ',shrink,'         ',round(rmse[i],2),'          ',round(rsq[i],2)))
  i<-i+1
}
}
}
}

###500,0.7,2,0.1 optimum parameter


rmse_cv<-NULL
rsq_cv<-NULL
index<-1
for(index in 1:10)
{
  train<-final_data[grp != index,]
  test<-final_data[grp == index,]
  ls<-uniform_level(train,test)
  train<-data.frame(ls[1])
  test<-data.frame(ls[2])
  gbm_cv<-gbm(logELHTBTU~.,distribution = 'gaussian',interaction.depth = 2,
              shrinkage = 0.1,cv.folds = 1,data=train,n.trees = 500, bag.fraction = 0.7)
  predtest<-predict(gbm_cv,newdata = test,n.trees=500)
  predtrain<-gbm_cv$fit
  rmse_cv[index]<-sqrt(mean((predtest-test$logELHTBTU)^2))
  rsq_cv[index]<-mean(sum((predtrain-mean(train$logELHTBTU))^2))/
    mean(sum((train$logELHTBTU-mean(train$logELHTBTU))^2))
  
}

mean(rmse_cv)
mean(rsq_cv)

gbm<-gbm(logELHTBTU~.,distribution = 'gaussian',interaction.depth = 2,
         shrinkage = 0.1,cv.folds = 1,data=final_data,n.trees = 500, bag.fraction = 0.7)
pred_gbm<-gbm$fit

summary(gbm)
save(gbm,file="rai16.RData")##saving final optimal model

##Partial Dependance plot
(1:ncol(final_data))[sapply(final_data, is.numeric)]
par(mar=c(2,3,2,3))
par(mfrow=c(3,2))
plot(gbm,i.var=c(6),lwd = 1, col = "blue", main = "Partial Dependency plot of Workhours",cex.axis=0.8,cex.lab=0.8,cex.main=0.9)
plot(gbm,i.var=c(8),lwd = 1, col = "blue", main = "Partial Dependency plot of Heat%",cex.axis=0.8,cex.lab=0.8,cex.main=0.9)
plot(gbm,i.var=c(14),lwd = 1, col = "blue", main ="Partial Dependency plot of Cool%",cex.axis=0.8,cex.lab=0.8,cex.main=0.9)
plot(gbm,i.var=c(20),lwd = 1, col = "blue", main = "Partial Dependency plot of Number of Printers",cex.axis=0.8,cex.lab=0.8,cex.main=0.9)
plot(gbm,i.var=c(25),lwd = 1, col = "blue", main = "Partial Dependency plot of log(MFHTBTU)",cex.axis=0.8,cex.lab=0.8,cex.main=0.9)
plot(gbm,i.var=c(26),lwd = 1, col = "blue", main = "Partial Dependency plot of log(SQFT)",cex.axis=0.8,cex.lab=0.8,cex.main=0.9)



par(mfrow=c(1,1))##variable importance
#barplot(gbm_imp$rel.inf,names.arg = row.names(gbm_imp))
gbm_imp$var<-factor(gbm_imp$var,levels=gbm_imp$var[order(gbm_imp$rel.inf,decreasing = F)])
ggplot(gbm_imp[1:15,],aes(x=var,y=rel.inf))+geom_bar(stat = 'identity')+coord_flip()+labs(title='variable importance plot')









                                                      ############# MODEL SELECTION ###############
####Statistical test to view differences in model performnance#####
resid_allmodels<-data.frame(cbind(pred_bart-final_data$logELHTBTU,pred_gam-final_data$logELHTBTU,pred_gbm-final_data$logELHTBTU,
                                  pred_svm-final_data$logELHTBTU,pred_mars-final_data$logELHTBTU,pred_rf-final_data$logELHTBTU))
colnames(resid_allmodels)<-c('BART','GAM','GBM','SVM','MARS','RF')
library(reshape2)
resid_allmodels$id<-1
resid_allmodels<-melt(resid_allmodels)
resid_allmodels<-resid_allmodels[resid_allmodels$variable!='id',]
resid_allmodels$variable<-as.factor(resid_allmodels$variable)
colnames(resid_allmodels)<-c('models','residuals')

summary(aov(residuals~models,data=resid_allmodels))
pairwise.t.test(resid_allmodels$residuals, resid_allmodels$models , p.adjust="bonferroni")
par(mfrow=c(1,1))
qqnorm(y=pred_gbm-final_data$logELHTBTU,main='qqplot of "gbm" model-residuals',cex.main=1)

###collecting all results and visualizing
par(mar=c(4,4,4,4))
par(mfrow=c(3,3))
plot(pred_lm~final_data$logELHTBTU,main=paste('Actual vs Fitted(linear \nRegression);','Cor =',round(cor(final_data$logELHTBTU, pred_lm),2)),cex.main=1,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)

plot(pred_step~final_data$logELHTBTU,main=paste('Actual vs Fitted(Stepwise \nRegression);','Cor =',round(cor(final_data$logELHTBTU, pred_lm),2)),cex.main=1,xlab='Actual(logELHTBTU)'
     ,ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)


plot(pred_reg~final_data$logELHTBTU,main=paste('Actual vs Fitted(Regularised \nRegression);','Cor =',round(cor(final_data$logELHTBTU, pred_reg),2))
     ,cex.main=1,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)

plot(pred_mars~final_data$logELHTBTU,main=paste('Actual vs Fitted\n(MARS);','Cor =',round(cor(final_data$logELHTBTU, pred_mars),2))
     ,cex.main=01,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)

plot(pred_gam~final_data$logELHTBTU,main=paste('Actual vs Fitted\n(GAM);','Cor =',round(cor(final_data$logELHTBTU, pred_gam),2)),
     cex.main=1,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)

plot(pred_rf~final_data$logELHTBTU,main=paste('Actual vs Fitted\n(Random Forest);','Cor =',round(cor(final_data$logELHTBTU, pred_rf),2))
     ,cex.main=1,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)

plot(pred_gbm~final_data$logELHTBTU,main=paste('Actual vs Fitted\n(GBM);','Cor =',round(cor(final_data$logELHTBTU, pred_gbm),2))
     ,cex.main=1,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)

plot(pred_svm~final_data$logELHTBTU,main=paste('Actual vs Fitted\n(SVM);','Cor =',round(cor(final_data$logELHTBTU, pred_svm),2))
     ,cex.main=1,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)

plot(pred_bart~final_data$logELHTBTU,main=paste('Actual vs Fitted\n(BART);','Cor =',round(cor(final_data$logELHTBTU, pred_bart),2))
     ,cex.main=1,xlab='Actual(logELHTBTU)',ylab='Fitted(logELHTBTU)',cex.axis=0.8,cex.lab=0.8,cex.main=0.9)




