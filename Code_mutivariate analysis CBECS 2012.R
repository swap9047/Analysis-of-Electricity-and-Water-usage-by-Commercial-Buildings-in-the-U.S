
####loading all packages####
list.of.packages <- c("mice",'VIM','ggplot2','dplyr','mvtboost','lattice','gridExtra' )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


####Data Preparation####
list.files()
enrgy_use<-read.csv("2012_public_use_data_aug2016.csv" ,header = T,stringsAsFactors = F)
watr_use<-read.csv("lrgbldg_water_pubuse_102416.csv",header=T,stringsAsFactors = F)




###defining variables to keep and converting to factor and numeric accordingly
enrgy_use1<-enrgy_use[enrgy_use$PUBID %in% watr_use$PUBID,]


##dropping colnames with imputed or finalwt values
colnames(enrgy_use)[446]
enrgy_use1<-enrgy_use1[,colnames(enrgy_use)[c(1:445,1049:1073,1084:1119)]]
watr_use1<-watr_use[,colnames(watr_use)[1:10]]

###checking if common columns in both dataset represnt same values
enrgy_use1<-enrgy_use1[order(enrgy_use1$PUBID),]
watr_use1<-watr_use1[order(watr_use1$PUBID),]
colcheck<-colnames(watr_use1)[-c(9,10)]

for(i in colcheck)
{ print(sum(enrgy_use1[i]!=watr_use1[,i]))}## we observe that all columns represent same value. so we import 'WTUSED' and 'WTCNS' into enrgy _use file

enrgy_use1$WTUSED<-watr_use1$WTUSED
enrgy_use1$WTCNS<-watr_use1$WTCNS
##Now we will deal with 'enrgy_use1' data only since it contains all the reuqired variables

####Data Cleaning####
##finding columns with high missing values and keeping columns that are relevant


enr_num_var0<-c('BASEMNT','BOILP','CDD65','HDD65','COOLP','COPIERN','DHBTU','DHOTBTU','EDSEAT','EVAPP','FKHTBTU','HEATP',
            'HTPHP','MFHTBTU','OTHTP','PKGHP','PRNTRN','RCACP',
            'SLFCNP','SQFT','STHWP','WKHRS','XRAYN','NGHTBTU','NFLOOR','DAYLTP','ELCNS','WTCNS')
enr_fact_var0<-c('CENDIV','BLDSHP','BLRDUCT','BOILER','CHILLR','CHLDUCT','COHT1','DCWDUCT','DHAIR','DHDUCT','DHFNCL','DHINDC',
                'DHMANU','DHOTH','EQGLSS','OWNOCC','PKGCL','CHWT','EMCS','RDCLNF','MAINT','ENRGYPLN','WTHTEQ',
             'RFGEQP','DATACNTR','RGSTR','COPIER','FAX','LED','SCHED','OCSN','DIM','DAYHARV','PLGCTRL','LTEXPC','PKLT','TINT',
              'REFL','DHPKG','DHRAD','ECN','ELHT1','EVAPCL','FKHT1','FREESTN','GLSSPC','HPHAIR','HPHBKUP','HPHDUAL','HPHGRD','HPHMINI','HPHPKG'
             ,'HPHROOM','HPHSPLT','HPHVRF','HT1','HTLS50','HTPMPC','HTPMPH','HTPOOL','HTRCHLR'
             ,'HWRDHT','LNHRPC','LOHRPC','MAINCL','MAINHT','NWMNHT','OTCLEQ','OTDUCT','OTHT1','OTHTEQ'
             ,'OTPIU','OTSTRP','PBA','PKGHT','PKGHTP','PKGHTTYP','PKGPIU','PUBCLIM','RCAC','RDHTNF','REGION','RFCNS','SLFCON','SOHT1'
             ,'SOUSED','STHT1','STHW','WHRECOV','WLCNS','WOHT1','ELUSED','ELHT2','ELCOOL','ELWATR','ELCOOK','ELMANU','NGGENR','FKGENR',
             'TVVIDEO', 'YRCONC','PRGENR','WOGENR','COGENR','SOGENR','OTGENR','GENUSE','NGHT1','SERVERC','RFGCOMP','RFCOOL','RFTILT'
             )
num_var1<-c('ACWNWP','DHCKBTU','DHHTBTU','FURNP','PCTERMN','LAPTPN','NWKER','NELVTR','NESLTR')
fact_var1<-c('ACWNWL','DHCOOK','DHHT1','FURNAC','PCTRMC','LAPTPC','NWKERC','ELEVTR','ESCLTR')

colkeep<-c(enr_num_var0,enr_fact_var0,num_var1,fact_var1)
#write.csv(colkeep,'colkeep.csv')
#colkeep[order(colkeep)];length(colkeep);unique(colkeep);colkeep[duplicated(colkeep)]
enrgy_use1<-enrgy_use1[,colnames(enrgy_use1) %in% colkeep]


##Data Transformation

##Some perenatge values were used in conjunction with character. Wherever 'No'/'2' set to 0
i<-1
for( i in 1:length(num_var1))
{
  num_var11<-num_var1[i]
  fact_var11<-fact_var1[i]
  print(fact_var11)
  print(table(enrgy_use1[,num_var11]>0,enrgy_use1[,fact_var11],useNA = 'always'))
}###Based on tabular output 'ACWNWP','FURNP','NELVTR can be transforemed in case of discrepancy#

##Imputing variables based on above logic
enrgy_use1$ACWNWP[enrgy_use1$ACWNWL=='2']<-0
enrgy_use1$FURNP[enrgy_use1$FURNAC=='2']<-0
enrgy_use1$NELVTR[enrgy_use1$ELEVTR=='2']<-0
enrgy_use1$NESLTR[enrgy_use1$ESCLTR=='2']<-0
enrgy_use1[colnames(enrgy_use1) %in% c(enr_fact_var0,fact_var1)]<-lapply(enrgy_use1[colnames(enrgy_use1) %in% c(enr_fact_var0,fact_var1)],as.factor)

sapply(enrgy_use1,function(x) sum(is.na(x)))[sapply(enrgy_use1,function(x) sum(is.na(x))>0)]


##Outlier detetcion using boxplot
par(mfrow=c(1,2))
boxplot(enrgy_use1$ELCNS/enrgy_use1$SQFT~enrgy_use1$PBA,ylab='ELCNS/SQFT',xlab='PBA',main='Boxplot of ELCNS/SQFT vs PBA');abline(h=150,col='red',lwd=1)
boxplot(enrgy_use1$WTCNS/enrgy_use1$SQFT~enrgy_use1$PBA,ylab='WTCNS/SQFT',xlab='PBA',main='Boxplot of WTCNS/SQFT vs PBA');abline(h=145,col='red',lwd=1)


temp<-enrgy_use1##assigning energy_use1 to temp file

temp<-temp[!temp$ELCNS==0,]
temp<-temp[!temp$WTCNS==0,]

##Checking log transformations of numeric variables

dropcol<-colnames(temp)[sapply(temp,function(x)(sum(is.na(x)))>400)]
temp<-temp[,!colnames(temp) %in% dropcol]
num_var2<-colnames(temp)[sapply(temp, is.numeric)]


##plotting normal plot to judge how normal are the predictor variables
somePDFPath = "logtransform.pdf"
pdf(file=somePDFPath) 
par(mar=c(4,4,4,4))
par(mfrow=c(4,2))
for( i in num_var2)
{
 
qqnorm(log(temp[!is.na(temp[,i]),i]+0.001),main=paste0('qqplot of log(',i,')'))##normal data
qqnorm(temp[!is.na(temp[,i]),i],main=paste0('qqplot of ',i))##highly skerwed data
}
dev.off()
##drop 'DHOTBTU','FKHTBTU','NGHTBTU','XRAYN','DAYLTP'### these columns needs to be dropped these doesnt have a distribution; might add noise to our model
##log-'DHHTBTU','MFHTBTU',DHBTU','ELCNS','COPIERN','PRNTRN','PCTERMN','WTCNS','LAPTPN','NWKER','SQFT'## log transformation is more normal


dropcol<-c('DHOTBTU','FKHTBTU','NGHTBTU','XRAYN','DAYLTP')#columns that will be log-transformed 
logcol<-c('DHHTBTU','MFHTBTU','DHBTU','ELCNS','COPIERN','PRNTRN','PCTERMN','WTCNS','LAPTPN','NWKER','SQFT')
temp<-temp[,!colnames(temp) %in% dropcol]
logcol1<-temp[,colnames(temp) %in% logcol];logcol2<-data.frame(lapply(logcol1,function(x)(log(x+0.0001))));colnames(logcol2)<-paste0('l',colnames(logcol1))
temp<-cbind(temp,logcol2)
#temp<-temp[,!colnames(temp) %in% logcol]
plot(temp$ELCNS~temp$NFLOOR)

par(mfrow=c(2,2))
par(mar=c(2,3,2,3))


par(mfrow=c(2,2))
par(mar=c(2,3,2,3))
qqnorm(enrgy_use1$ELCNS,main='normal plot of response=ELCNS',cex.lab=0.8,cex.main=0.8,cex.axis=0.5)
qqnorm(enrgy_use1$WTCNS,main='normal plot of response=WTCNS',cex.lab=0.8,cex.main=0.8,cex.axis=0.5)
qqnorm(log(enrgy_use1$ELCNS+0.001),main='normal plot of response=log(ELCNS)',cex.lab=0.8,cex.main=0.8,cex.axis=0.5,col.main='red')
qqnorm(log(enrgy_use1$WTCNS+0.001),main='normal plot of response=log(WTCNS)',cex.lab=0.8,cex.main=0.8,cex.axis=0.5,col.main='red')

temp$NFLOOR[temp$NFLOOR==994]<-20
temp$NFLOOR[temp$NFLOOR==995]<-30#transforming NFLOOR
temp$NELVTR[temp$NELVTR==995]<-54#transforming NELVTR

numvar<-colnames(temp)[sapply(temp, is.numeric)]
non_logcol<-numvar[!numvar %in% c(logcol,paste0('l',colnames(logcol1)))]

##Visualizing correlation between numerical variables
##This wil help us decide in which cases log transformations are useful
library(corrplot)
par(mfrow=c(1,1))
M<-cor(temp[complete.cases(temp[,colnames(temp) %in% c(non_logcol,logcol)]),colnames(temp) %in% c(non_logcol,logcol)]);corrplot(M,method='number') ##nonlog matrix
lM<-cor(temp[complete.cases(temp[,colnames(temp) %in% c(non_logcol,paste0('l',colnames(logcol1)))]),colnames(temp) %in% c(non_logcol,paste0('l',colnames(logcol1)))]);corrplot(lM,method='number') ##log matrix
M1<-data.frame(M);M1<-M1[order(row.names(M1)),colnames(M1) %in% c('WTCNS','ELCNS')];corrplot(as.matrix(M1),method='number',tl.cex=0.8,number.cex = 0.8)
#M1<-data.frame(M);M1<-M1[order(row.names(M1)),];corrplot(as.matrix(M1),method='number',tl.cex=0.8,number.cex = 0.6)
lM1<-data.frame(lM)
##DOING LOG TRASNFORMATIONS IMPROVES CORRELATION AND MAKES DISTRIBUTION MORE NORMAL
##aLSO LAPTPN, PCTERMN,MFHTBTU done perform good with log trasnformation
row.names(lM1)<-c("NFLOOR", "BASEMNT","NELVTR", "NESLTR", "WKHRS","HEATP","FURNP","COOLP","ACWNWP", "HDD65","CDD65","SQFT","NWKER",
"PCTERMN","LAPTPN","PRNTRN","COPIERN","ELCNS", "MFHTBTU","WTCNS")
lM1<-lM1[order(row.names(lM1)),colnames(lM1) %in% c('lWTCNS','lELCNS')];
row.names(lM1)<-c("ACWNWP","BASEMNT" ,"CDD65"  , "COOLP",   "lCOPIERN", "lELCNS",   "FURNP"  , "HDD65" ,  "HEATP",   "lLAPTPN",  "lMFHTBTU" 
                  ,"NELVTR" , "NESLTR"  ,"NFLOOR" ,"lNWKER"  , "lPCTERMN" ,"lPRNTRN" , "lSQFT"  ,  "WKHRS"  , "lWTCNS" )
corrplot(as.matrix(lM1),method='number',tl.cex=0.8,number.cex = 0.6)

####EDA####

eda<-c("lSQFT","NELVTR","PBA","lNWKER", "WKHRS", "CDD65", "lPRNTRN","MFHTBTU","PUBCLIM","CENDIV","HDD65", 
"GLSSPC","lCOPIERN","SERVERC","YRCONC","ELHT2", "ESCLTR","WLCNS", "LTEXPC","PCTERMN", 
 "ELHT1", "MAINCL","LAPTPN","RFCOOL","FREESTN","REGION",'NESLTR') 

require(ggplot2)
require(gridExtra)
temp1<-temp[complete.cases(temp),colnames(temp) %in% c(eda,'lWTCNS','lELCNS')]
somePDFPath = "eda.pdf"
pdf(file=somePDFPath) 
for ( i in eda)
{
  if(is.numeric(temp[,i])) 
  {p1<-ggplot(temp1,aes_string(y='lWTCNS',x=i))+geom_point()+labs(title=paste('lWTCNS vs',i))
   p2<- ggplot(temp1,aes_string(y='lELCNS',x=i))+geom_point()+labs(title=paste('lELCNS vs',i))
  grid.arrange(p1,p2,ncol=2)  
  }
  else
  {
    p1<-ggplot(temp1,aes_string(y='lWTCNS',x=i))+geom_violin(draw_quantiles = 0.5)+labs(title=paste('lWTCNS vs',i))
    p2<- ggplot(temp1,aes_string(y='lELCNS',x=i))+geom_violin(draw_quantiles = 0.5)+labs(title=paste('lELCNS vs',i))
    grid.arrange(p1,p2,nrow=2)
    
  }
}
dev.off()  
rm(temp1)
  


##dropping extra variables and choosing variables of importance
colnames(temp)[order(colnames(temp))]
dropcol1<-c('ACWNWP',"DHHTBTU","DHBTU", "ELCNS","COPIERN", "PRNTRN", "WTCNS","NWKER","SQFT", "lMFHTBTU", "lPCTERMN","lLAPTPN",'FURNP'
,'PCTRMC','LAPTPC','NWKERC','NESLTR','ELEVTR','ELUSED','WLUSED')
temp<-temp[,!colnames(temp) %in% dropcol1]
## on onbservig all the variables, variables with generally >200 observations are not of much use. So we will drop these columns
coldrop<-sapply(enrgy_use1,function(x) sum(is.na(x)))[sapply(enrgy_use1,function(x) sum(is.na(x))>200)]
temp<-temp[,!colnames(temp) %in% names(coldrop)]
temp$FREESTN<-ifelse(is.na(temp$FREESTN),'No','Yes')
colnames(temp)[(order(colnames(temp)))]

temp$FREESTN<-as.factor(temp$FREESTN)
summary(temp)

###Assessing initial variable importance####


library(mvtboost)
temp1<-temp[complete.cases(temp),]
Y<-temp1[,c("lWTCNS",'lELCNS')]
Ys <- scale(Y)
X<-temp1[,!colnames(temp1) %in% c("lWTCNS",'lELCNS')]
num.ids <- unlist(lapply(X,is.numeric))
X[,num.ids]<-lapply(X[,num.ids],scale)

summary(X)

set.seed(101)
trainset<-sample(1:nrow(temp1),size=0.75*nrow(temp1))

out <- mvtb(Y=Ys,X=X,          # data
            n.trees=1000,          # number of trees
            shrinkage=.01,         # shrinkage or learning rate
            interaction.depth=3,
            s=trainset,
            cv.folds=5)   # tree or interaction depth

out$best.trees
summary(out)
covex <- mvtb.covex(out, Y=Ys, X=X)
sumcov<-apply(covex,2,sum)

covex<-covex[,order(sumcov,decreasing = T)]
colnames(covex)[1:30]##variables with some significance
## we will use these variables to further develop or model by cleaning data
# [1] "lSQFT"   "PBA"     "WKHRS"   "lNWKER"  "NELVTR"  "MFHTBTU" "PCTERMN" "CENDIV"  "CDD65"   "BLDSHP"  "HDD65"   "lPRNTRN" "YRCONC" 
# [14] "SERVERC" "PUBCLIM" "RFTILT"  "LNHRPC"  "WLCNS"   "COOLP"   "RFCNS"   "MAINCL"  "LOHRPC"  "GLSSPC"  "RFCOOL"  "CHILLR"  "LTEXPC" 
# [27] "ELCOOL"  "NFLOOR"  "LAPTPN"  "CHWT

temp<-temp[,colnames(temp) %in% c(colnames(covex)[1:30],'lWTCNS','lELCNS')]

####Imputing#### 

misscol<-sapply(temp,function(x) sum(is.na(x)))[sapply(temp,function(x) sum(is.na(x))>0)]
#temp<-temp[!is.na(temp$GLSSPC),]
##'GLSSPC', ,'BLDSHP','NELVTR' have 153 missing values. these are important predictors so will drop these cases and impute other numeric columns
sapply(temp,function(x) sum(is.na(x)))[sapply(temp,function(x) sum(is.na(x))>0)]

numvar<-colnames(temp)[sapply(temp, is.numeric)]
library(mice);md.pattern(temp[,numvar])
library(VIM);mice_plot <- aggr(temp[,numvar], col=c('navyblue','yellow'),
                                 numbers=TRUE, sortVars=TRUE,
                                 labels=names(temp[,numvar]), cex.axis=.7,
                                 gap=3, ylab=c("Missing data","Pattern"))



imputed_Data <- mice(temp[,!colnames(temp) %in% c('lWTCNS','lELCNS')], m=10, maxit = 10, method = 'rf', seed = 101)
require(lattice)
densityplot(imputed_Data)##visualizing imputed distributions with original distrirbutions

stripplot(imputed_Data)
temp<-temp[!is.na(temp$BLDSHP),]
##COPIERN, LAPTPN,PCTERMN,HEATP shows good imputation

NFLOOR<-NULL
WKHRS<-NULL
COOLP<-NULL
PCTERMN<-NULL
LAPTPN<-NULL
HDD65<-NULL
CDD65<-NULL
MFHTBTU<-NULL
lSQFT<-NULL
lNWKER<-NULL
lPRNTRN<-NULL



which(colnames(complete(imputed_Data,1))=='NFLOOR');which(colnames(complete(imputed_Data,1))=='WKHRS');which(colnames(complete(imputed_Data,1))=='COOLP');which(colnames(complete(imputed_Data,1))=='PCTERMN')
which(colnames(complete(imputed_Data,1))=='LAPTPN');which(colnames(complete(imputed_Data,1))=='HDD65');which(colnames(complete(imputed_Data,1))=='CDD65');which(colnames(complete(imputed_Data,1))=='MFHTBTU')
which(colnames(complete(imputed_Data,1))=='lSQFT');which(colnames(complete(imputed_Data,1))=='lNWKER');which(colnames(complete(imputed_Data,1))=='lPRNTRN')

for ( i in 1:10)
{  
NFLOOR<-cbind(NFLOOR,complete(imputed_Data,i)[,9])
WKHRS<-cbind(WKHRS,complete(imputed_Data,i)[,12])
COOLP<-cbind(COOLP,complete(imputed_Data,i)[,14])
PCTERMN<-cbind(PCTERMN,complete(imputed_Data,i)[,18])
LAPTPN<-cbind(LAPTPN,complete(imputed_Data,i)[,19])
HDD65<-cbind(HDD65,complete(imputed_Data,i)[,24])
CDD65<-cbind(CDD65,complete(imputed_Data,i)[,25])
MFHTBTU<-cbind(MFHTBTU,complete(imputed_Data,i)[,26])
lSQFT<-cbind(lSQFT,complete(imputed_Data,i)[,28])
lNWKER<-cbind(lNWKER,complete(imputed_Data,i)[,29])
lPRNTRN<-cbind(lPRNTRN,complete(imputed_Data,i)[,30])


}

NFLOOR<-apply(NFLOOR, 1,mean)
WKHRS<-apply(WKHRS, 1,mean)
COOLP<-apply(COOLP, 1,mean)
PCTERMN<-apply(PCTERMN, 1,mean)
LAPTPN<-apply(LAPTPN, 1,mean)
HDD65<-apply(HDD65, 1,mean)
CDD65<-apply(CDD65, 1,mean)
MFHTBTU<-apply(MFHTBTU, 1,mean)
lSQFT<-apply(lSQFT, 1,mean)
lNWKER<-apply(lNWKER, 1,mean)
lPRNTRN<-apply(lPRNTRN, 1,mean)
sum(COOLP==temp$COOLP,na.rm=T)

##imputing variable with average imputation
df<-data.frame(temp$lCOPIERN[!is.na(temp$lCOPIERN)])
df$line<-rep("a",nrow(df))
df1<-data.frame(lCOPIERN[!is.na(temp$lCOPIERN)],rep("b",nrow(df)));colnames(df1)<-colnames(df)
df<-rbind(df,df1);colnames(df)[1]<-'lCOPIERN'
library(ggplot2)
#install.packages("evaluate")
ggplot(df,aes(x=lCOPIERN,fill=line))+geom_density(alpha=0.5)

temp$NFLOOR<-NFLOOR
temp$WKHRS<-WKHRS
temp$COOLP<-COOLP
temp$PCTERMN<-PCTERMN
temp$LAPTPN<-LAPTPN
temp$HDD65<-HDD65
temp$CDD65<-CDD65
temp$MFHTBTU<-MFHTBTU
temp$lSQFT<-lSQFT
temp$lNWKER<-lNWKER
temp$lPRNTRN<-lPRNTRN



sapply(temp,function(x) sum(is.na(x)))[sapply(temp,function(x) sum(is.na(x))>0)]
#temp1<-temp
temp<-temp[!is.na(temp$BLDSHP),]
sapply(temp,function(x) sum(is.na(x)))[sapply(temp,function(x) sum(is.na(x))>0)]
temp<-temp[complete.cases(temp),]
summary(temp)








# One of the challenges of using multivariate decision tree ensembles is that the model is more difficult to interpret than a single tree. While tree boosting can be used to build a very accurate predictive model, it is potentially more important for researchers to interpret the effects of predictors. Below, we describe approaches that have been developed to
# 
# identify predictors with effects on individual outcome variables
# identify groups of predictors that jointly influence one or more outcome variables
# visualize the functional form of the effect of important predictors
# detect predictors with possible interaction non-linear effects.

####model building####

library(mvtboost)
Y<-temp[,c("lWTCNS",'lELCNS')]
Ys <- scale(Y)
X<-temp[,!colnames(temp) %in% c("lWTCNS",'lELCNS')]
# numvar<-colnames(temp[,!colnames(temp) %in% c("WTCNS",'ELCNS')])[sapply(temp[,!colnames(temp) %in% c("WTCNS",'ELCNS')], is.numeric)]
# char.ids <- unlist(lapply(X,is.character))
# X[,char.ids] <- lapply(X[,char.ids],as.factor)
# X[,numvar]<-lapply(X[,numvar],scale)


set.seed(101)
trainset<-sample(1:nrow(temp),size=0.75*nrow(temp))

out <- mvtb(Y=Ys,X=X,          # data
            n.trees=1000,          # number of trees
            shrinkage=.02,         # shrinkage or learning rate
            interaction.depth=3,
            s=trainset,
            cv.folds=5)   # tree or interaction depth

####Results####

##Relative Influence


ri<-data.frame(round(mvtb.ri(out,relative = "tot"),2))


ri$sum<-ri$lWTCNS+ri$lELCNS
row.names(ri[ri$sum==0,])

numformat <- function(val){sub("^(-?)0.", "\\1.", sprintf("%.1f", val))}
par(mar=c(10,5,5,5))
mvtb.heat(t(mvtb.ri(out,relative = 'col')[order(ri$sum,decreasing = T)[1:15],]),
          clust.method = NULL,cexRow=0.8,cexCol=0.8)


##Fit

testset <- (1:nrow(Ys))[!(1:nrow(Ys) %in% trainset)]
yhat <- predict(out,newdata=X[testset,])
diag(var(yhat)/var(Ys[testset,]))


##Covariance

par(mar=c(16,8,8,8),mfrow=c(1,1))

covex <- mvtb.covex(out, Y=Ys, X=X)
sumcov<-apply(covex,2,sum)

covex<-covex[,order(sumcov,decreasing = T)]

par(mar=c(10,10,5,5),mfrow=c(1,1))
numformat <- function(val){sub("^(-?)0.", "\\1.", sprintf("%.2f", val))}
mvtb.heat(covex[,1:15],cexRow=.9,numformat=numformat,clust.method = NULL)

##clustering
mvtb.cluster(covex)
par(mar=c(10,10,5,5),mfrow=c(1,1))
mvtb.heat(covex,cexRow=.9)


##Partial Dependence Plots and Interaction plots


somePDFPath = "partial_plots.pdf"
pdf(file=somePDFPath)

plot_names<-colnames(covex)[1:15]
par(mfrow=c(3,2),mar=c(4,4,4,4))
for( i  in plot_names)
{
plot(out,predictor.no=which(colnames(temp)==i),response.no=1,ylim=c(-1,1),main=paste('Partial plot of',i,'vs lWTCNS'),cex.main=1) # variable vs lWTCNS
plot(out,predictor.no=which(colnames(temp)==i),response.no=2,ylim=c(-1,1),main=paste('Partial plot of',i,'vs lELCNS'),cex.main=1) # variable vs WTCNS
}
dev.off()

somePDFPath = "interactions_plots.pdf"
pdf(file=somePDFPath)
plot_names<-colnames(covex)[1:4]
par(mfrow=c(2,2),mar=c(2,2,2,2))
#text(-4,1.825,labels="A",xpd=TRUE)
for (i in 1:(length(plot_names)-1))
{for (j in (i+1):length(plot_names))
mvtb.perspec(out,predictor.no=c(which(colnames(temp)==plot_names[i]),which(colnames(temp)==plot_names[j])),response.no=1,main=paste('Interaction plot of',plot_names[i],'-',plot_names[j],'vs lWTCNS'),cex.main=0.9)
mvtb.perspec(out,predictor.no=c(which(colnames(temp)==plot_names[i]),which(colnames(temp)==plot_names[j])),response.no=2,,main=paste('Interaction plot of',plot_names[i],'-',plot_names[j],'vs lWTCNS'),cex.main=0.9)
#print(paste(i,j))
  }
dev.off()


##Departures from additivity

res.nl <- mvtb.nonlin(out,Y=Ys,X=X)
lapply(res.nl,function(r){head(r[[1]])})












