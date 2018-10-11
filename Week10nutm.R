#Week 10
library(ggplot2)
Data <- read.csv("cleanedData.csv")
dim(Data)
DataTon<-Data[Data$AGEC>=18 & Data$FATPER1>35 & Data$CHOPER1<45,]
nrow(DataTon)

# Scatterplot BMI v BMR
sex<-factor(ifelse(DataTon$SEX==1,'male','female'))
ifDiet<-ifelse(DataTon$BDYMSQ04>=1 & DataTon$BDYMSQ04<=3,'on a diet','other')
ifDiet<-ifelse(DataTon$BDYMSQ04==5,'not on a diet',ifDiet)
ifDiet<-as.factor(ifDiet)
BMIvBMR<-na.omit(data.frame(BMI=DataTon$BMISC,BMR=DataTon$BMR,Sex=sex,
                 Diet=ifDiet))
ggplot(BMIvBMR,aes(x=BMI,y=BMR))+
  geom_point(aes(colour=Sex))+geom_smooth(aes(group=Sex),method = "lm") #group by gender

# BMI v Diet
BMIvDiet<-na.omit(data.frame(BMI=DataTon$BMISC,Sex=sex,Diet=ifDiet))
ggplot(BMIvDiet,aes(x=BMI,group=factor(Diet),fill=factor(Diet)))+
  geom_histogram(aes(y=..density..),position='dodge',bins=10)
ggplot(BMIvBMR,aes(x=BMR,group=factor(Diet),fill=factor(Diet)))+
  geom_histogram(aes(y=..density..),position='dodge',bins=10)
t.test(BMI~Diet,data=BMIvBMR)
t.test(BMR~Diet,data=BMIvBMR)
anova(lm(BMR~BMI+Diet,data=BMIvBMR))
var.test(BMI~Diet,data=BMIvBMR)
var(BMIvBMR$BMI[BMIvBMR$Diet=='on a diet'])
var(BMIvBMR$BMI[BMIvBMR$Diet=='not on a diet'])
var.test(BMR~Diet,data=BMIvBMR)

# Energy Intake
EIBMR.Data<-na.omit(data.frame(EIBMR=DataTon$EIBMR1,BMI=DataTon$BMISC,BMR=DataTon$BMR,Sex=sex,
                          Diet=ifDiet))
ggplot(EIBMR.Data,aes(x=EIBMR,y=BMR))+
  geom_point(aes(colour=Sex))+geom_smooth(aes(group=Sex),method = "lm")
ggplot(EIBMR.Data,aes(x=EIBMR,group=factor(Diet),fill=factor(Diet)))+
  geom_histogram(aes(y=..density..),position='dodge',bins=10)
summary(lm(EIBMR~BMR,data=EIBMR.Data))
lm(EIBMR~Diet,data=EIBMR.Data)
t.test(EIBMR~Diet,data=EIBMR.Data)
var.test(EIBMR~Diet,data=EIBMR.Data)
var(EIBMR.Data$EIBMR[EIBMR.Data$Diet=='on a diet'])
var(EIBMR.Data$EIBMR[EIBMR.Data$Diet=='not on a diet'])
t.test(EIBMR~Sex,data=EIBMR.Data)

# Stepwise Seleciton
Data.candidate<-na.omit(data.frame(BMR=DataTon$BMR,EIBMR=DataTon$EIBMR1,BMI=DataTon$BMISC,
                           Sex=sex,Diet=ifDiet, Hypterintensive = DataTon$HYPBC,
                           ADTOTSE=DataTon$ADTOTSE,Age=DataTon$AGEC))
model.full<-regsubsets(BMI ~ . , data=Data.candidate, 
           method="exhaustive")
X<-Data.candidate[,names(Data.candidate)!='BMI']
y<-Data.candidate$BMI
Xy<-cbind(X,y)
model.bestglm<-bestglm(Xy,IC='BIC')#exhaustive search, use method=forward for forward selection
models.table<-model.bestglm$Subsets
lm(y~.,data=X[as.logical(models.table[2,2:(ncol(models.table)-2)])])

# k-fold cross-validation
K<-10
n<-nrow(Xy)
m<-nrow(models.table)-1 #number of models comparing
g<-20 #run CV multiple times and average the result since the CV error depend on the split of dataset
err.out<-matrix(0,K*g,m) #miss-classification rate

##
set.seed(0)
fold<-sample(rep(1:K, each=n/K))

##
for(ginger in 1:g){fold<-sample(rep(1:K, each=n/K))
for(i in 1:m){
  for(k in 1:K){
    data.train<-Xy[fold!=k,]
    data.test<-Xy[fold==k,]
    mod.train<-lm(y~.,data=X[as.logical(models.table[i+1,2:(ncol(models.table)-2)])])
    pred.test<-predict(mod.train, newdata=data.test)
    # err.out[k,i]<-sum(abs(data.test$Survived-pred.test))/length(pred.test)
    err.out[(ginger-1)*K+k,i]<-mean((data.test$y-pred.test)^2)
  }
}
}
cv<-apply(err.out,2,mean)
cv.sd<-apply(err.out,2,sd)
cv.estimation<-cbind(cv,cv-cv.sd,cv+cv.sd)
colnames(cv.estimation)<-c('CV','cv-SE(CV)','CV+SE(CV)')
cv.estimation
plot(cv.estimation[,1],type='b',ylim=c(8,28))
lines(cv.estimation[,2],lty=2,col='blue')
lines(cv.estimation[,3],lty=2,col='blue')
