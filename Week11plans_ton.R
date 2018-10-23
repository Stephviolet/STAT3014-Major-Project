#Week 11
setwd("~/GitHub/STAT3014-Major-Project")
#library(ggplot2)
Data <- read.csv("cleanedData.csv")
dim(Data)
adultData<-Data[Data$AGEC>=18,]
carb.cat<-cut(adultData$CHOPER1,
              breaks=c(-1,45,65,100),
              labels=c('low','medium','high'))
fat.cat<-cut(adultData$FATPER1,
             breaks=c(-1,20,35,100),
             labels=c('low','medium','high'))
protein.cat<-cut(adultData$PROPER1,
                 breaks=c(-1,15,25,100),
                 labels=c('low','medium','high'))


# Test independence of high.carb, high.protein, high.fat (log-linear models!)

# Find the impact of carb, protein, fat on BMI (LDA, CART, Logistic Regression)
# also include age, sex, etc. as varaibles.
# Write the limitations if not considering interactions

# "women might be more likely to adopt a low-fat diet?"
# Logistic Regression! or LDA, CART

# log-linear model
tables.3way<-table(carb.cat,fat.cat,protein.cat)
table.y<-NULL
for(i in 1:3){
  for(j in 1:3){
    table.y<-c(table.y,tables.3way[j,,i])
  }
}
table.fat<-factor(rep(c('low','medium','high'),9))
table.carb<-factor(rep(c('low','medium','high'),each=3,times=3))
table.protein<-factor(rep(c('low','medium','high'),each=9))
loglin.dat<-data.frame(y=table.y,
                          fat=table.fat,
                          carb=table.carb,
                          protein=table.protein)
loglin.dat
glm.additive<-glm(y~carb+fat+protein,
                family='poisson',data=loglin.dat)
glm.additive$dev
# additive model is for complete independence
glm.cf<-glm(y~carb+fat+protein+carb:fat,
                family='poisson',data=loglin.dat)
glm.cp<-glm(y~carb+fat+protein+carb:protein,
          family='poisson',data=loglin.dat)
glm.fp<-glm(y~carb+fat+protein+fat:protein,
          family='poisson',data=loglin.dat)
c(glm.cf$dev,glm.cp$dev,glm.fp$dev)
c(glm.cf$df.residual,glm.cp$df.residual,glm.fp$df.residual)
# glm.cf has least deviance
anova(glm.cf,test='Chisq')
1-pchisq(glm.additive$dev-glm.cf$dev,
glm.additive$df.residual-glm.cf$df.residual)
1-pchisq(glm.additive$dev-glm.cp$dev,
glm.additive$df.residual-glm.cp$df.residual)
1-pchisq(glm.additive$dev-glm.fp$dev,
glm.additive$df.residual-glm.fp$df.residual)

# reject the additive model(complete independence)
glm.cf.cp<-glm(y~carb+fat+protein+carb:fat+carb:protein,
                family='poisson',data=loglin.dat)
glm.cf.fp<-glm(y~carb+fat+protein+carb:fat+fat:protein,
                family='poisson',data=loglin.dat)
c(glm.cf.cp$dev,glm.cf.fp$dev)
c(glm.cf.cp$df.residual,glm.cf.fp$df.residual)
#
1-pchisq(glm.cf$dev-glm.cf.cp$dev,
glm.cf$df.residual-glm.cf.cp$df.residual)
anova(glm.cf.cp,test="Chisq")
# reject null and prefer glm.cf.cp

# test the uniform association case
glm.u.a<-glm(y~carb+fat+protein+carb:fat+carb:protein+fat:protein,
                family='poisson', data=loglin.dat)
glm.u.a$dev
1-pchisq(glm.cf.cp$dev-glm.u.a$dev,
glm.cf.cp$df.residual-glm.u.a$df.residual)

# test the saturated model
glm.sat<-glm(y~carb*fat*protein,
                family='poisson', data=loglin.dat)
1-pchisq(glm.u.a$dev-glm.sat$dev,
glm.u.a$df.residual-glm.sat$df.residual)
anova(glm.sat,test="Chisq")

# accept saturated
# mu_ijk hat = y_ijk
diet.prop<-cbind(loglin.dat[,1]/nrow(adultData),
  loglin.dat[,2:4])
colnames(diet.prop)[1]<-'proportion'

# sort the df by proportion
diet.prop<-diet.prop[order(
  diet.prop$proportion,decreasing=TRUE),]

# see most common diet types
head(diet.prop)

# 70% of the people are in 5 most popular diet types
sum(diet.prop$proportion[1:5])

# compare with the marginal proportion
summary(carb.cat)
summary(protein.cat)
summary(fat.cat)
pie(summary(fat.cat)) # pie chart