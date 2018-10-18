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
d.additive<-glm(y~carb+fat+protein,
                family='poisson',data=loglin.dat)$dev
d.additive
glm.cf<-glm(y~carb+fat+protein+carb:fat,
                family='poisson',data=loglin.dat)
glm.cp<-glm(y~carb+fat+protein+carb:protein,
          family='poisson',data=loglin.dat)
glm.fp<-glm(y~carb+fat+protein+fat:protein,
          family='poisson',data=loglin.dat)
c(glm.cf$dev,glm.cp$dev,glm.fp$dev)
anova(glm.fp,test='Chisq')
glm(y~fat+carb+protein,family='poisson',data=loglin.dat)
