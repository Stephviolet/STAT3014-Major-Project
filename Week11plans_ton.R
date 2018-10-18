#Week 11
setwd("C:/Users/luodo/OneDrive/Courses/STAT3914/Project")
#library(ggplot2)
Data <- read.csv("cleanedData.csv")
dim(Data)
adultData<-Data[Data$AGEC>=18,]
# low.fat_high.carb<-Data$FATPER1<25 & Data$CHOPER1>55
# high.protein_low.carb<-Data$PROPER1>20 & Data$CHOPER1<45
# high.fat_low.carb<-Data$FATPER1>30 & Data$CHOPER1<45
carb.cat<-cut(adultData$CHOPER1,
              breaks=c(-1,45,65,100),
              labels=c('low','medium','high'))
fat.cat<-cut(adultData$FATPER1,
             breaks=c(-1,20,35,100),
             labels=c('low','medium','high'))
protein.cat<-cut(adultData$PROPER1,
                 breaks=c(-1,15,25,100),
                 labels=c('low','medium','high'))
mean(high.carb);mean(high.protein);mean(high.fat)
adultData1<-cbind(adultData,high.carb,high.protein,high.fat)


# Test independence of high.carb, high.protein, high.fat (log-linear models!)

# Find the impact of carb, protein, fat on BMI (LDA, CART, Logistic Regression)
# also include age, sex, etc. as varaibles.
# Write the limitations if not considering interactions

# "women might be more likely to adopt a low-fat diet?"
# Logistic Regression! or LDA, CART