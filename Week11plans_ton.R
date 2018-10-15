#Week 11
library(ggplot2)
Data <- read.csv("cleanedData.csv")
dim(Data)
adultData<-Data[Data$AGEC>=18,]
# low.fat_high.carb<-Data$FATPER1<25 & Data$CHOPER1>55
# high.protein_low.carb<-Data$PROPER1>20 & Data$CHOPER1<45
# high.fat_low.carb<-Data$FATPER1>30 & Data$CHOPER1<45
high.carb<-adultData$CHOPER1>45
high.protein<-adultData$PROPER1>20
high.fat<-adultData$FATPER1>35
mean(high.carb);mean(high.protein);mean(high.fat)
adultData1<-cbind(adultData,high.carb,high.protein,high.fat)


# Test independence of high.carb, high.protein, high.fat (log-linear models!)

# Find the impact of carb, protein, fat on BMI (LDA, CART, Logistic Regression)
# also include age, sex, etc. as varaibles.
# Write the limitations if not considering interactions

# "women might be more likely to adopt a low-fat diet?"
# Logistic Regression! or LDA, CART
