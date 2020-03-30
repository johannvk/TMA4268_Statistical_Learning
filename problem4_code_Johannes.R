library(ggplot2)
library(ggfortify)
library(MASS)
library(dplyr)
library(ISLR)
library(reshape2)
library(RColorBrewer)
library(splines)
library(caret)
library(e1071)


# Support vector classifier and Support vector machines: (Plus one chosen classification method. Linear Discriminant perhaps? Or Logistic classification.)
id <- "1Fv6xwKLSZHldRAC1MrcK2mzdOYnbgv0E"  # google file ID
d.diabetes <- dget(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                           id))
d.train = d.diabetes$ctrain
d.test = d.diabetes$ctest

head(d.train)

# Look at a) i:
# By performing a linear regression of diabetes ~ bmi + glu, we can get an indication 
# that increasing bmi and glu correspond to greater number of cases with diabetes?
pairs(d.train[, c("diabetes", "bmi", "glu")])
diabetes_lm = lm(diabetes ~ bmi + glu, data=d.train)
summary(diabetes_lm)
# Yes, females with high glucose levels and bmi seem to have a higher risk of diabetes. 

# Look at a) ii:
cat("Range of npreg:", range(d.train$npreg), "\n")
# Yes, some women had up to 17 Prenancies.

# Look at a) iii:
pairs(d.train[, c("bmi", "skin")])
cat("Correlation of bmi and skin:", cor(d.train$bmi, d.train$skin), "\n")
# Yes, bmi and triceps skin thickness seem to be possitively correlated.

# Look at a) iv:


# d.train$diabetes <- as.factor(d.train$diabetes)
# d.test$diabetes <- as.factor(d.test$diabetes)
