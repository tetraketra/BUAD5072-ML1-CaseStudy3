#### TEAM 16 ####
    #Ben Silverberg
    #Andrew Lagattuta
    #Jiacan Li
    #Jeffri Bahrens
    #Smita Duta




#### SETUP #### ----------------------------------------------------------------
rm(list = ls())
if (!require(lmtest)) {install.packages("lmtest"); library(lmtest)}
if (!require(glue)) {install.packages("glue"); library(glue)}
if (!require(mltools)) {install.packages("mltools"); library(mltools)}
airData <- read.csv("case3.csv", stringsAsFactors = T)[-1:-4]




#### INITIAL EXPLORATION #### --------------------------------------------------
default <- par(mfrow = c(1, 1))
par(mfrow = c(3, 3))

pairs(airData[unlist(lapply(airData, is.numeric))], lower.panel = NULL) #That's pretty hard to interpret.

numeric <- airData[unlist(lapply(airData, is.numeric))][-10]
for (i in 1:ncol(numeric)) {
    plot(numeric[,i], airData$FARE, xlab = names(numeric)[i], ylab = "FARE")
    #Coupon, the incomes, population, and distance look possibly decent.
    }

par(default)




#### SIMPLE LINEAR MODEL #### --------------------------------------------------
lm.simple <- lm(FARE ~ ., data = airData)
summary(lm.simple) #This is already significant enough, but I'd like to remove insignificant variables for parsimony/

lm.simple.2 <- lm(FARE ~ . - COUPON - NEW, data = airData)
summary(lm.simple.2) #Nearly as good and more parsimonious.

#TODO ASSUMPTIONS - ANDREW




#### INVESTIGATE Y TRANSFORMS #### ---------------------------------------------
hist(airData$FARE)
range(airData$FARE)
hist(log(airData$FARE))
range(log(airData$FARE)) #This is tighter,

#TODO: ANDREW




#### INVESTIGATE X TRANSFORMS #### ---------------------------------------------
par(mfrow = c(4, 2))
for (var in c("HI", "S_INCOME", "E_INCOME", "S_POP")) {
    hist(airData$FARE, main = var); hist(log(airData[[var]]), main = glue("log({var})"))
    }

par(mfrow = c(3, 2))
for (var in c("E_POP", "DISTANCE", "PAX")) {
    hist(airData$FARE, main = var); hist(log(airData[[var]]), main = glue("log({var})"))
    }

par(default)


#The log transform reduces skewness in all spotlighted variables, but the noticable ones were...
    #HI, which now has a central hump.
    #S & E_INCOME, which now have more-even tails.
    #S & E_POP, which is more symmetric.
    #DISTANCE, which looks significantly closer ot normal.

lm.logX <- lm(FARE ~ VACATION + SW + log(HI) +
              log(S_INCOME) + log(E_INCOME) +
              log(S_POP) + log(E_POP) + SLOT +
              GATE + log(DISTANCE) + PAX, data = airData)
summary(lm.logX)

#This lowered our R^2. We should apply the LOG function to only the ones that are most in-need AND most-benefitted.

lm.logX.2 <- lm(FARE ~ VACATION + SW + log(HI) +
              S_INCOME + E_INCOME +
              S_POP + E_POP + SLOT +
              GATE + log(DISTANCE) + PAX, data = airData)
summary(lm.logX.2)

#This also lowered our R^2 from lm.simple.2.
#Let's rerun these with a logged dependent variable.

lm.logXY <- lm(log(FARE) ~ VACATION + SW + log(HI) +
              S_INCOME + E_INCOME +
              S_POP + E_POP + SLOT +
              GATE + log(DISTANCE) + PAX, data = airData)
cor(airData$FARE, predict(lm.logXY, newdata = airData))

lm.logXY.2 <- lm(log(FARE) ~ VACATION + SW + log(HI) +
              log(S_INCOME) + log(E_INCOME) +
              log(S_POP) + log(E_POP) + SLOT +
              GATE + log(DISTANCE) + PAX, data = airData)
cor(airData$FARE, predict(lm.logXY.2, newdata = airData))

#Minimal log transforms of the most egregious independent variables, ...
#then a log of the dependent variable was the most effective model.

lm.bestVars <- lm.logXY.2




#### OUTLIERS #### -------------------------------------------------------------
#TODO: JEFFRI




#### FINAL REGRESSION EQUATION AND STATS#### -----------------------------------
#TODO: JIACAN




#### MODEL SUMMARY #### --------------------------------------------------------
#TODO: SMITA



