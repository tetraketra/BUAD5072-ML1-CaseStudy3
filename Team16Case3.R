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

numeric <- airData[unlist(lapply(airData, is.numeric))][-1]
for (i in 1:ncol(numeric)) {
    plot(numeric[,i], airData$COUPON, xlab = names(numeric)[i], ylab = "COUPON")
    #HI, DISTANCE, PAX, and FARE look possible promising given some transformations. Hard to tell with the rest.
    }

par(default)




#### SIMPLE LINEAR MODEL #### --------------------------------------------------
lm.simple <- lm(COUPON ~ ., data = airData)
summary(lm.simple) #This is already significant enough, but I'd like to remove insignificant variables for parsimony/

lm.simple.2 <- lm(COUPON ~ HI + S_INCOME + SLOT + DISTANCE + PAX, data = airData)
summary(lm.simple.2) #Nearly as good and overwhelmingly more parsimonious. Adj R^2: 0.6662

#TODO: ASSUMPTIONS <---- <---- <--- <--- <---




#### INVESTIGATE Y TRANSFORMS #### ---------------------------------------------
hist(airData$COUPON)
range(airData$COUPON) #Long skew, but ultimately a tight range (1.00 - 1.94).
hist(log(airData$COUPON))
range(log(airData$COUPON)) #This is tighter, but not enough for the loss of interpretation to be worth it.




#### INVESTIGATE X TRANSFORMS #### ---------------------------------------------
par(mfrow = c(2, 2))

hist(airData$HI) #Only slightly skewed.
hist(airData$S_INCOME) #Slightly spiky to the right of the mean, but otherwise acceptable.
hist(airData$DISTANCE) #Significant right skew.
hist(airData$PAX) #Extremely long tail.

#We believe that the log() transform would compact DISTANCE's and PAX's tails.

hist(airData$HI)
hist(airData$S_INCOME)
hist(log(airData$DISTANCE)) #Now only a slight left skew.
hist(log(airData$PAX)) #Much less skew.

lm.transformed <- lm(COUPON ~ HI + S_INCOME + SLOT + log(DISTANCE) + log(PAX), data = airData)
summary(lm.transformed) #Slightly higher Adj R^2 of 0.6888. This might be worth the interpretability loss if we can simplify.

lm.transformed <- lm(COUPON ~ HI + SLOT + log(DISTANCE) + log(PAX), data = airData)
summary(lm.transformed) #Barely lost any Adj R^2 (now 0.6877) but dropped a variable. More parsimonious.




#### OUTLIERS #### -------------------------------------------------------------
#TODO




#### FINAL REGRESSION EQUATION AND STATS#### -----------------------------------
#TODO




#### MODEL SUMMARY #### --------------------------------------------------------
#TODO



