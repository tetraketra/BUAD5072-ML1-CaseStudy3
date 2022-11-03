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
if (!require(moments)) {install.packages("moments"); library(moments)}
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

par(mfrow = c(1, 1))




#### SIMPLE LINEAR MODEL #### --------------------------------------------------
lm.simple <- lm(FARE ~ ., data = airData)
summary(lm.simple) #This is already significant enough, but I'd like to remove insignificant variables for parsimony/

lm.simple.2 <- lm(FARE ~ . - COUPON - NEW, data = airData)
summary(lm.simple.2) #Nearly as good and more parsimonious.

hist(lm.simple.2$residuals)
    # Histogram of residuals looks normally distributed
mean(lm.simple.2$residuals)
    # Mean is very close to 0, also indicating that it is normally distributed

par(default)
plot(lm.simple.2)
# The first plot Residuals vs Fitted which tests for normality, looks good, no extreme outliers or patterns in the data.
# The Normal QQ Plot which tests for linearity looks good, the residuals fit on a line very well.
# Scale-Location looks good as well with a pretty straight horizontal line similar to the first plot and no pattern in the data. This plot tests for homoskedascticity and it appears to be homoskedasctic meaning the variances are constant. This conflicts with the BP Test done below.
# The Residuals vs Leverage plot seems clustered to the left, but the line is horizontal and pretty flat, along with no points beyond Cook's line so it looks good.

lmtest::bptest(lm.simple.2)
# The p value is very low, so we can reject the null hypothesis which is homoskedasticity, and conclude the data is heteroskedastic.

car::vif(lm.simple.2)
# This tests for multicollinearity, but all of the variables have very low values, all under 3, so we can say there is no multicollinearity in the linear model.




#### INVESTIGATE Y TRANSFORMS #### ---------------------------------------------

#Lets investigate visually first.
par(mfrow = c(2,1))
hist(airData$FARE)
range(airData$FARE)
hist(log(airData$FARE))
range(log(airData$FARE)) #This is tighter,

skewness(airData$FARE)
# 0.6208544

skewness(log(airData$FARE))
# -0.2640935

#skewness is a measure of assymetry and although the histogram of FARE does not appear as normal as the histogram of log(FARE), the skewness is not very high, only 0.62. Anything under 1 is not considered to be highly skewed. 0.62 is considered moderately skewed, but using the log(FARE) a lot of intepretability is lost.

kurtosis(airData$FARE)
# 2.662211

kurtosis(log(airData$FARE))
# 2.306927

# A standard normal distribution has a kurtosis of 3. Both FARE and log(FARE) have similiar kurtosis and both are close to 3, though FARE is slightly better.

shapiro.test(airData$FARE)
# W = 0.95358, p-value = 2.732e-13
shapiro.test(log(airData$FARE))
# W = 0.97821, p-value = 3.811e-08

# The Shapiro-Wilk normality test measures normality on a scale of 0 to 1 with 1 indicating normality and although log(FARE) is slightly higher, both still fail to reach normality because the p-value for both is very very low which means we reject the null hypothesis, which is normality. So neither are normal and log(FARE) is not much better, so we keep FARE and do not transform to log(FARE).

# Overall FARE is not perfectly normal and log(FARE) appears slightly better visually in a histogram however, interpretability is lost when using log(FARE) and skew is not too bad for FARE and kurtosis is actually better for FARE. Neither distributions pass the Shapiro-Wilk normality test so using lof(FARE) does not seem to make sense, we will stick with FARE and re-examine after looking at the x variables.




#### INVESTIGATE X TRANSFORMS #### ---------------------------------------------
par(mfrow = c(4, 2))
for (var in c("HI", "S_INCOME", "E_INCOME", "S_POP")) {
    hist(airData[[var]], main = "original", xlab = var); hist(log(airData[[var]]), main = "logged", xlab = glue("log({var})"))
    }

par(mfrow = c(3, 2))
for (var in c("E_POP", "DISTANCE", "PAX")) {
    hist(airData[[var]], main = "original", xlab = var); hist(log(airData[[var]]), main = "logged", xlab = glue("log({var})"))
    }

par(default)

#The log transform reduces skewness in all spotlighted variables, but the noticeable ones were...
    #HI, which now has a central hump.
    #S & E_POP, which are more central and symmetric.
    #DISTANCE & PAX, which looks significantly closer to normal.

lm.mixed_semilog <- lm(FARE ~ VACATION + SW + log(HI) +
              S_INCOME + E_INCOME +
              log(S_POP) + log(E_POP) + SLOT +
              GATE + log(DISTANCE) + log(PAX), data = airData)
summary(lm.mixed_semilog)

#This lowered our R^2 from lm.simple.2.
#Let's rerun this with the logged dependent variable from the previous section.

lm.mixed_loglog <- lm(log(FARE) ~ VACATION + SW + log(HI) +
              S_INCOME + E_INCOME +
              log(S_POP) + log(E_POP) + SLOT +
              GATE + log(DISTANCE) + log(PAX), data = airData)
cor(airData$FARE, exp(lm.mixed_loglog$fitted.values)) #Better

#Now compare to the original non-log model.
cor(airData$FARE, exp(lm.mixed_loglog$fitted.values)) #new model
cor(airData$FARE, lm.simple.2$fitted.values) #old model
#This is a mild improvement.

lm.bestVars <- lm.mixed_loglog




#### OUTLIERS #### -------------------------------------------------------------
#TODO: JEFFRI




#### FINAL REGRESSION EQUATION AND STATS#### -----------------------------------
#TODO: JIACAN




#### MODEL SUMMARY #### --------------------------------------------------------
#TODO: SMITA



