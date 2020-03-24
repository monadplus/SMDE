#### Simple Linear Regression ####

library(FactoMineR)
data(decathlon)
colnames(decathlon)
head(decathlon)

# We add x infront of the variables 100m and 110m.hurdle. Otherwise
# in the lm function they are not read since they begin with a number.
colnames(decathlon)[c(1,5,6,10)]<-c("x100m","x400m","x110m.hurdle","x1500m")

#There is a moderate strong linear relationship between 100m and 110m.hurdle.
cor.test(decathlon[,6],decathlon[,1]) # Correlation

## Scatter Plot of the variables including the regression line
??scatterplot
library(car)
scatterplot(x100m~x110m.hurdle, regLine=lm, smooth=FALSE, data=decathlon)

# Simple Linear Regression Model
RegModel.1 <-lm(x100m~ x110m.hurdle, data=decathlon)
summary(RegModel.1)

# MSE(Mean squared error)(residual standard erorr)=0.217, ths value is smaller than average of the response. 
# So the model is relatively good.

#Multiple R^2: 0.3363. Aproximately %34 of the variation of the response
#variable can be explained by the model (by using x110m.hurdle as the independent variable in the regression model.

# The F test shows that the model is significant (p<0.05).

# The outputs of the model
names(RegModel.1)

#The estimated regression equation is: 
coef_vec<-RegModel.1$coefficients
coef_vec #yhat=6.276 + 0.323x 

#The intercept of the model
b0<-RegModel.1$coefficients[1]
b0
# 6.27 is the predicted value of the response when x=0. It is significant (p<0.05).
# But t is meaningless interpret it since the data set does not include 0.

#The slope of the model
b1<-RegModel.1$coefficients[2]
b1
# 0.323 is the slope. One unit increase in x110m.hurdle
# results 0.323 unit increase in x100m.
# The p value of the t statistic is less than 0.05.
# The effect of 110m.hurdle on 100m is significant at %95 confidence level.

# Confidence intervals for the parameters of the model
confint(RegModel.1)

# The null hypothesis is H0:B1=0.
# If the confidence interval includes 0 => we accept the null hypothesis.
#
# (0.1761801, 0.4703976) the confidence intervals of the parameters does not include 0.
#      => The null hypothesis B0=0 and B1=0 are rejected.
#
# Therefore the coefficients are significant.


#Predicted Values of the Response
yhat<-RegModel.1$fitted.values

# Predicted Values of the Response with Their confidence Levels
predict.lm(RegModel.1,interval="confidence")

# Prediction interval for the Response (for x=16)
?predict.lm

new=data.frame(x110m.hurdle=16)
predict.lm(RegModel.1, newdata=new, interval="prediction")


### Test of Regression Assumptions ###

###### 1. Normality of the Error Term

# Using QQ plot
qqnorm(residuals(RegModel.1)) # Since the values are taking part close to the diagonal, the distribution is approximately normal.
# Using Histogram
hist(residuals(RegModel.1)) # It is approximately normal.
#Shapiro Wilks Test
shapiro.test(residuals(RegModel.1)) # The error term follows a Normal distribution. (p>0.05)

###### 2. Homogenity of Variance

# Residual Analysis #
plot(residuals(RegModel.1)) # Residuals have a rectangular pattern around the zero mean.
                            # There is no violation of this assumption.

##Breusch Pagan Test
library(lmtest)
bptest(RegModel.1) # H0 is accepted (p>0.05).
                   # The homogenity of variances is provided.

### 3. The independence of errors ### 
library(lmtest)
dwtest(RegModel.1, alternative = "two.sided") # There is not an autocorrelaiton in the data set (p>0.05).
                                              # The errors/observations are independent.


####### Example: anscombe data set

# Check the data set
anscombe #In anscombe data there are four independent and four dependent variables.

#Let us construct four diffrent simple linear regression models.
reg1<-lm(y1~x1,data=anscombe)
summary(reg1)

reg2<-lm(y2~x2, data=anscombe)
summary(reg2)

reg3<-lm(y3~x3,data=anscombe)
summary(reg3)

reg4<-lm(y4~x4,data=anscombe)
summary(reg4)

# Both of the models seem significant with almost the same values for residual standard error, R^2, adjusted R^2 and F test. 

#### Which model is better? 

# Let us check the coefficient of correlation between variables and scatter plots.

## Correlation Tests

# In all cases the coefficient of correlation is 0.816 and significant (p=0.002).
cor.test(anscombe$x1,anscombe$y1)
cor.test(anscombe$x2,anscombe$y2)
cor.test(anscombe$x3,anscombe$y3)
cor.test(anscombe$x4,anscombe$y4)

## Scatter Plots
op<-par(mfrow=c(2,2))
plot(y1~x1,data=anscombe)
plot(y2~x2,data=anscombe)
plot(y3~x3,data=anscombe)
plot(y4~x4,data=anscombe)
par(op)

# 1st scatter plot: The relationship is linear.

# 2nd scatter plot: The relationship is not linear. The shape is curvelinear.
# The plot of residuals vs. fitted values suggests a parabolic relationship /a quadratic model
names(reg2)
plot(reg2$fitted.values,reg2$residuals)

# Adding a quadratic term can increase R^2 value.
x2_2<-(anscombe$x2)^2

#If we add x^2 to the regression model the R^2 increases to 1.
# So all the variation of y could be explained by using x2 and x2_2 together in the model.
reg2_2<-lm(y2~x2+x2_2,data=anscombe)
summary(reg2_2)

# 3rd scatter plot: The relationship is linear but there is 
# one outlier. Either this observation should be ommitted or
# a robust regression analysis should be done so as to the 
# regression line is not effected from the outlier.

# When we omit that observation.(3rd observation)) 
anscombe2<-anscombe[-3,]
reg3_2<-lm(y3~x3,data=anscombe2)
summary(reg3_2)
# R squared value increase to 1. Perfect fit!

# 4th scatter plot: Most of the observations cumulated around x=8. 
# There is just one observation apart from other which causes a linear relationship in the data set. 
# This is an influential point.
# When it is removed, there is no reason to use regression.