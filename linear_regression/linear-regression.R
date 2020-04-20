### Linear Regression

# Data set
summary(cars)
head(cars)

# Linear model
cars.lm <- lm(dist ~ speed, data=cars) # Distance respect to speed, y = distance
cars.lm #  dist = -17.579 + 3.932*speed
summary(cars.lm)

# Is there a relation between speed and distance ?

# Draw a scatterplot
# with(cars, scatter.smooth(speed, dist))
scatterplot(dist~speed, regLine=lm, smooth=FALSE, data=cars)

### Test the Hypotheses

# Normality of the residuals
shapiro.test(residuals(cars.lm)) # 0.02152

# Constant variance assumption (Breusch-Pagan test)
library(lmtest)
bptest(cars.lm) # p-value - 0.07297

# Independence assumption (Durbin-Watson test)
dwtest(cars.lm, alternative = "two.sided") # p-value 0.1904
# H_0 = true autocorrelation is 0
# We can't refuse this hypothesis

### Predictions

new.speeds <- data.frame(speed = c(12, 19, 24))
predict(cars.lm, newdata = new.speeds) # We are forgetting about intervals, recall this is an statistical prediction!

# Confidence interval (by default, 95% of confiderence interval around the **mean**)
predict(cars.lm, newdata = new.speeds, interval = "confidence")
# fit: the predicted value
# lwr: lower confidence limit
# upr: upper confidence limit
#
# For example: for speed=12, disntace= 95% of the distances are between (24.39 , 34.82)

# Prediction Interval (uncertainty around a **single value**)
# Recall this has a worst approximation than the mean, wider intervals)
predict(cars.lm, newdata = new.speeds, interval = "prediction")
# speed = 12, distance = 29.60981 (-1.749529, 60.96915)