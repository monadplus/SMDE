library(HSAUR)
data("heptathlon", package = "HSAUR")

plot(heptathlon)

#Changing direction of some variables
# WHY ? 
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

## Correlation among Independent Variables
# which: gives the true indices
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])
cor(heptathlon[,-score])

### Multiple Linear Regression Model ###
reg_model1<-lm(score~., data=heptathlon)
summary(reg_model1)

## Eliminating hurdles from data set
reg_model2<-lm(score~highjump+shot+run200m+longjump+javelin+run800m, data=heptathlon)
summary(reg_model2)

### Testing Regression Assumptions ###
### 1. Normality of the Error Term ###
# Using QQ plot
qqnorm(residuals(reg_model2))
# Using Histogram
hist(residuals(reg_model2))
# It is skewed.
#Shapiro Wilks Test
shapiro.test(residuals(reg_model2))
# The error term does not follow a Normal distribution. (p<0.05)
# But this is ok if we take it into account.

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_model2))
##Breusch Pagan Test
library(lmtest)
bptest(reg_model2)
# H0 is accepted (p>0.05).
# The homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(reg_model2, alternative = "two.sided")
# There is not an autocorrelaiton in the data set (p>0.05).
# The errors/observations are independent.

### 4. Multicollinearity ###
cor(heptathlon)

#####################################
##### PCA with FactoMineR ###########
#####################################

library(FactoMineR)
pca_hep<-PCA(heptathlon[,-score]) # Remove the dependent variable score.

### Explained Variation ###
pca_hep$eig
plot(pca_hep$eig[,1], type="o", main="Scree Plot")

## Component Loadings ## 
pca_hep$var$coord
pca_hep$var$coord[,1:2]

# Scores of PC1 and PC2
pca_hep$ind$coord[,1:2]


### Principal Component Regression ###
heptathlon$PC1<-pca_hep$ind$coord[,1]
heptathlon$PC2<-pca_hep$ind$coord[,2]
reg_pc<-lm(score~PC1 + PC2, data=heptathlon)
summary(reg_pc)

##### Assumptions ###
#1. Normality
#Shapiro Wilks Test
shapiro.test(residuals(reg_pc))
# The error term does not follow a Normal distribution. (p<0.05)

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_pc))
##Breusch Pagan Test
bptest(reg_pc)
# H0 is accepted (p>0.05).
# The homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(reg_pc, alternative = "two.sided")
# There is not an autocorrelaiton in the data set (p>0.05).
# The errors/observations are independent.

###########################
### Prediction Accuracy ###
###########################

n <- nrow(heptathlon)
train.sample <- sample(1:n, round(0.67*n)) # length = 17
# Indexes of the samples for training

train.set <- heptathlon[train.sample, ] # Get the training samples from indexs
test.set <- heptathlon[-train.sample, ] 

train.model <- lm(score ~ PC1+PC2 , data = train.set)
summary(train.model)

?predict
yhat<-predict(train.model, test.set, interval="prediction")
yhat
y<-test.set$score

error<-cbind(yhat[,1,drop=FALSE],y,(yhat[,1]-y)^2)
sqr_err<-error[,3]
sse<-sum(sqr_err)

### Root Mean Square Error (RMSE) 
### RMSE = sqrt(SSE/N)

# In order to compare goodness of models
# you can use RMSE to compare them
RMSE<-sqrt(sse/(nrow(test.set)))
RMSE
