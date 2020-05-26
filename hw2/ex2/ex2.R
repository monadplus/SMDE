################################################ 
# Homework 2: Exercise 2
################################################ 

path <- "/home/arnau/MIRI/SMDE/hw2"
dataset <- read.csv(paste(path, "ex1/dataset.csv", sep="/"),header=TRUE,sep=",")
plot(dataset)

## Correlation among Independent Variables
answer <- which(colnames(dataset) == "answer")
plot(dataset[,-answer])
cor(dataset[,-answer])

################################################ 
# Multiple Linear Regression Model
################################################ 

reg_model1<-lm(answer~., data=dataset) # I guess this outputs NA because f6-f10 are linear combinatons of f1-f5.
summary(reg_model1)

reg_model2<-lm(answer~factor6+factor7+factor8+factor9+factor10, data=dataset)
summary(reg_model2)

#### Only factor 3 has no linear relation with the variable answer, as we expected
reg_model3<-lm(answer~factor1+factor2+factor3+factor4+factor5, data=dataset)
summary(reg_model3)

#### Propose an expression to understand the relations between the data
# Answer = +0.005562 + 1.032*factor1 + 0.988*factor2 + 0.988*factor4 + 4.94*factor5

### Testing Regression Assumptions ###
### 1. Normality of the Error Term ###
# Using QQ plot
qqnorm(residuals(reg_model3))
# Using Histogram
hist(residuals(reg_model3))
#Shapiro Wilks Test
shapiro.test(residuals(reg_model3))
# H_0 is accepted: the error term does follows a Normal distribution (p > 0.05)

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_model3))
##Breusch Pagan Test
library(lmtest)
bptest(reg_model3)
# H_0 is accepted (p>0.05): the homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(reg_model3, alternative = "two.sided")
# There is not an autocorrelated in the data set (p>0.05).
# The errors/observations are independent.

### 4. Multicollinearity ###
cor(dataset)

################################################ 
## PCA
################################################ 

library(FactoMineR)
pca_ds<-PCA(dataset[,-answer]) # Remove the dependent variable score.

### Explained Variation
pca_ds$eig # cumulative percentage of variance > 75%
plot(pca_ds$eig[,1], type="o", main="Scree Plot")

## Component Loadings ## 
pca_ds$var$coord
pca_ds$var$coord[,1:3]

# Scores of PC1 and PC2
pca_ds$ind$coord[,1:3]


### Principal Component Regression ###
dataset$PC1<-pca_ds$ind$coord[,1]
dataset$PC2<-pca_ds$ind$coord[,2]
dataset$PC3<-pca_ds$ind$coord[,3]
reg_pc<-lm(answer~PC1 + PC2 + PC3, data=dataset)
summary(reg_pc)

##### Assumptions ###
#1. Normality
#Shapiro Wilks Test
shapiro.test(residuals(reg_pc))
# The error term does follow a Normal distribution. (p>0.05)

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