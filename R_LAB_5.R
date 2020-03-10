#### ANOVA ####

### Generating Normal random vectors
v1=rnorm(200, mean=0, sd=1)
v2=rnorm(200, mean=2, sd=1)
v3=rnorm(200, mean=0, sd=1)

plot(density(v1),xlim=c(-4,6),main="Three Normal distributions")
lines(density(v2),col=2)
lines(density(v3),col=3)

v1n=data.frame(x1=v1, x2="v1")
v2n=data.frame(x1=v2, x2="v2")
v3n=data.frame(x1=v3, x2="v3")

??mergeRows
library(RcmdrMisc)
# We create a single data frame for the three distributions (600 obs).
data=mergeRows(v1n, v2n, common.only=FALSE)
data=mergeRows(as.data.frame(data), v3n, common.only=FALSE)

### ANOVA on simulated data ####
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1) # Pr(>F) = p-value
# p-value = 2e-16 then we can refuse the null hypothesis.
Boxplot(x1~x2,data=data,id=FALSE)


library(lmtest)

### Assumptions of ANOVA

# 1.- Independent observations.
# 2.- Normality.
# 3.- Variances are equal.

# Durbin Watson (Independency)
# Ho = autocorrelation of the disturbances is 0.
dwtest(AnovaModel.1, alternative ="two.sided")
# p-value = 0.7453 
# We can't refurse the null hypothesis

#Shapiro test (Normality)
shapiro.test(residuals(AnovaModel.1))
# p-value = 0.4894
# We can't refurse the null hypothesis

#Breusch Pagan test (Variance equality)
# ^^^ Tests heteroskedasticity 
bptest(AnovaModel.1) 
# p-value = 0.2581
# We can't refurse the null hypothesis


### ANOVA Example: Wine (FactoMineR)
library(FactoMineR)
data(wine)
head(wine)
summary(wine)

#### Assumptions ####
#### Normality of quantitative variables ###
shapiro.test(wine$Aroma.intensity)
shapiro.test(wine$Flower)
shapiro.test(wine$Odor.Intensity)


model1<-aov(Aroma.intensity~Soil,data=wine)
summary.aov(model1) # P(>F) = 0.262 # We can't reject the null hypothesis

model2<-aov(Flower~Soil,data=wine)
summary.aov(model2) # P(>F) = 0.0525 # We can't reject the null hypothesis

model3<-aov(Intensity~Soil,data=wine)
summary.aov(model3)

model4<-aov(Intensity~Label,data=wine)
summary.aov(model4)

?Boxplot
Boxplot(wine$Intensity~wine$Soil,id=FALSE,col=2:(nlevels(wine$Soil)+1))

#### Independency of observations ###
dwtest(model3,alternative="two.sided")

### Homogenity of Variances #####
### Levene's Test #####
library(car)
?leveneTest # Homogeneity of variance across groups.
leveneTest(Flower~Soil,data=wine)
leveneTest(Intensity~Soil,data=wine)

### Breusch Pagan test (from lmtest) ####
bptest(model3)

### Bartless test of homogeneity of variances.
bartlett.test(Intensity~Soil, data=wine)

#########################################
########## Multiple Comparisons #########
#########################################

# Cross-product comparison of median/variance.

# The most used are:
# * Turkey's HSD
# * Bonferroni t-test

### Tukey's HSD
?TukeyHSD
TukeyHSD(model3)

### Pairwise t-test 
# (this is not a good option because it is increasing the type one error, use Bonferroni to correct this)

?pairwise.t.test

with(wine,
{
  pairwise.t.test(Intensity,Soil,p.adj="none")
})

### Bonferroni Correction

with(wine,
{ 
pairwise.t.test(Intensity,Soil,p.adj="bonf")
})

### LSD Test (Least Significant Difference test)
library(agricolae)
out <- LSD.test(model3, "Soil", console=TRUE)
out

### Other Multiple Comparison Test.
duncan.test(model3, "Soil", console=TRUE)
scheffe.test(model3, "Soil", console=TRUE)

####### Two-Way ANOVA #####

model5<-aov(Intensity~Soil+Label,data=wine)
summary(model5)

head(wine)

for (i in 3:31){
  print(colnames(wine)[i])
  print(summary(aov(wine[,i]~Soil+Label,data=wine)))
}

model6<-aov(Odor.Intensity~Soil+Label,data=wine)
summary(model6)

dwtest(model6)
shapiro.test(wine$Odor.Intensity)

leveneTest(Odor.Intensity~Soil,data=wine)
leveneTest(Odor.Intensity~Label,data=wine)

