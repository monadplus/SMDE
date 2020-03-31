###############################################################
######                                                   ######
###### R SCRIPT: Principal Components Analysis (PCA)     ######
######           Factor Analysis                         ######             
######                                                   ######
######           Nihan Acar-Denizli, PhD                 ######
######                                                   ######
###############################################################

### Data set "Food-Price"###
### The food price data set consists of prices of different aliments in various States.
## The excel file "food.xls" contains row names in the first column and column names on the first row.
## You can import excel file by using RCommander or any function you prefer.
library(Rcmdr)

## Rcmdr command to import excel file
## food <- readXL("C:/Users/Nihan/Desktop/SMDE_UPC/scripts/food.xls", 
##               rownames=TRUE, header=TRUE, na="", sheet="Sayfa1", stringsAsFactors=FALSE)

View(food)
str(food)
head(food)
colnames(food)

########## TEST OF FACTORABILITY (BARTLETT TEST AND KMO INDEX)######################

###### Bartlett's Test of Spherecity####

install.packages("psych")
library(psych)
?cortest.bartlett

## Bartlett test requires the computation of the correlation matrix R and the number of observations.
## The correlation matrix of food prices ##
R<-cor(food[,1:5])
print(R)

### The number of observations (n) ###
n<-nrow(food)
n

## Bartlett tests if the correlation matrix is an identity matrix. 
#### (H0:R=I) ##
### If we reject the null hypothesis the variables are correlated.
cortest.bartlett(R,n)
## The p-value of Bartlett test is 0.001 < 0.05. 
## Then the null hypothesis is rejected. The variables are correlated.

###### Kaiser-Meyer-Olkin (KMO) Test ###

## We can use kmo function written by Prof. Shigenobu Aok.
### (http://minato.sip21c.org/swtips/factor-in-R.pdf)
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # Omit missing values
  r <- cor(x)                             # Correlation matrix
  r2 <- r^2                               # Squared correlation coefficients
  i <- solve(r)                           # Inverse matrix of correlation matrix
  d <- diag(i)                            # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0               # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

#KMO index
kmo(food[,1:5])

#$KMO
#[1] 0.6623388
# KMO index shows that the data is factorable. 
# It is recommended to have an index greater than 0.50.


######## PRINCIPAL COMPONENTS ANALYSIS ##############
?princomp
pca<-princomp(food[,1:5],cor=TRUE,scores=TRUE) #,cutoff=0.01)

# Proportion of Explained Variances by components#
summary(pca)
# The proportion of explained variance for the first two components
# 0.48 + 0.22 =0.70. It is greater than 0.66. 
# According to thumb rule we can say that use of the first two components is enough.

# The elements of summary vector
names(summary(pca))

# The loadings of variables on the components (by default loadings less than 0.1 are not shown)
summary(pca)$loadings

# The Computation of Eigenvalues (Lambdas) #
s2<-(summary(pca)$sdev)^2
s2 # eigenvalues

# The sum of eigenvalues is equal to the number of the variables when they are standardized.
# When we use correlation matrix instead of covariance matrix, we use standardized variables.
sum(s2) 

### Scree Plot ###
plot(pca)
plot(pca,type="line")
## The steepest decrese is from 1st to 2nd component.
## The elbow point is the 2nd component. 
# So the use of the first two components is enough.

pca$loadings
# The loadings of variables on the first component are similar except Oranges.
# The loading of Oranges on the second component is the highest (-0.797). It is negatively loaded.
# The loading less than 0.1 are not shown as default.

#PC1 vs. PC2#
## Biplot shows the direction of the variables and the position (scores) of the individuals on the first and on the second compoenent. 
# Arrows for each variable indicates the direction of increasing values of that variable.
biplot(pca)

# The scores are the principal component scores for each individual.
pca$scores

############ PCA with the Covariance Matrix by using"princomp" and "prcomp" functions #############
# Since the units of measurements of all variables are the same, we might use Covariance Matrix rather than Correlation Matrix.
# To use the correlation matrix the parameter "cor=TRUE" should be added.
pca_cov<-princomp(food[,1:5],scores=TRUE) 
summary(pca_cov)

# The proportions of explained variances increase when we use Covariance matrix instead of Correlation Matrix. 
names(pca_cov)
pca_cov$loadings 

# When we use the covariance matrix Oranges is loaded on the first component. 
# Other variables are loaded on the second component.
# We might say that the first component is an indictor of Orange prices and 
# the second component is the indicator of the prices of the rest of the aliments.
# If the variation in Orange prices is important for the analysis, we might use covariance matrix 
# instead of correlation matrix. Otherwise, we would prefer to use Correlation matrix.

# PC1 versus PC2
biplot(pca_cov)
# All the variables except oranges have the same direction of growth
# and are loaded on the second component.

# The scores of individuals (of States in this example). 
pca_cov$scores

#The minimum score on the 1st component
which.min(pca_cov$scores[,1])
#The minimum score on the 1st component
which.max(pca_cov$scores[,1])
# Baltimore is the cheapest, Honolulu is the most expensive State according to the Orange Prices. 

# The minimum score on the 2nd component
which.min(pca$scores[,2])
# Milwaukee is the chepest State according to the prices of other aliments.

### Cost of Living Index ###
## By summing the scores of the first and the second component we might generate 
# a general cost of living index since all the variables are positively loaded on the components.
cost_ind<- apply(pca_cov$scores[,1:2],1,sum)
cost_ind
which.min(apply(pca_cov$scores[,1:2],1,sum))
which.max(apply(pca_cov$scores[,1:2],1,sum))

# Then we find that San Diego is the state where the life is least expensive.
# Honolulu is the most expensive State followed by New York. 

### PCA with "prcomp" function ###
?prcomp
pca_pr_cov<-prcomp(food[,1:5]) 
# If the parameter "scale=TRUE" is added, correlation matrix is used in the analysis.

# Standard deviation and Proportion of Variances per components
summary(pca_pr_cov)
names(pca_pr_cov)

# "rotation" gives the loading vectors for PCs 
pca_pr_cov$rotation

#"x" gives the scores for the States
pca_pr_cov$x 

# Biplot is the  same with previous one
biplot(pca_pr_cov)

#The most expensive States according to the components
# Since the loadings are positive we take the max score. 
which.max(pca_pr_cov$x[,1])
which.max(pca_pr_cov$x[,2])

###################################
### PCA with FactoMineR Package ###
###################################
res_PCA<-PCA(food,scale=TRUE, graph=FALSE) # by default scale=TRUE
# graph=TRUE by default, gives all the graphs
# Stadardized PCA on correlation matrix is performed

# You can see the elements of the output of PCA 
res_PCA

# eigenvalues and percentage of variances
res_PCA$eig

# The loadings of the variables on the components
res_PCA$var$coord 
res_PCA$var$cor

# The plot of variable coordinates 
# This plot shows correlation of variables with the components.
# In other words, loadings.
# Biplot just shows the loadings not the correlations!!
plot(res_PCA,choix="var")

# Orange is correlated with the second component,
# Other aliments are correlated with the second component.

# The scores of the individuals
res_PCA$ind$coord

# The plot of individual scores on 2 dimensiones
plot(res_PCA,choix = "ind")

# The highest score on the first dimension belongs to Honolulu and then New York.
# The highest score on the second component belongs to Milwaukee.
############################################################################################


#####################################################
########## FACTOR ANALYSIS WITH ROTATION ############
## Rcmdr command to import excel data domes.
## domes <- 
# readXL("C:/Users/Nihan/Desktop/SMDE_UPC/scripts/domes.xls", 
##       rownames=FALSE, header=TRUE, na="", sheet="Sayfa1", 
##       stringsAsFactors=FALSE)

## domes data set is from the book: 
## "Handbook of Univariate and Multivariate Data Analysis with IBM SPSS".
## by Robert Ho.

## For the detailed information please check the Word document.

head(domes)
colnames(domes)

data<-domes[,5:13]

## Principal Components and factor analysis is similar but not exactly the same.
## In Factor Analysis we do rotation.
## In Factor analysis we can decide on the number of components or 
# we might do rotation in order to load each variables just on one factor.
# Factor analysis aims to find the latent structure behind the variables.
# It is important to name the factors considering the loading of the variables.

?factanal

#If there are missing observations in the data use="complete.obs".
?cor
R<-cor(data) 
R

#Factor Analysis (we use the correlation matrix R)
fit <- factanal(data, 3, covmat=R, rotation="none")
names(fit)

#the number of factors
fit$factors 
# the loading of variables on the factors
fit$loadings
# Let us see just the loadingss exceeding 0.3.
print(fit, digits=2, cutoff=.3, sort=TRUE)

#Varimax Rotation
fit_var <- factanal(data, 3, covmat=R, rotation="varimax")
load_fit<-fit_var$loadings[,1:3] 
print(fit_var, digits=2, cutoff=.3, sort=TRUE) 

# The Graph of Factor Loadings
plot(load_fit,type="n") # set up plot
text(load_fit,labels=names(data),cex=.7)

# The variables mental, insane, stable are loaded on the second factor.
# The variables protect, defende and save are loaded more on the first factor.
# Provo, passion and caused loaded on the third factor.

########################################################################################################

