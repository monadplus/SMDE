##### Chi square Test ###########

#### Example: Horoscope Signs ####

astro<-matrix(c(888,1025,1032,1055,861,1005,1065,1069,914,1031,1008,
                1047,850,991,1039,1120),nrow=4,ncol=4, byrow=TRUE)
astro

##Joint Probabilities
pt<-prop.table(astro)
pt

## Marginal Probabilities
?margin.table
margin.table(astro)
margin.table(astro,1)
margin.table(astro,2)

row_marg<-margin.table(pt,1)
col_marg<-margin.table(pt,2)

### Conditional Prob ###
?sweep
sweep(pt,1,row_marg,'/')

## Test of Independence ##
chisq.test(astro) # Since the p > \alpha, then we can't refuse the H_0


###  Chi-Square Test for Given Probabilities ###
### Example: Is the die biased? ###
pr<-rep(1/6,6)
chisq.test(c(2,12,25,18,17,15),p=pr) # Not following an uniform probability

####Exercise 1: Epilachna varivestis (Poisson Dist) ###
### Expected Value
ind<-c(0,1,2,3,4,5)
freq=c(12,56,23,10,5,4)
l<-sum(ind*freq)/110

p1<-dpois(0,l)# 
p1
p2<-dpois(1,l)
p2

p<-vector()

for (i in 0:5){
p[i+1]<-dpois(i,l)
}
p

p<-p/sum(p)
p
sum(p) # 1 

n=sum(freq)
exp<-n*p
chi2<-sum((freq-exp)^2/exp)
chi2

?pchisq
pval<-1-pchisq(chi2,5) # 1 - F(chi2)
pval

chisq.test(freq,p=p) # Warning, freq contains a value smaller than 5.

### Merging Categories ###
exp
exp2<-c(exp[1:4],exp[5]+exp[6])
freq2<-c(freq[1:4],freq[5]+freq[6])

chi2_new<-sum((freq2-exp2)^2/exp2)
chi2_new

pval2<-1-pchisq(chi2_new,4)
pval2

p2<-c(p[1:4],p[5]+p[6])
sum(p2)


######### Exercise 2: Internet Shopping ######
# See slides:

shop<-matrix(c(399,119,39,261,72,50,284,97,20,263,51,15,393,143,41,
         531,145,97,502,150,86),nrow=3,ncol=7)
shop

margin.table(shop)
margin.table(shop,1)
margin.table(shop,2)


pshop<-prop.table(shop)
margin.table(pshop,1)
margin.table(pshop,2)

chi_shop<-chisq.test(shop)
names(chi_shop)
chi_shop$expected
chi_shop

##### Comparing Means of Two Groups ####
############### t-Test #################

##### Assumption 1 ########
#### Testing Normality ###
?shapiro.test

cars
hist(cars$speed)
hist(cars$dist) # It's easy to see that the distribution is skew to the left.

shapiro.test(cars$speed) # Follows a normal distribution
shapiro.test(cars$dist) # Does not follow a normal distribution


## Categorizing Distance ##
summary(cars$dist)
cars$dist_cat<- cut(cars$dist,c(2,36,120))
levels(cars$dist_cat)<-c("1","2")

### Checking distribution of speed across groups ###
?which
g1<-which(cars$dist_cat=="1")
g2<-which(cars$dist_cat=="2")

plot(density(cars$speed[g1]),main="Density curves of 2 Groups",xlim=c(0,35))
lines(density(cars$speed[g2]),col=2)


######### Assumption 2 ##################
### Homogenity of Variances ###
# We need to test that the variances are equal
var.test(cars$speed~cars$dist_cat,data=cars)
# ^^^^ linear model: speed~distance

####### t-test #########
?t.test
t.test(cars$speed~cars$dist_cat,var.equal=TRUE) # Means are not the same

plot(cars$speed~cars$dist_cat)



