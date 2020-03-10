#### Importing Data ####

### Importing txt data ###

### From the website: 
### http://archive.ics.uci.edu/ml/datasets/Car+Evaluation

# 1.- Download the data set and rename it to *.txt

car<-read.table("/home/arnau/MIRI/smde/lab-18-11-20/car.txt")
?read.table # ! The set was not properly read
car
summary(car)
head(car)

car<-read.table( "/home/arnau/MIRI/smde/lab-18-11-20/car.txt"
               , sep=","
               # not working.. , col.names = c("buying", "maint", "doors", "persons", "lug_boot", "safety")
               )
summary(car)
# Add missing headers
colnames(car) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety")


### Importing xls data ###
??read_excel
install.packages("readxl")
library(readxl)

??read.xlsx
install.packages("openxlsx")
library(openxlsx)

# source = https://archive.ics.uci.edu/ml/datasets/Wine+Quality

# read_excel ~ wine_xl2
wine_xl1<-read_excel("/home/arnau/MIRI/smde/lab-18-11-20/winequality-red.xlsx")
summary(wine_xl1)

wine_xl2<-read.xlsx("/home/arnau/MIRI/smde/lab-18-11-20/winequality-red.xlsx")
summary(wine_xl2)


### Importing csv data ###
?read.csv2
red<-read.csv2("/home/arnau/MIRI/smde/lab-18-11-20/winequality-red.csv")
summary(red)
red<-read.csv2("/home/arnau/MIRI/smde/lab-18-11-20/winequality-red.csv",dec=".")

white<-read.csv2("/home/arnau/MIRI/smde/lab-18-11-20/winequality-white.csv",dec=".")
summary(white)
head(white)

### Importing web data ###
redwine<-read.csv2( "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
                   , dec="."
                   )

summary(redwine)

####### Creating a Factor ####

dim(red)
dim(white)

# Combine the rows
winequal<-rbind(red,white)
dim(winequal)

# Which rows belongs to red, which to white
winequal$cat<-as.factor(rep(c(1,2),c(nrow(red),nrow(white))))
head(winequal)
summary(winequal)
# Define the levels of the cat. variable
levels(winequal$cat)<-c("red","white")

##############################################################
### Converting a Numerical Variable to a Categorical Variable 
##############################################################

summary(winequal$quality)

winequal$qual_cat<- cut(winequal$quality,c(1,5.5,10))
levels(winequal$qual_cat)<-c("Low","High")

winequal$qual_cat<- cut(winequal$quality,c(2,5,6,9))
summary(winequal$qual_cat)
levels(winequal$qual_cat)<-c("Low","Medium","High")

##################################
##### Factor Data and Tables #####
##################################

?mtcars
summary(mtcars)
# Something is strange with vs, am ... they are boolean!
# Let's transform them to boolean instead of numeric
mtcars[,8]<-as.factor(mtcars[,8])
mtcars[,8]
mtcars[,9]<-as.factor(mtcars[,9])
summary(mtcars)

## Alternatively ##
for (i in 8:9){
mtcars[,i]<-as.factor(mtcars[,i])
}

summary(mtcars)

levels(mtcars[,8])<-c("V-shaped","straight")
levels(mtcars[,9])<-c("automatic","manual")

## Cross Table ##

# Intersection of two categories.
tab<-table(mtcars$vs,mtcars$am)
tab


?prop.table

#Joint Probability
prop.table(tab) # Sum of all should be 1

#Marginal Probabilities
colSums(prop.table(tab)) # Sum should be 1
rowSums(prop.table(tab)) # Sum should be 1

#Conditional Probabilities
# Row Profile
prop.table(tab,1) # P(automatic | V-shaped) = 0.6667

#Column Profile 
prop.table(tab,2)


#######################
###Probability in R ###
#######################

install.packages("prob")
library(prob)

##Tossing one coin
tosscoin # see the implementation
tosscoin(1) # Ω of rolling 1 = 2^1 = 2
tosscoin(3) # Ω of rolling 3 = 2^3 = 8

rolldie(1) # 6^1 = 6
rolldie(2) # 6^2 = 36

cards
cards() # All cards 52.
S<-cards() # Sample Space
?subset
# All cards that are "Heart"
A<-subset(S,suit=="Heart")
# All cards that have a value [7,9]
B<-subset(S,rank %in% 7:9)

p=rep(1/6,6) # Rolling a dice 1/6
outcomes=rolldie(1)
# Combine both
probspace(outcomes,probs=p)
# Same as before
probspace(outcomes)

S <- cards(makespace = TRUE) # Prob is attached
A <- subset(S, suit == "Heart")
B <- subset(S, rank %in% 7:9)

Prob(A) # 13/52
Prob(S, suit == "Heart") # Same as A

S=rolldie(2,6,TRUE)
?rolldie
A=subset(S,X1==X2)
B=subset(S,X1+X2>=8)

Prob(A) # 6/36
Prob(B) # 15/36  0.4166
# P(A|B) = P(A AND B) / P(B)
Prob(A, given=B) # (3/36) / (15/36) = 0.2
Prob(S, X1==X2, given = (X1 + X2 >= 8))

Prob(B,given=A)
Prob(S, X1+X2 >= 8, given = (X1==X2) )

# Test independence:
# P(A|B) = P(A)

##########################
#### Birthday Problem ####
##########################

?qbirthday

qbirthday(prob=0.5,365,2)

pbirthday(9,classes=365,coincident=2)
pbirthday (23,365,2)


library(Rcmdr) # R commander
library(RcmdrPlugin.IPSUR)


#### Data Sources ####
# http://archive.ics.uci.edu/ml/index.php
# https://www.kaggle.com/datasets




