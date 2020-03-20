##Introduction to R

options(digits=16)
10/3

#Square root
sqrt(2)

exp(1) # Euler's constant e 

pi

options(digits=7) # back to the default

x<-7*41/pi
x

sqrt(-1)
sqrt(-1+0i)
sqrt(as.complex(-1))
(0+1i)^2
typeof((0+1i)^2)

# c means "combine"
x<-c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x
x <- c(1:5, 10.5, "next")
x

## Conversion to numeric
as.numeric("2")
xa<-c(1,"2")
newXa <- sapply(xa, as.numeric)
sapply(x, +1)
length(newXa)

# Vectors
seq(from = 1, to = 5)
seq(from = 2, by = -0.1, length.out = 4)

x <- c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x[1]
x[2:4]
x[c(1,3,4,8)]
# All except those marked with '-'
x[-1]
x[-c(1,3,4,8)]

LETTERS[1:5] # "A" "B" "C" "D" "E"
letters[-(6:24)]

#Data Frames
x <- 5:8
x
y <- letters[3:6]
# length x = length y
A <- data.frame(x, y) # Like a matrix
A
A <- data.frame(v1 = x, v2 = y)
A
# x y
# 1 5 c
# 2 6 d
# 3 7 e
# 4 8 f

# help
?data.frame

A[3, ] # 7 e
A[1, ] # 
A[, 2] # y = c d e f

##Functions and Expressions
x <- 1:5 
sum(x) # 15

length(x) # 5
min(x) # 1
max(x) # 5

#Sample mean
mean(x) # 3

#Sample standard deviation
sd(x) # 1.581139

?intersect
intersect
y <- 1:3 
intersect(x,y) # 1 2 3

# Function
plus2 <- function (x) {
   x + 2 
}
plus2(2) # 4
sapply(x, plus2) # 3 4 5 6 7

##Scan
#x<-scan()

# importing csv
sleepHours <- read.csv( "/home/arnau/MIRI/smde/lab00/data.csv"
                    , header = TRUE
                    , sep = ","
                    )
sleepHours$age
sleepHours$sleep_hours[1]
plot(sleepHours$age,sleepHours$sleep_hours,type="l",col="red")

### Displaying Data ###

### Data classification

# - Quantitative (discrete, continuous)
# - Qualitative (enums: nominal, ordinal)
# - Logical

## Numerical Data

# discret/continuous - small data set
?stripchart
str(airquality) # rivers, discoveries are like existing data frames.
stripchart(airquality) # nice plot
stripchart(precip, xlab = "rainfall")
stripchart(rivers, method = "jitter", xlab = "length")
stripchart(discoveries, method = "stack", xlab = "number")

stripchart(airquality$Ozone,
           main="Mean ozone in parts per billion at Roosevelt Island",
           xlab="Parts Per Billion",
           ylab="Ozone",
           method="jitter",
           col="orange",
           pch=1
)

## Histogram: continuous or qualitative data freq.
hist(precip, main = "Precipitation")
hist(precip, freq = FALSE, main = "") # density
# breaks: number of cells to split the range
hist(precip, breaks = 10, main = "My title")
hist(precip, breaks = 200, main = "")

## Stemplots
library(aplpack)
UKDriverDeaths
stem.leaf(UKDriverDeaths, depth = FALSE)

##Index Plot (~ Scatter)
plot(LakeHuron, type = "h") # LakeHuron is a vector of 91 positions
plot(LakeHuron, type = "p")

## Box Plot
boxplot(count~spray, ylab="count", xlab="spray", data=InsectSprays)
head(InsectSprays)
dim(InsectSprays) # 72 2 (two variables)
colnames(InsectSprays) # "count" "spray"
summary(InsectSprays) # right is category:#samples


### Linear Models & Linear Regression

heigth <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
heigths <- data.frame(heigth, weight)

scatter.smooth(x=heigth, y=weight, main="Weight ~ Heigth")

## Linear model
linearMod <- lm(weight~heigth) # lm(w~h, data=heigths)
summary(linearMod)

## Linera regression
# Predict: weight of a person with height 170.
a <- data.frame(heigth = 170)
result <- predict(linearMod,a)
result # 76.22869

# TODO example of linear regression
par(mgp=c(2,1,0), mar=c(3,3,1,1))
# Fit regression line
require(stats)
reg<-lm(weight ~ heigth, data = heigths)
coeff=coefficients(reg)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(cars, main=eq)
abline(reg, col="blue")


# Write to file
png(file = "/home/arnau/MIRI/smde/lab00/linearregression.png")
# > plot
dev.off()
