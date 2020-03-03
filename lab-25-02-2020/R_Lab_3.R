
###################################
### Generating Random Variables ###
###################################

## Uniform Distribution ###
?runif
# generating a random number
runif(1)    
#generating 3 random numbers
runif(3)    
# generating 3 random numbers ona defined range 
runif(3, min=5, max=10)    
runif(5, min=0.25, max=0.5)

#generating an uniform random vector of 100 observations for the interval [0,1]
vec_u<-runif(100,0,1)
hist(vec_u)


## Normal Distribution ###
?rnorm
# generating 1 random number from a standard normal distribution
rnorm(1)   
# generating 3 random numbers from a standard normal distribution
rnorm(3)    
# generating 3 random numbers from a Normal distribution with a specific mean and standard deviation
rnorm(3, mean=10, sd=2)   

# generating three different samples of lenght 100 observations from a Normal distribution

vec_n1<-rnorm(100, mean=60, sd=5)
hist(vec_n1) 

vec_n2<-rnorm(100, mean=40,sd=5)
hist(vec_n2)

vec_n3<-rnorm(100, mean=60, sd=10)   
hist(vec_n3)

?density
plot(density(vec_n1))


## Arranging limits of axes ###
plot(density(vec_n1),xlim=c(10,100),ylim=c(0,0.08), main="Three different Normal distributed samples")
lines(density(vec_n2),col=2)
lines(density(vec_n3),col=3)

### Poisson Distribution ###
?rpois
vec_p1<-rpois(50,10)
hist(vec_p1)

vec_p2<-rpois(100,10)
hist(vec_p2)

vec_p3<-rpois(1000,10)
hist(vec_p3)


############ Expected Value and Variance #############################
### Expected Value of Rolling a Die ###

p_die <- rep(1/6,6)
die <- 1:6
exp_val<-sum(die*p_die)
exp_val

?sample
sample(die, size=10, prob=p_die, replace=T)
hist(sample(die, size=1000, prob=p_die, replace=T))

s <- table(sample(die, size=1000, prob=p_die, replace=T))

#### Distribution of Sample Means ####

numeric()
############################################
## Function for generating n sample means ###
gen_sample_means <- function(n) {
  sample_means <- numeric()
  for (i in 1:1000) { 
    sample_means <- append(sample_means, sum(sample(die, size=n, prob=p_die, replace=T))/n)
  }
  return (sample_means)
}
###############################################

sample_means <- gen_sample_means(100)
hist(sample_means)
plot(density(sample_means), main="Distribution of the sample means",xlab="sample mean", col="red")

mean(sample_means)
sd(sample_means)
var(sample_means)