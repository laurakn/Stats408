# Laura Niss
# 408 lab 1

# This will be an introduction to R as well as looking at some of the distributions mentioned in lecture
# If you would like to add notes to this script, put a "#" infront of your comments and R will ignore it
# Always remember, 99.99999% of all questions can be answered easily by google, and the remaining
# can be answered eventually with enough poking around in google
install.packages('reshape2')
library(ggplot2)
library(reshape2)

### you can do calculations in R
2 + 2
1 / 124543
(56)^3
8*9

### Generating Bernoulli data
# try running the code below, then change the values of "p" and "n" to see what happens
p <- .5 # probability
# rbinom(n, size, prob), this is the function we use to generate both Bernoulli and Binomial data
n <- 10 # number of trials
s <- 1 # sample size in each trial
set.seed(1)
sample <- rbinom(n, s, p) 

qplot(x=seq(n), y=sample)

### Genertating binomial from Bernoulli
# remember when we said a binomial random variable was just the sum of Bernoulli random variables?
# lets generate some binomial samples from the bernoulli distribution
p <- .5
m <-  10
n <- 10 

s <- 1
x <- 0
for (i in 1:m){
  set.seed(i)
  x[i] = sum(rbinom(n, s, p)) # since s=1, this is generating n=10 Bernoulli trials, and them summing
                              # them together to create a sample from the Binomial distribution.
                              # this is repeated m=10 times to give us 10 sample points from the Binomial dist.
}
qplot(x=seq(n), y=x)
qplot(x=seq(n), y=sort(x))

# Now lets compare that with generating m=10 binomial samples directly
set.seed(1)
s <- m
y <- rbinom(n, s, p) # n=10, so we're generating 10 trials of size=10
qplot(x=seq(m), y=y)
qplot(x=seq(m), y=sort(y))

df <- data.frame(id=seq(n), sort(x), sort(y))
df.m <- melt(df, 'id')

ggplot(df.m) + geom_point(aes(x=id, y=value, color=variable))

### Generating Binomial samples. Try changing the values of "p", "s", and "n" to see how 
# it affects your sample
p <- .5
s <-  10
n <- 10
y <- rbinom(n, s, p) 
qplot(x=seq(n), y=y)


### Making histograms
# Generate some data using the rbinom function and then plotting it
# Remember, for generating a binomial sample, s=size of the trial, n=number of trials
# p=probability of success. Lets assume we want 50 datasets of size 23 with the probability of 
# success being p=14/79. Then
s=23
n=50 
p=14/79
set.seed(1)
data <- rbinom(n, s, p)/23 
hist(data, freq=FALSE) 
hist(data, freq=FALSE, main="Histogram of Successes", xlab="Proportion of Success",
     col="green") 
# Does this graph make sense?
# Does it make sense to say "the proportion of successes is near .05?"
# If our data came from testing talk therapy against just chatting with a friend,
# and there was no difference between groups when participants were afterwards asked, "Do you 
# feel better," around where would you guess was the proportion of talk therapy participants who said "yes"?
# To save a graph, press "Export" and choose an option.

## What if we want to stratify our data by gender?
# let's say we have 301 subject, with 41% women and 59% men, and that women felt better after talk therapy
# 30% of the time, and men felt better after talk therapy 43% of the time. Can we model this with
# a binomial distribution? Why or why not? Discuss with a partner.

