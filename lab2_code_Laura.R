# Laura Niss
# Lab2

# reading histograms
# note: while we always talk about binomail(n, p) in class, in my R code this is 
# equivalent to binomial(s, p), I'm using "s" to be consisnt with the R documentation
s=15
n=100 
p=.25
set.seed(1)
data <- rbinom(n, s, p)

data_proportion <- data/s

hist(data, breaks = 10, freq = FALSE, main = "Histogram of Successes", 
     xlab = "Count of successes", 
     ylab = "Component Density", col = "green")

hist(data, breaks = 10, freq = TRUE, main = "Histogram of Successes", 
     xlab = "Count of successes",
     ylab = "Count of Trials", col = "green")

hist(data_proportion, breaks = 10, freq = FALSE, main = "Histogram of Successes", 
     xlab = "Proportion of successes",
     ylab = "Component Density", col = "green")

hist(data_proportion, breaks = 10, freq = TRUE, main = "Histogram of Successes", 
     xlab = "Proportion of successes",
     ylab = "Count of Trials", col = "green")
# In general, I would recommend "freq=TRUE" and graphing the proportions.

############################
# Lets look at how the sample mean converges to the expectation (law of large numbers), 
# and how variance changes with sample size
# X~Binomial, E[X]=np, Var[X]=np(1-p)
# change n, s, and p to see how large samples compare to small samples and how a 
# large number of trials compares to a small number. When both are large, what does
# the distribution look like?
s <- 30
p <- .5
n <- 10
set.seed(3)
binom_sample <- rbinom(n, s, p)
hist(binom_sample, breaks = 8, freq = TRUE, col = "green")
t_mean <- s*p # true mean
t_mean
mean(binom_sample)

t_var <- s*p*(1-p) # true variance
t_var
var(binom_sample)


# Compare to normal dist. with same mean and variance
# rnorm(n, mean = 0, sd = 1)
set.seed(3)
norm_sample <- rnorm(n, t_mean, sqrt(t_var))
hist(norm_sample, breaks = 8, freq = TRUE, col = "green")
# extra: compare sample mean and variance of our norm_sample to the true mean and variance.
# how does sample size affect this?

# Try setting xlim, ylim to compare histograms better

# Question: how does the normal sample differ from the binomial?

# Question: how big do you think s compared to p needs to be for a good normal approximation 
# of the binomial?

# Ex:
n <- 10
s <- 10
p <- .9999999
set.seed(3)
binom_ex <- rbinom(n, s, p)
hist(binom_ex, breaks = 8, freq = TRUE, col = "green") # binomial
hist(rnorm(n, s*p, sqrt(s*p*(1-p))), breaks = 8, freq = TRUE, col = "green") # norm approx
hist(rep(10, 10), breaks = 8, freq = TRUE, col = "green")# choose 10 each time with prob 1
# which one is better at approximating our binomial dist?

############################
# Challenge: create example of Simpson's paradox using rmultinom(n, size, prob)
# Ex. of rmultinom usage
n <- 1000
s <- 33
p <- c(.2, .3, .4, .1) # must sum to 1!
set.seed(3)
multi_sample <- rmultinom(n, s, p)
df <- data.frame(multi_sample)
multi_sample[,1]
mean_cat1 <- mean(multi_sample[1,])
mean_prop_in_cat1 <- mean_cat1 / s
