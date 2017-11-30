# Laura Niss, stats 408

### for homework
help(pbinom) # gives info on what binomial function does

pbinom(5, 10, .2) # prob. of seeing 5 or fewer successes in a sample size of 10, with probability of success = 0.2
# in other words, pbinom is the cdf for a binomal distribution with specified n, p.
# That is, if X ~ binom(n, p), this function returns P(X <= y), probability x is less than or equal to y,
# for y between 0 and n

# tip, remember that x/y = 1/(y / x)

### Linear regression

# get some data 
library(MASS)
data(Boston)
names(Boston)

lm.fit = lm(medv~lstat, data=Boston)
summary(lm.fit)

names(lm.fit) # to find out information stored in lm.fit
coef(lm.fit) # gives coefficients of lm.fit()
confint(lm.fit) # gives confidence intervals of lm.fit

plot(lstat, medv) # Plots data points only.
abline(lm.fit, col='red') # Plots regression line.

# Can generate our own data, lets say x = study time, y = exam score
x <- rnorm(100, mean = 10, sd = 3) 
b0 <- 50
b1 <- 3
e <- rnorm(100, mean = 0, sd = 5)
y <- b0 + b1*x + e
y[y > 100] <- 100 # make max 100


df <- data.frame(y, x)

lm.fit <- lm(y~ ., data = df)
summary(lm.fit)

coef(lm.fit) # gives coefficients of lm.fit
confint(lm.fit) # gives confidence intervals of lm.fit

plot(x, y)
abline(lm.fit, col = 'red')

# Try it out yourself. Generate some data (try include multiple predictors), 
# then fit a model using the lm() function. 

# what does confint() return? How do we interpret the coefficients, and what information does the confidence interval
# provide?


############################
### Log odds
# from lecture,  
pD = 1/30 # proportion adverse given drug
pP = 1/35 # proportion adverse given placebo
n.adverse <- c(1,1) # number of adverse effects
n <- c(30,35) # sample size
D <- c(1,0) # D = 1 drug, D = 0 placebo
Y <- cbind(n.adverse, n - n.adverse)

logit.fit <- glm(Y ~ D, family = binomial(link = logit)) # need link = logit to get logit of Y
summary(logit.fit)
names(logit.fit) # list of what's in logit.fit
coef(logit.fit)

a <- logit.fit$coef[1] # alpha
b <- logit.fit$coef[2] # beta
names(a) <- NULL
names(b) <- NULL

c(exp(a) / (1 + exp(a)), 1/35) # First is our Pp from the logistic funtion, second is sample prop, both estimate PP
c(exp(a + b) / (1 + exp(a + b)), 1/30) #both estimate PD
c(log((1 / 29) / (1 / 34)), b) #log(OR)

confint(logit.fit) # coefficient for intercept is a (alpha), coefficient for D is b (beta)

### similarity to linear regression
library(arm)
p <- n.adverse / n
Y <- logit(p)
lm.fit <- lm(Y ~ D)
summary(lm.fit)
# compare to
summary(logit.fit) # our logisitc regression
# What do you notice?

coef(lm.fit)
coef(logit.fit)

### What if we change pDP= 0/35 = 0
pD = 1/30 # proportion adverse given drug
pP = 0/35 # proportion adverse given placebo
n.adverse <- c(1, 0) # number of adverse effects
n <- c(30,35) # sample size
D <- c(1,0) # D = 1 drug, D = 0 placebo
Y <- cbind(n.adverse, n - n.adverse)

logit.fit <- glm(Y ~ D, family = binomial(link = logit)) # need link = logit to get logit of Y
summary(logit.fit)
names(logit.fit) # list of what's in logit.fit
coef(logit.fit)

a <- logit.fit$coef[1] # alpha
b <- logit.fit$coef[2] # beta
names(a) <- NULL
names(b) <- NULL

c(exp(a) / (1 + exp(a)), 0/35) # First is our Pp from the logistic funtion, second is sample prop, both estimate PP
c(exp(a + b) / (1 + exp(a + b)), 1/30) #both estimate PD
c(log((1 / 29) / (0 / 34)), b) #log(OR)

confint(logit.fit)

####################
### Monte Carlo Method, find distribution of estimated b (beta) (numbers not the same as in homework!)
n.promotions <- 55 # total number of promotions
n.total <- 1000 # total number of people eligible for promotions
prop.promoted.women <- 27/55 # proportion of promoted who are women
prop.total.women <- 570/1000 # proportion of eligible for promotion who are women, equal to pi
D <- c(1, 0) # 1 promoted

n.promoted.women <- rbinom(1000, n.promotions, prop.promoted.women) # 1000 samples of number of  
  # women were promoted out of 41
n.total.women <- rbinom(1000, n.total, prop.total.women) # 1000 samples of number of women eligible to be promoted
n <- c(n.promotions, n.total)

est.beta <- rep(NA, 1000) # create empty vector to store estimated betas
for (i in 1:10000) {
  n.women = c(n.promoted.women[i], n.total.women[i])
  est.beta[i] = glm(cbind(n.women, n - n.women) ~ D, family = binomial(link = logit))$coef[2]
}

exp(quantile(est.beta, c(.025, .975))) # 95% interval for OR

# now compare to CI given by logit
n.women <- c(27, 570)
n.total <- c(55, 1000)
D <- c(1,0);
Y <- cbind(n.women, n.total - n.women)
logit.fit <- glm(Y~ D, family=binomial(link=logit))
confint(logit.fit)
exp(confint(logit.fit)[2,]) # 95% interval for OR
