# Laura Niss
# lab3

# visualing stats: https://students.brown.edu/seeing-theory/
# has great set theory and conditional probability examples

require(ggplot2)
require(reshape2)
######### Comparing distributions with histograms
s <- 30
p <- .5
n <- 100
set.seed(3)
prop_1 <- rbinom(n, s, p)/s 

s <- 50
p <- .4
n <- 100
set.seed(3)
prop_2 <- rbinom(n, s, p)/s

df <- data.frame("sample 1" = prop_1, "sample 2" = prop_2)

ggplot(melt(df), aes(value, fill = variable)) + geom_histogram(position = "dodge")

# Would it be a good idea to look at the hist comparing counts instead of proportions? 
# Try it to test.

# Comparing binomial and normal review

s <- 30
p <- .5
n <- 1000
set.seed(3)
binom_sample <- rbinom(n, s, p)
hist(binom_sample, breaks = 8, freq = TRUE, col = "green")

t_mean <- s*p # true mean
t_var <- s*p*(1-p) # true variance
set.seed(3)
norm_sample <- rnorm(n, t_mean, sqrt(t_var))
hist(norm_sample, breaks = 8, freq = TRUE, col = "green")

######## Simpsons paradox using Kidney example from wikipedia
# S = small stones, L = large stones, A = treatment A, B = treatment B
SA <- 81/87
SB <- 234/270
LA <- 192/263
LB <- 55/80

# total is 700
# have 8 categories, so need 8 probabilities (success and failure for each pair)
p <- c(81/700, 234/700, 192/700, 55/700, 6/700, 36/700, 71/700, 25/700) 
#      SA     SB        LA        LB      SAF     SBF     LAF     LBF
s <- 700
n <- 1000

set.seed(3)
multi_sample <- rmultinom(n, s, p)
df <- data.frame(multi_sample)
sum(df[c(1,3),])/(s*n/2) #prop_success_A
sum(df[c(2,4),])/(s*n/2) #prop_success_B

sum(df[1,])/sum(df[c(1, 5),]) #prop_success_A given S
sum(df[2,])/sum(df[c(2, 6),]) #prop_success_B given S

# try changing the numbers in each group, how does this affect the data generated?
# make sure the sum is still 700

# Now lets compare using binomial so we can control the size of each group
# first try it with the same sizes as given in the original trial, then try other sizes
sSA <- 87
sLA <- 263
sSB <- 270
sLB <- 80
# make sure sSA + sLA = 350 and sSB + sLB = 350

set.seed(3)
m <- matrix(c(rbinom(n, sSA, SA), 
              rbinom(n, sSB, SB), 
              rbinom(n, sLA, LA), 
              rbinom(n, sLB, LB)),
            nrow = 4, ncol = 1000, byrow = TRUE)
df2 <- data.frame(m)

sum(df2[c(1,3),])/(s*n/2) #prop_success_A
sum(df2[c(2,4),])/(s*n/2) #prop_success_B

sum(df2[1,])/(n*sSA) #prop_success_A given S
sum(df2[2,])/(n*sSB) #prop_success_B given S

# What happens when the groups are all the name size?


# Use Binomial or Multinomial model?
# think of the example above, what would be the model using just one binomial? What would
# it be using the multinomial? Compare the two.

