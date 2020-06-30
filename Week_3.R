#Week 3 > Inference I: P-values, Confidence Intervals and Power Calculations > T-test Exercises

#T-test Exercises

babies <- read.table("babies.txt", header=TRUE)
babies
library(rafalib)
library(dplyr)
mypar()
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

#T-test Exercises #1
#Q: Set the seed at 1 and obtain a samples from the non-smoking mothers (dat.ns) of size  N=25 . Then, without resetting the seed, take a
#   sample of the same size from and smoking mothers (dat.s). Compute the t-statistic (call it tval).

#   What is the absolute value of the t-statistic?

N=25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)

X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)

X.s <- mean(dat.s)
sd.s <- sd(dat.s)

sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.s - X.ns)/sd.diff
abs(tval)

1.6593

#T-test Exercises #2
#Q: Recall that we summarize our data using a t-statistics because we know that in situations where the null hypothesis is true (what we mean when
#   we say "under the null") and the sample size is relatively large, this t-value will have an approximate standard normal distribution. Because we
#   know the distribution of the t-value under the null, we can quantitatively determine how unusual the observed t-value would be if the null
#   hypothesis were true.

#   The standard procedure is to examine the probability a t-statistic that actually does follow the null hypothesis would have larger absolute value
#   than the absolute value of the t-value we just observed -- this is called a two-sided test.

#   We have computed these by taking one minus the area under the standard normal curve between -abs(tval) and abs(tval). In R, we can do
#   this by using the pnorm() function, which computes the area under a normal curve from negative infinity up to the value given as its first
#   argument:

pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval

0.09705034

#T-test Exercises #3
#Q: Because of the symmetry of the standard normal distribution, there is a simpler way to calculate the probability that a t-value under the null
#   could have a larger absolute value than tval. Choose the simplified calculation from the following:

#>> 2*pnorm(-abs(tval))

#Week 3 > Inference I: P-values, Confidence Intervals and Power Calculations > Confidence Intervals Exercises

#Confidence Intervals Exercises

babies <- read.table("babies.txt", header=TRUE)
babies
library(rafalib)
library(dplyr)
mypar()
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

#Confidence Intervals Exercises #1

#Q: Set the seed at 1 and obtain two samples, each of size N = 25, from non-smoking mothers (dat.ns) and smoking mothers (dat.s). If instead of CLT,
#   we use the t-distribution approximation, what do we add and subtract to obtain a 99% confidence interval (use 2*N-2 degrees of freedom)?

N <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N) 
dat.s <- sample(bwt.smoke, N) 
qt(0.995,48)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
##note that if you define dat.s before dat.ns, you get a different answer
##due to sampling randomness
##tolerance is set to accept both answers

13.70744

#Confidence Intervals Exercises #2

#Q: No matter which way you compute it, the p-value pval is the probability that the null hypothesis could have generated a t-statistic more
  # extreme than than what we observed: tval. If the p-value is very small, this means that observing a value more extreme than tval would be
  # very rare if the null hypothesis were true, and would give strong evidence that we should reject the null hypothesis. We determine how small the
  # p-value needs to be to reject the null by deciding how often we would be willing to mistakenly reject the null hypothesis.
  # 
  # The standard decision rule is the following: choose some small value [mathjaxinline]\alpha[/mathjaxinline] (in most disciplines the conventional
  # choice is [mathjaxinline]\alpha = 0.05[/mathjaxinline]) and reject the null hypothesis if the p-value is less than
  # [mathjaxinline]\alpha[/mathjaxinline]. We call [mathjaxinline]\alpha[/mathjaxinline] the significance level of the test.
  #   
  # It turns out that if we follow this decision rule, the probability that we will reject the null hypothesis by mistake is equal to
  # [mathjaxinline]\alpha[/mathjaxinline]. (This fact is not immediately obvious and requires some probability theory to show.) We call the event of
  # rejecting the null hypothesis, when it is in fact true, a Type I error, we call the probability of making a Type I error, the Type I error rate, and we say
  # that rejecting the null hypothesis when the p-value is less than [mathjaxinline]\alpha[/mathjaxinline], controls the Type I error rate so that it is
  # equal to [mathjaxinline]\alpha[/mathjaxinline]. We will see a number of decision rules that we use in order to control the probabilities of other
  # types of errors. Often, we will guarantee that the probability of an error is less than some level, but, in this case, we can guarantee that the
  # probability of a Type I error is exactly equal to [mathjaxinline]\alpha[/mathjaxinline].
  #   
  # Which of the following sentences about a Type I error is not true?

#>> From the original data alone, you can tell whether you have made a Type I error.

#Confidence Intervals Exercises #3
#Q: In the simulation we have set up here, we know the null hypothesis is false -- the true value of difference in means is actually around
#   [mathjaxinline]8.9[/mathjaxinline]. Thus, we are concerned with how often the decision rule outlined in the last section allows us to conclude that
#   the null hypothesis is actually false. In other words, we would like to quantify the Type II error rate of the test, or the probability that we fail to
#   reject the null hypothesis when the alternative hypothesis is true.
# 
#   Unlike the Type I error rate, which we can characterize by assuming that the null hypothesis of "no difference" is true, the Type II error rate
#   cannot be computed by assuming the alternative hypothesis alone because the alternative hypothesis alone does not specify a particular value
#   for the difference. It thus does not nail down a specific distribution for the t-value under the alternative.
# 
#   For this reason, when we study the Type II error rate of a hypothesis testing procedure, we need to assume a particular effect size, or hypothetical
#   size of the difference between population means, that we wish to target. We ask questions such as "what is the smallest difference I could
#   reliably distinguish from 0 given my sample size [mathjaxinline]N[/mathjaxinline]?" or, more commonly, "How big does
#   [mathjaxinline]N[/mathjaxinline] have to be in order to detect that the absolute value of the difference is greater than zero?" Type II error control
#   plays a major role in designing data collection procedures before you actually see the data, so that you know the test you will run has enough
#   sensitivity or power. Power is one minus the Type II error rate, or the probability that you will reject the null hypothesis when the alternative
#   hypothesis is true.
# 
#   There are several aspects of a hypothesis test that affect its power for a particular effect size. Intuitively, setting a lower
#   [mathjaxinline]\alpha[/mathjaxinline] decreases the power of the test for a given effect size because the null hypothesis will be more difficult to
#   reject. This means that for an experiment with fixed parameters (i.e., with a predetermined sample size, recording mechanism, etc), the powerof
#   the hypothesis test trades off with its Type I error rate, no matter what effect size you target.
# 
#   We can explore the trade off of power and Type I error concretely using the babies data. Since we have the full population, we know what the
#   true effect size is (about 8.93) and we can compute the power of the test for true difference between populations.
# 
#   Set the seed at 1 and take a random sample of [mathjaxinline]N=5[/mathjaxinline] measurements from each of the smoking and nonsmoking
#   datasets. What is the p-value (use the t.test() function)?

N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value

0.1843195

#Week 3 > Inference I: P-values, Confidence Intervals and Power Calculations > Power Calculations Exercises

#Power Calculations Exercises

library(rafalib)
library(dplyr)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

#Power Calculations Exercises #1
#Q: We can explore the trade off of power and Type I error concretely using the babies data. Since we have the full population, we know what the
# true effect size is (about 8.93) and we can compute the power of the test for true difference between populations.
 
# Set the seed at 1 and take a random sample of N=5 measurements from each of the smoking and nonsmoking datasets. Use the t-test function
# to find the p-value. (Note that you already performed this calculation in the last assessment.)
 
# The p-value is larger than 0.05 so using the typical cut-off, we would not reject. This is a type II error. Which of the following is *not* a way to
# decrease this type of error?

#>> Find a population for which the null is not true.

#Power Calculations Exercises #2
#Q: Set the seed at 1, then use the replicate() function to repeat the code used in the exercise above 10,000 times. What proportion of the time
#   do we reject at the 0.05 level?

N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

0.096

#Power Calculations Exercises #3
#Q: Note that, not surprisingly, the power is lower than 10%. Repeat the exercise above for samples sizes of 30, 60, 90 and 120. Which of those four
#   gives you power of about 80%?

Ns=c(30,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.05
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 

60

#Power Calculations Exercises #4
#Q: Repeat the problem above, but now require an [mathjaxinline]\alpha[/mathjaxinline] level of 0.01. Which of those four gives you power of about
#   80%?

Ns=c(30,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 

90

#Week 3 > Inference II: Monte Carlo Simulation, Permutation Tests and Association tests > Monte Carlo Exercises

#Monte Carlo Exercises

#Monte Carlo Exercises #1
#Q: Imagine you are William Sealy Gosset and have just mathematically derived the distribution of the t-statistic when the sample comes from a
#   normal distribution. Unlike Gosset, you have access to computers and can use them to check the results.

#   Let's start by creating an outcome.

#   Set the seed at 1, then use rnorm() to generate a random sample of size 5,  X1,…,X5  from a standard normal distribution, then compute the
#   t-statistic  t=5–vX¯/s  with  s  the sample standard deviation. What value do you observe?

set.seed(1)
N <- 5
X <- rnorm(N)
sqrt(N)*mean(X)/sd(X)

0.3007746

#Monte Carlo Exercises #2
#Q: You have just performed a Monte Carlo simulation using rnorm() , a random number generator for normally distributed data. Gosset's
#   mathematical calculation tells us that the t-statistic defined in the previous exercises, a random variable, follows a t-distribution with  N-1
#   degrees of freedom. Monte Carlo simulations can be used to check the theory: we generate many outcomes and compare them to the
#   theoretical result. Set the seed to 1, then generate  B=1000  t-statistics as done in exercise 1. What proportion is larger than 2?

set.seed(1)
N <- 5
B<- 1000

tstats <- replicate(B,{
  X <- rnorm(N)
  sqrt(N)*mean(X)/sd(X)
})
mean(tstats>2)

0.068

#Monte Carlo Exercises #3
#Q: The answer to exercise 2 is very similar to the theoretical prediction: 1-pt(2,df=4). We can check several such quantiles using the qqplot
#   function.

#   To obtain quantiles for the t-distribution we can generate percentiles from just above 0 to just below 1:
#   B=100; ps = seq(1/(B+1), 1-1/(B+1),len=B), and compute the quantiles with qt(ps,df=4). Now we can use qqplot() to compare these
#   theoretical quantiles to those obtained in the Monte Carlo simulation. Use Monte Carlo simulation developed for exercise 2 to corroborate that
#   the t-statistic  t=N--vX¯/s  follows a t-distribution for several values of  N  (try Ns < seq(5,30,5)).
 
#   For which sample sizes does the approximation best work?

#>> The approximations are spot on for all sample sizes.

#Monte Carlo Exercises #4
#Q: Use Monte Carlo simulation to corroborate that the t-statistic comparing two means and obtained with normally distributed (mean 0 and sd)
#   data follows a t-distribution. In this case we will use the t.test() function with var.equal=TRUE. With this argument the degrees of freedom
#   will be df=2*N-2 with N the sample size. For which sample sizes does the approximation best work?

#>> The approximations are spot on for all sample sizes.

#Monte Carlo Exercises #5
#Q: Is the following statement true or false? If instead of generating the sample with X=rnorm(15) we generate it with binary data (either positive or
#   negative 1 with probability 0.5) X =sample(c(-1,1), 15, replace=TRUE) then the t-statistic
tstat <- sqrt(15)*mean(X) / sd(X)
#   is approximated by a t-distribution with 14 degrees of freedom.

#>> false

#Monte Carlo Exercises #6
#Q: Is the following statement true or false ? If instead of generating the sample with X=rnorm(N) with N=1000, we generate the data with binary
#   data X= sample(c(-1,1), N, replace=TRUE), then the t-statistic sqrt(N)*mean(X)/sd(X) is approximated by a t-distribution with 999 degrees of
#   freedom.

#>> true

#Monte Carlo Exercises #7
#Q: We can derive approximation of the distribution of the sample average or the t-statistic theoretically. However, suppose we are interested in the
#   distribution of a statistic for which a theoretical approximation is not immediately obvious.

#   Consider the sample median as an example. Use a Monte Carlo to determine which of the following best approximates the median of a sample
#   taken from normally distributed population with mean 0 and standard deviation 1.

#>> The sample median is approximately normal with mean 0 and SD larger than  1/N--v .

#Week 3 > Inference II: Monte Carlo Simulation, Permutation Tests and Association tests > Permutations Exercises

#Permutations Exercises

#Permutations Exercises #1
#Q: We will generate the following random variable based on a sample size of 10 and observe the following difference:
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
#   The question is whether this observed difference is statistically significant. We do not want to rely on the assumptions needed for the normal or
#   t-distribution approximations to hold, so instead we will use permutations. We will reshuffle the data and recompute the mean. We can create
#   one permuted sample with the following code:
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar)
#   The last value is one observation from the null distribution we will construct. Set the seed at 1, and then repeat the permutation 1,000 times to
#   create a null distribution. What is the permutation derived p-value for our observation?

set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )

0.05694306

#Permutations Exercises #2
#Q: Repeat the above exercise, but instead of the differences in mean, consider the differences in median obs <- median(smokers) -
#   median(nonsmokers). What is the permutation based p-value?

set.seed(1)
obs <- median(smokers) - median(nonsmokers)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 )
## As before we add 1 to avoid p-value of 0 but we also accept
( sum( abs(null) >= abs(obs)) ) / ( length(null) )

0.02897103

#Week 3 > Inference II: Monte Carlo Simulation, Permutation Tests and Association tests > Association Tests Exercises

#Association Tests Exercises

d = read.csv("assoctest.csv")

#Association Tests Exercises #1
#Q: Compute the Chi-square test for the association of genotype with case/control status (using the table() function and the chisq.test()
#   function). Examine the table to see if it looks enriched for association by eye.

#   What is the X-squared statistic?

tab = table(d$allele, d$case)
chisq.test(tab)

3.3437

#Association Tests Exercises #2
#Q: Compute the Fisher's exact test ( fisher.test() ) for the same table. What is the p-value?

tab = table(d$allele, d$case)
fisher.test(tab)

0.05194
#---------------------------------------------------------------------END--------------------------------------------------------------------------------#