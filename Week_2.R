#Week 2 > Random Variables and Probability Distributions > Random Variables Exercises

#Random Variables Exercises

library(dplyr)
x<-read.csv("femaleControlsPopulation.csv")
x<-unlist(x)

#Random Variables Exercises #1
#Q: What is the average of these weights?

obs<-mean(x)
obs

23.89338

#Random Variables Exercises #2
#Q: Set the seed to 1:
#   set.seed(1)
#   Take a random sample of size 5. What is the absolute value (use abs()) of the difference between the average of the sample and the average of
#   all the values?

set.seed(1)
random <- sample(x,5)
abs( mean(random) - mean(x) )

0.3293778

#Random Variables Exercises #3
#Q: After setting the seed at 5, set.seed(5), take a random sample of size 5. What is the absolute value of the difference between the average of
#   the sample and the average of all the values?

set.seed(5)
randoms <- sample(x,5)
abs( mean(randoms) - mean(x) )

0.3813778

#Random Variables Exercises #4
#Q: Why are the answers from 2 and 3 different?

#>> Because the average of the samples is a random variable.

#Week 2 > Random Variables and Probability Distributions > Null Distributions Exercises

#Null Distributions Exercises

#Null Distributions Exercises #1
#Q: Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages.
#   What proportion of these 1,000 averages are more than 1 gram away from the average of x ?

set.seed(1) 
n<-1000
nulls<-vector("numeric",n)
for (i in 1:n) {
  control<-sample(x,5)
  nulls[i]<-mean(control)
}
mean( abs( nulls-mean(x) )>1)

0.503 

#Null Distributions Exercises #2
#Q: We are now going to increase the number of times we redo the sample from 1,000 to 10,000. Set the seed at 1, then using a for-loop take a
#   random sample of 5 mice 10,000 times. Save these averages.
#   What proportion of these 10,000 averages are more than 1 gram away from the average of x ?

set.seed(1) 
n<-10000
nulls1<-vector("numeric",n)
for (i in 1:n) {
  control1<-sample(x,5)
  nulls1[i]<-mean(control1)
}
mean( abs( nulls1-mean(x) )>1)

0.5084

#Null Distributions Exercises #3
#Q: Note that the answers to 1 and 2 barely changed. This is expected. The way we think about the random value distributions is as the distribution
#   of the list of values obtained if we repeated the experiment an infinite number of times. On a computer, we can't perform an infinite number of
#   iterations so instead, for our examples, we consider 1,000 to be large enough, thus 10,000 is as well. Now if instead we change the sample size,
#   then we change the random variable and thus its distribution.
    
#   Set the seed at 1, then using a for-loop take a random sample of 50 mice 1,000 times. Save these averages.

#   What proportion of these 1,000 averages are more than 1 gram away from the average of x ?

set.seed(1) 
n<-1000
nulls2<-vector("numeric",n)
for (i in 1:n) {
  control2<-sample(x,50)
  nulls2[i]<-mean(control2)
}
mean(abs(nulls2-mean(x))>1)

0.014

#Week 2 > Random Variables and Probability Distributions > Probability Distributions Exercises

#Probability Distributions Exercises

install.packages("gapminder")
library(gapminder)
data("gapminder")
head(gapminder)

#Probability Distributions Exercises #1
#Q: In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the function F(a) for any a, which
#   tells you the proportion of the values which are less than or equal to a.

#   We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are less than or equal
#   to a, divided by the total number of values in x, in other words the proportion of values less than or equal to a.

#   The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that
#   doesn't return a value, but a function.

#   Let's continue, using the simpler mean() function.

#   What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?

dat1952 = gapminder[ gapminder$year == 1952, ]
c = dat1952$lifeExp
mean(c <= 40)

0.2887324

#Probability Distributions Exercises #2
#Q: What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years?

data1952 = gapminder[ gapminder$year == 1952, ]
y = data1952$lifeExp
mean(y<=60)-mean(y<=40)

0.4647887

#Week 2 > Central Limit Theorem > Normal Distribution Exercises

#Normal Distribution Exercises

read.csv("femaleControlsPopulation.csv")
x<-unlist(read.csv("femaleControlsPopulation.csv"))

set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}

hist(averages5)
hist(averages50)

#Normal Distribution Exercises #1
#Q: Use a histogram to "look" at the distribution of averages we get with a sample size of 5 and a sample size of 50. How would you say they differ?

#>> They both look roughly normal, but with a sample size of 50 the spread is smaller.

#Normal Distribution Exercises #2
#Q: For the last set of averages, the ones obtained from a sample size of 50, what proportion are between 23 and 25?

mean( averages50 < 25 & averages50 > 23)

0.982

#Normal Distribution Exercises #3
#Q: Note that you can use the function pnorm() to find the proportion of observations below a cutoff x given a normal distribution with mean
#   mu and standard deviation sigma with pnorm(x, mu, sigma) or pnorm( (x-mu)/sigma ).

#   What is the proportion of observations between 23 and 25 in a normal distribution with average 23.9 and standard deviation 0.43?

pnorm(25,23.9,0.43)-pnorm(23,23.9,0.43)

0.9765648

#Week 2 > Central Limit Theorem > Population, Samples, and Estimates Exercises

#Population, Samples, and Estimates Exercises

library(dplyr)
dat<-read.csv("mice_pheno.csv")
dat<-na.omit(dat)
dat

#Population, Samples, and Estimates Exercises #1
#Q: Use dplyr to create a vector x with the body weight of all males on the control (chow) diet.
#   What is this population's average?

x<-filter(dat,Sex=="M",Diet=="chow")
y<-c(x$Bodyweight)
unlist(y)
mean(y)

30.96381

#Population, Samples, and Estimates Exercises #2
#Q: Now use the rafalib package and use the popsd() function to compute the population standard deviation.

library(rafalib)
popsd(y)

4.420501

#Population, Samples, and Estimates Exercises #3
#Q: Set the seed at 1. Take a random sample  X  of size 25 from x.
#   What is the sample average?

set.seed(1)
X<-sample(y,25)
mean(X)

32.0956

#Population, Samples, and Estimates Exercises #4
#Q: Use dplyr to create a vector y with the body weight of all males on the high fat hf) diet.
#   What is this population's average?

a<-filter(dat,Sex=="M",Diet=="hf")
b<-c(a$Bodyweight)
unlist(b)
mean(b)

34.84793

#Population, Samples, and Estimates Exercises #5
#Q: Now use the rafalib package and use the popsd() function to compute the population standard deviation.

popsd(b)

5.574609

#Population, Samples, and Estimates Exercises #6
#Q: Set the seed at 1. Take a random sample  Y  of size 25 from y.
#   What is the sample average?

set.seed(1)
Y<-sample(b,25)
mean(Y)

35.8036

#Population, Samples, and Estimates Exercises #7
#Q: What is the difference in absolute value between  y¯-x¯  and  Y¯-X¯ ?

p<-mean(Y)-mean(X)
q<-mean(b)-mean(y)
abs(p-q)

1.399884

#Population, Samples, and Estimates Exercises #8
#Q: Repeat the above for females, this time setting the seed to 2.
#   What is the difference in absolute value between  y¯-x¯  and  Y¯-X¯ ?

0.3647172

#Population, Samples, and Estimates Exercises #9
#Q: For the females, our sample estimates were closer to the population difference than with males. What is a possible explanation for this?

#>> The population variance of the females is smaller than that of the males; thus, the sample variable has less variability.

#Week 2 > Central Limit Theorem > Central Limit Theorem Exercises

#Central Limit Theorem Exercises

library(dplyr)
library(rafalib)
dat<-read.csv("mice_pheno.csv")
dat<-na.omit(dat)

#Central Limit Theorem Exercises #1
#Q: If a list of numbers has a distribution that is well approximated by the normal distribution, what proportion of these numbers are within one
#   standard deviation away from the list's average?

pnorm(1)-pnorm(-1)

0.6826895

#Central Limit Theorem Exercises #2
#Q: What proportion of these numbers are within two standard deviations away from the list's average?

pnorm(2)-pnorm(-2)

0.9544997

#Central Limit Theorem Exercises #3
#Q: What proportion of these numbers are within three standard deviations away from the list's average?

pnorm(3)-pnorm(-3)

0.9973002

#Central Limit Theorem Exercises #4
#Q: Define y to be the weights of males on the control diet.
#   What proportion of the mice are within one standard deviation away from the average weight?

y<-filter(dat, Sex=="M")
Y<-c(y$Bodyweight)
unlist(Y)
mean(Y)
z<-(Y-mean(Y))/popsd(Y)
mean(abs(z)<=1)

0.6850962

#Central Limit Theorem Exercises #5
#Q: What proportion of these numbers are within two standard deviations away from the list's average?

mean( abs(z)<=2)

0.9461883

#Central Limit Theorem Exercises #6
#Q: What proportion of these numbers are within three standard deviations away from the list's average?

mean( abs(z)<=3)

0.9927885

#Central Limit Theorem Exercises #7
#Q: Note that the numbers for the normal distribution and our weights are relatively close. Also, notice that we are indirectly comparing quantiles of
#   the normal distribution to quantiles of the mouse weight distribution. We can actually compare all quantiles using a qqplot.
#   Which of the following best describes the qq-plot comparing mouse weights to the normal distribution?

qqnorm(z)
abline(0,1)

#>> The mouse weights are well approximated by the normal distribution, although the larger values (right tail) are larger than predicted by
#   the normal. This is consistent with the differences seen between question 3 and 6.

#Central Limit Theorem Exercises #8
#Q: Create the above qq-plot for the four populations: male/females on each of the two diets. What is the best explanation for all these mouse
#   weights being well approximated by the normal distribution?

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

#>> This just happens to be how nature behaves in this particular case. Perhaps the result of many biological factors averaging out.

#Central Limit Theorem Exercises #9
#Q: Here we are going to use the function replicate() to learn about the distribution of random variables. All the above exercises relate to the
#   normal distribution as an approximation of the distribution of a fixed list of numbers or a population. We have not yet discussed probability in
#   these exercises. If the distribution of a list of numbers is approximately normal, then if we pick a number at random from this distribution, it will
#   follow a normal distribution. However, it is important to remember that stating that some quantity has a distribution does not necessarily imply
#   this quantity is random. Also, keep in mind that this is not related to the central limit theorem. The central limit applies to averages of random
#   variables. Let's explore this concept.

#   We will now take a sample of size 25 from the population of males on the chow diet. The average of this sample is our random variable. We will
#   use the replicate() function to observe 10,000 realizations of this random variable. Set the seed at 1, then generate these 10,000 averages.
#   Make a histogram and qq-plot of these 10,000 numbers against the normal distribution.

#   We can see that, as predicted by the CLT, the distribution of the random variable is very well approximated by the normal distribution.

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

# What is the average of the distribution of the sample average?

mean(avgs)

30.96856

#Central Limit Theorem Exercises #10
#Q: What is the standard deviation of the distribution of sample averages (use popsd())?

popsd(avgs)

0.827082

#Week 2 > Central Limit Theorem > CLT and t-distribution in Practice Exercises

#CLT and t-distribution in Practice Exercises

library(rafalib)
mypar()
library(dplyr)
dat<-read.csv("femaleMiceWeights.csv")

#CLT and t-distribution in Practice Exercises #1
#Q: The CLT is a result from probability theory. Much of probability theory was originally inspired by gambling. This theory is still used in practice by
#   casinos. For example, they can estimate how many people need to play slots for there to be a 99.9999% probability of earning enough money to
#   cover expenses. Let's try a simple example related to gambling.

#   Suppose we are interested in the proportion of times we see a 6 when rolling n=100 dice. This is a random variable which we can simulate with
#   x=sample(1:6, n, replace=TRUE) and the proportion we are interested in can be expressed as an average: mean(x==6). Because the die rolls
#   are independent, the CLT applies.

#   We want to roll n dice 10,000 times and keep these proportions. This random variable (proportion of 6s) has mean p=1/6 and variance
#   p*(1-p)/n. So according to the CLT, z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1.

#   Set the seed to 1, then use replicate() to perform the simulation, and report what proportion of times z was larger than 2 in absolute value
#   (CLT says it should be about 0.05).

set.seed(1)
n <- 100
dice <- 6
p <- 1/dice
zs <- replicate(10000,{
  x <- sample(1:dice,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)

0.0431

#CLT and t-distribution in Practice Exercises #2
#Q: For the last simulation you can make a qqplot to confirm the normal approximation. Now, the CLT is an asymptotic result, meaning it is closer and
#   closer to being a perfect approximation as the sample size increases. In practice, however, we need to decide if it is appropriate for actual sample
#   sizes. Is 10 enough? 15? 30?

#   In the example used in exercise 1, the original data is binary (either 6 or not). In this case, the success probability also affects the appropriateness
#   of the CLT. With very low probabilities, we need larger sample sizes for the CLT to "kick in".

#   Run the simulation from exercise 1, but for different values of p and n. For which of the following is the normal approximation best?
  
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}  

#>. p=0.5 and n=30  

#CLT and t-distribution in Practice Exercises #3
#Q: As we have already seen, the CLT also applies to averages of quantitative data. A major difference with binary data, for which we know the
#   variance is  p(1-p) , is that with quantitative data we need to estimate the population standard deviation.

#   In several previous exercises we have illustrated statistical concepts with the unrealistic situation of having access to the entire population.In
#   practice, we do *not* have access to entire populations. Instead, we obtain one random sample and need to reach conclusions analyzing that
#   data. dat is an example of a typical simple dataset representing just one sample. We have 12 measurements for each of two populations:

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

#   We think of  X  as a random sample from the population of all mice in the control diet and  Y  as a random sample from the population of all mice
#   in the high fat diet.

#   Define the parameter  µX  as the average of the control population. We estimate this parameter with the sample average  X¯ . What is the sample
#   average?

mean(X)

23.81333

#CLT and t-distribution in Practice Exercises #4
#Q: We don't know  µX  , but want to use  X¯  to understand  µX . Which of the following uses CLT to understand how well  X¯  approximates  µX  ?

#>> X¯  follows a normal distribution with mean  µX  and standard deviation  sX12v  where  sX  is the population standard deviation.

#CLT and t-distribution in Practice Exercises #5
#Q: The result above tells us the distribution of the following random variable:  Z=12--vX¯-µXsX . What does the CLT tell us is the mean of  Z  (you
#   don't need code)?

0

#CLT and t-distribution in Practice Exercises #6
#Q: The result of 4 and 5 tell us that we know the distribution of the difference between our estimate and what we want to estimate, but don't know.
#   However, the equation involves the population standard deviation  sX , which we don't know.

#   Given what we discussed, what is your estimate of  sX ?

sd(X)

3.022541

#CLT and t-distribution in Practice Exercises #7
#Q: Use the CLT to approximate the probability that our estimate  X¯  is off by more than 2 grams from  µX.

2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )

0.02189533

#CLT and t-distribution in Practice Exercises #8
#Q: Now we introduce the concept of a null hypothesis. We don't know  µX  nor  µY . We want to quantify what the data say about the possibility that
#   the diet has no effect:  µX=µY . If we use CLT, then we approximate the distribution of  X¯  as normal with mean  µX  and standard deviation
#   sX/M--v  and the distribution of  Y¯  as normal with mean  µY  and standard deviation  sY/N--v , with  M and N  the sample sizes for  X and Y
#   respectively, in this case 12. This implies that the difference  Y¯-X¯  has mean  0 . We described that the standard deviation of this statistic (the
#   standard error) is  SE(X¯-Y¯)=s2Y/12+s2X/12------------v  and that we estimate the population standard deviations  sX  and  sY  with the sample
#   estimates.

#   What is the estimate of  SE(X¯-Y¯)=s2Y/12+s2X/12------------v  ?

sqrt( var(X)/12 + var(Y)/12)

1.469867

#CLT and t-distribution in Practice Exercises #9
#Q: So now we can compute  Y¯-X¯  as well as an estimate of this standard error and construct a t-statistic. What number is this t-statistic?

t.test(Y,X)$stat

2.055174

#CLT and t-distribution in Practice Exercises #10
#Q: If we apply the CLT, what is the distribution of this t-statistic?

#>> Normal with mean 0 and standard deviation 1.

#CLT and t-distribution in Practice Exercises #11
#Q: Now we are ready to compute a p-value using the CLT. What is the probability of observing a quantity as large as what we computed in 9, when
#   the null distribution is true?

Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
2*( 1-pnorm(Z)) 

0.0398622

#CLT and t-distribution in Practice Exercises #12
#Q: CLT provides an approximation for cases in which the sample size is large. In practice, we can't check the assumption because we only get to see
#   1 outcome (which you computed above). As a result, if this approximation is off, so is our p-value. As described earlier, there is another approach
#   that does not require a large sample size, but rather that the distribution of the population is approximately normal. We don't get to see this
#   distribution so it is again an assumption, although we can look at the distribution of the sample with qqnorm(X) and qqnorm(Y). If we are
#   willing to assume this, then it follows that the t-statistic follows the t-distribution.

#   What is the p-value under the t-distribution approximation?

t.test(X,Y)$p.value

0.05299888

#CLT and t-distribution in Practice Exercises #13
#Q: With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution, one that is larger. They can't both be right. What
#   best describes the difference?

#>> These are two different assumptions. The t-distribution accounts for the variability introduced by the estimation of the standard error and
#   thus, under the null, large values are more probable under the null distribution.

#--------------------------------------------------------------------------------END-------------------------------------------------------------------------#





















