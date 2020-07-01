#Week 4 > Exploratory Data Analysis > Scatterplot Exercises

#Scatterplot Exercises

data(nym.2002, package="UsingR")

#Scatterplot Exercises #1
#Q: Use dplyr to create two new data frames: males and females, with the data for each gender. For males, what is the Pearson correlation
#   between age and time to finish?

head(nym.2002)
library(dplyr)
males <- filter(nym.2002, gender=="Male") 
females <- filter(nym.2002, gender=="Female") 

cor(males$age,males$time)

#Scatterplot Exercises #2
#Q: For females, what is the Pearson correlation between age and time to finish?

cor(females$age,females$time)

#Scatterplot Exercises #3
#Q: If we interpret these correlations without visualizing the data, we would conclude that the older we get, the slower we run marathons, regardless
#   of gender. Look at scatterplots and boxplots of times stratified by age groups (20-25, 25-30, etc..).

#   After examining the data, what is a more reasonable conclusion?

library(rafalib)
mypar(2,2)
plot(females$age, females$time)
plot(males$age, males$time)
group <- floor(females$age/5) * 5
boxplot(females$time~group)
group <- floor(males$age/5) * 5
boxplot(males$time~group)

#>> Finish times are constant up through around 50-60, then they get slower.

#Week 4 > Exploratory Data Analysis > Symmetry of Log Ratios Exercises

#Symmetry of Log Ratios Exercises

time = sort(nym.2002$time)

#Symmetry of Log Ratios Exercises #1
#Q: What is the fastest time divided by the median time?

min(time) / median(time)

#Symmetry of Log Ratios Exercises #2
#Q: What is the slowest time divided by the median time?

max(time) / median(time)

#Week 4 > Exploratory Data Analysis > Plots to Avoid Exercises

#Plots to Avoid Exercises

#Plots to Avoid Exercises #1
#Q: When is it appropriate to use pie charts or donut charts?

#>> Never.

#Plots to Avoid Exercises #2
#Q: The use of pseudo-3D plots in the literature mostly adds:

#>> Confusion.

#Week 4 > Robust Summaries > Median, MAD, and Spearman Correlation Exercises

#Median, MAD, and Spearman Correlation Exercises

data(ChickWeight)

head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time", direction="wide")

head(chick)

chick = na.omit(chick)

#Median, MAD, and Spearman Correlation Exercises #1
#Q: Focus on the chick weights on day 4 (check the column names of chick and note the numbers). How much does the average of chick weights at
#   day 4 increase if we add an outlier measurement of 3000 grams? Specifically, what is the average weight of the day 4 chicks, including the outlier
#   chick, divided by the average of the weight of the day 4 chicks without the outlier. Hint: use c() to add a number to a vector.

chickout<-c(chick$weight.4,3000) # adding the outlier value of 3000
mean(chickout)/mean(chick$weight.4)

2.062407

#Median, MAD, and Spearman Correlation Exercises #2
#Q: In exercise 1, we saw how sensitive the mean is to outliers. Now let's see what happens when we use the median instead of the mean. Compute
#   the same ratio, but now using median instead of mean. Specifically, what is the median weight of the day 4 chicks, including the outlier chick,
#   divided by the median of the weight of the day 4 chicks without the outlier.

median(c(chick$weight.4, 3000))/median(chick$weight.4)

1

#Median, MAD, and Spearman Correlation Exercises #3
#Q: Now try the same thing with the sample standard deviation (the sd() function in R). Add a chick with weight 3000 grams to the chick weights
#   from day 4. How much does the standard deviation change? What's the standard deviation with the outlier chick divided by the standard
#   deviation without the outlier chick?

sd(c(chick$weight.4, 3000))/sd(chick$weight.4)

#Median, MAD, and Spearman Correlation Exercises #4
#Q: Compare the result above to the median absolute deviation in R, which is calculated with the mad() function. Note that the MAD is unaffected
#   by the addition of a single outlier. The mad() function in R includes the scaling factor 1.4826, such that mad() and sd() are very similar for a
#   sample from a normal distribution.

#   What's the MAD with the outlier chick divided by the MAD without the outlier chick?

mad(c(chick$weight.4, 3000))/mad(chick$weight.4)

1

#Median, MAD, and Spearman Correlation Exercises #5
#Q: Our last question relates to how the Pearson correlation is affected by an outlier as compared to the Spearman correlation. The Pearson
#   correlation between x and y is given in R by cor(x,y). The Spearman correlation is given by cor(x,y,method="spearman").
# 
#   Plot the weights of chicks from day 4 and day 21. We can see that there is some general trend, with the lower weight chicks on day 4 having low
#   weight again on day 21, and likewise for the high weight chicks.
# 
#   Calculate the Pearson correlation of the weights of chicks from day 4 and day 21. Now calculate how much the Pearson correlation changes if we
#   add a chick that weighs 3000 on day 4 and 3000 on day 21. Again, divide the Pearson correlation with the outlier chick over the Pearson
#   correlation computed without the outliers.

plot(chick$weight.4,chick$weight.21)
cor(chick$weight.4,chick$weight.21)
plot(c(chick$weight.4,3000),c(chick$weight.21,3000))
cor(c(chick$weight.4,3000),c(chick$weight.21,3000))
cor(c(chick$weight.4,3000),c(chick$weight.21,3000))/cor(chick$weight.4,chick$weight.21)

2.370719

#Week 4 > Robust Summaries > Mann-Whitney-Wilcoxon Test Exercises

#Mann-Whitney-Wilcoxon Test Exercises

data(ChickWeight)

head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time", direction="wide")

head(chick)

chick = na.omit(chick)

#Mann-Whitney-Wilcoxon Test Exercises #1
#Q: Save the weights of the chicks on day 4 from diet 1 as a vector x. Save the weights of the chicks on day 4 from diet 4 as a vector y. Perform a
#   t-test comparing x and y (in R the function t.test(x,y) will perform the test). Then perform a Wilcoxon test of x and y (in R the function
#   wilcox.test(x,y) will perform the test). A warning will appear that an exact p-value cannot be calculated with ties, so an approximation is used,
#   which is fine for our purposes.

#   Perform a t-test of x and y, after adding a single chick of weight 200 grams to x (the diet 1 chicks). What is the p-value from this test? The p-
#   value of a test is available with the following code: t.test(x,y)$p.value

x = chick$weight.4[chick$Diet == 1]
y = chick$weight.4[chick$Diet == 4]
t.test(c(x, 200), y)$p.value

0.9380347

#Mann-Whitney-Wilcoxon Test Exercises #2
#Q: Do the same for the Wilcoxon test. The Wilcoxon test is robust to the outlier. In addition, it has less assumptions that the t-test on the
#   distribution of the underlying data.

x = chick$weight.4[chick$Diet == 1]
y = chick$weight.4[chick$Diet == 4]
wilcox.test(c(x, 200), y, exact=FALSE)$p.value

0.0009840921

#Mann-Whitney-Wilcoxon Test Exercises #3
#Q: We will now investigate a possible downside to the Wilcoxon-Mann-Whitney test statistic. Using the following code to make three boxplots,
#   showing the true Diet 1 vs 4 weights, and then two altered versions: one with an additional difference of 10 grams and one with an additional
#   difference of 100 grams. Use the x and y as defined above, NOT the ones with the added outlier.

library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

#   What is the difference in t-test statistic (obtained by t.test(x,y)$statistic) between adding 10 and adding 100 to all the values in the group
#   y? Take the the t-test statistic with x and y+10 and subtract the t-test statistic with x and y+100. The value should be positive.

t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

67.75097

#Mann-Whitney-Wilcoxon Test Exercises #4
#Q: Examine the Wilcoxon test statistic for x and y+10 and for x and y+100. Because the Wilcoxon works on ranks, once the two groups show
#   complete separation, that is all points from group y are above all points from group x, the statistic will not change, regardless of how large
#   the difference grows. Likewise, the p-value has a minimum value, regardless of how far apart the groups are. This means that the Wilcoxon test
#   can be considered less powerful than the t-test in certain contexts. In fact, for small sample sizes, the p-value can't be very small, even when the
#   difference is very large.

#   What is the p-value if we compare c(1,2,3) to c(4,5,6) using a Wilcoxon test?

wilcox.test(c(1,2,3),c(4,5,6))$p.value

0.1

#Mann-Whitney-Wilcoxon Test Exercises #5
#Q: What is the p-value if we compare c(1,2,3) to c(400,500,600) using a Wilcoxon test?

wilcox.test(c(1,2,3),c(400,500,600))$p.value

0.1
#-------------------------------------------------------------------END--------------------------------------------------------------------------------#