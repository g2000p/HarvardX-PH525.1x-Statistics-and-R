#Week 1 > Getting Started > Getting Started with R

#First assessment: Exercises

#Exercise #1
#Q: What version of R are you using?

version

#>> 4.0.0.

#Exercise #2
#Q: Create a numeric vector containing the numbers 2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23.
#   What is the average of these numbers?

x <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
mean(x)

#>> 7.794286
   
#Exercise #3
#Q: Use a for loop to determine the value of [mathjaxinline]\sum_{i=1}^{25} i^2[/mathjaxinline].

sum <- 0
  for(i in 1:25)
    sum <- sum + i^2
  sum
  
#>> 5525
   
#Exercise #4
#Q: The cars dataset is available in base R. You can type cars to see it. Use the class() function to determine what type of object is cars.
  
class(cars)

#>> data.frame
   
#Exercise #5
#Q: How many rows does the cars object have?

nrow(cars)

#>> 50
   
#Exercise #6
#Q: What is the name of the second column of cars?

names(cars)[2]

#>> dist

#Exercise #7
#Q: The simplest way to extract the columns of a matrix or data.frame is using [. For example you can access the second column with cars[,2].

mean(cars[,2])

#>> 42.98

#Exercise #8
#Q: Familiarize yourself with the which() function. Which row of cars has a a distance of 85?

which(cars[,2]==85)

#>> 50

#Week 1 > Getting Started > Getting Started Exercises

#Getting Started Exercises

#Getting Started Exercises #1
#Q: Read in the file femaleMiceWeights.csv and report the exact name of the column containing the weights.

dat<-read.csv("femaleMiceWeights.csv")
dat

names(dat)[2]

#>> Bodyweight

#Getting Started Exercises #2
#Q: The [ and ] symbols can be used to extract specific rows and specific columns of the table.
#   What is the entry in the 12th row and second column?

dat[12,2]

#>> 26.25

#Getting Started Exercises #3
#Q: You should have learned how to use the $ character to extract a column from a table and return it as a vector. Use $ to extract the weight 
#   column and report the weight of the mouse in the 11th row.

weights <- dat$Bodyweight
weights[11]

#>> 26.91

#Getting Started Exercises #4
#Q: The length() function returns the number of elements in a vector.
#   How many mice are included in our dataset?

weights <- dat$Bodyweight
length(weights)

#>> 24

#Getting Started Exercises #5
#Q: To create a vector with the numbers 3 to 7, we can use seq(3,7) or, because they are consecutive, 3:7. View the data and determine what 
#   rows are associated with the high fat or hf diet. Then use the mean() function to compute the average weight of these mice.
#   What is the average weight of mice on the high fat diet?

View(dat) 
weights <- dat$Bodyweight
mean(weights[13:24])

#>> 26.83417

#Getting Started Exercises #6
#Q: One of the functions we will be using often is sample(). Read the help file for sample() using ?sample. Now take a random sample of size 1 
#   from the numbers 13 to 24 and report back the weight of the mouse represented by that row. Make sure to type set.seed(1) to ensure that
#   everybody gets the same answer.

set.seed(1)
i <- sample(13:24, 1)
dat$Bodyweight[i]

#>> 34.02

# Week 1 > Getting Started > dplyr Exercises

#dplyr Exercises

#dplyr Exercises #1
#Q: Read in the msleep_ggplot2.csv file with the function read.csv() and use the function class() to determine what type of object is returned. 

library(dplyr)
dat<-read.csv("msleep_ggplot2.csv")
View(dat)
class(dat)

#>> data.frame

#dplyr Exercises #2
#Q: Now use the filter() function to select only the primates.
#   How many animals in the table are primates?
#   Hint: the nrow() function gives you the number of rows of a data frame or matrix.

prim<-filter(dat,order=="Primates")
prim
nrow(prim)

#>> 12

#dplyr Exercises #3
#Q: What is the class of the object you obtain after subsetting the table to only include primates?

class(prim)

#>> data.frame

#dplyr Exercises #4
#Q: Now use the select() function to extract the sleep (total) for the primates.
#   What class is this object?
#   Hint: use %>% to pipe the results of the filter() function to select().

prima<-filter(dat,order=="Primates") %>% select(sleep_total)
prima
class(prima)

#>> data.frame

#dplyr Exercises #5
#Q: Now we want to calculate the average amount of sleep for primates (the average of the numbers computed above). One challenge is that the
#   mean() function requires a vector so, if we simply apply it to the output above, we get an error. Look at the help file for unlist() and use it to
#   compute the desired average.
#   What is the average amount of sleep for primates?

primat<-filter(dat,order=="Primates") %>% select(sleep_total) %>% unlist
mean(primat)

#>> 10.5

#dplyr Exercises #6
#Q: For the last exercise, we could also use the dplyr summarize() function. We have not introduced this function, but you can read the help file
#   and repeat exercise 5, this time using just filter() and summarize() to get the answer.
#   What is the average amount of sleep for primates calculated by summarize() 

filter(dat, order=="Primates") %>% summarize( mean( sleep_total) )

#>> 10.5

#Week 1 > Introduction to Exploratory Data Analysis > Histogram Exercises

#Histogram Exercises

#Histogram Exercises #1
#Q: Example Histogram: 2 at 20, 3 at 30, 2 at 35, 4 at 45, 6 at 50, etc.
#   Given the above histogram, how many people are between the ages of 35 and 45?

library(UsingR)
sum(age>=35 & age<45)
# OR
# We can directly fing by looking at the histogram.

#>> 6

#Week 1 > Introduction to Exploratory Data Analysis > QQ-plot Exercises

#QQ-plot Exercises

load("C:/Users/GS/Downloads/skew.RData")
dim(dat)
dat <- scale(dat)
par(mfcol=c(3,3))
for (i in 1:9) {
  x <- dat[,i]
  qqnorm(x,  main=paste0("Q-Q plot for column V.",i,sep=""))
  qqline(x)
}

#QQ-plot Exercises #1
#Q: Which column has positive skew (a long tail to the right)?

for (i in 1:9) {
  qqnorm(dat[,i])
  qqline(dat[,i])
}

hist(dat[,4])

#>> 4

#QQ-plot Exercises #2
#Q: Which column has negative skew (a long tail to the left)?

hist(dat[,9])

#>> 9
#Confirm with histogram.

#Week 1 > Introduction to Exploratory Data Analysis > Boxplot Exercises

#Boxplot Exercises

#Boxplot Exercises #1

library(dplyr)
#Q: Which spray seems the most effective (has the lowest median count)?
boxplot( split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count ~ InsectSprays$spray)

#>> C

#Boxplot Exercises #2
#Q: Let's consider a random sample of finishers from the New York City Marathon in 2002. This dataset can be found in the UsingR package. Load the
#   library and then load the nym.2002 dataset.

library(dplyr)
data(nym.2002, package="UsingR")

#Use boxplots and histograms to compare the finishing times of males and females. Which of the following best describes the difference?
library(rafalib)
mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))

#>> Male and females have similar right skewed distributions with the former, 20 minutes shifted to the left.

#--------------------------------------------------------------------END-------------------------------------------------------------------------#