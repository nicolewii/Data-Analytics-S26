library(readr)
library(EnvStats)
library(nortest)

## Exercise 2 ##
# Keeping importing from sample code given
setwd("/Users/nicolelee/Documents/Courses/Data Analytics/Labs/Lab 1")
# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")
# view dataframe to verify dataset
View(epi.data)

# Variable summaries & box plots #

# Copying ECO data
ECO <- epi.data$ECO.new
ECO
# Find N/As and print rows
NAs <- is.na(ECO)
NAs
rownums <- which(NAs)
# print rows with NAs
ECO[rownums]
ECO.complete <- ECO[!NAs]
ECO.complete

# Copying BDH data
BDH <- epi.data$BDH.new
BDH

#Finding N/As
NAs <- is.na(BDH)
rownums <- which(NAs)
BDH[rownums]

#BDH
BDH.complete <- BDH[!NAs]
BDH.complete
BDH.below30 <- BDH.complete[BDH.complete<30]
BDH.below30

# stats
summary(BDH.below30)

# boxplot of variable(s)
boxplot(ECO.complete, BDH.below30, names = c("ECO.complete","BDH<30"))

# Histograms
hist(ECO.complete)
#Edited the sequence to fir the histogram range
x <- seq(15, 95, 10)
hist(ECO.complete, x, prob=TRUE)
lines(density(ECO.complete,bw="SJ"))
rug(ECO.complete)

#Running probabilities
hist(ECO.complete, x, prob=TRUE) 
x1<-seq(15,85,2)

#Using the real mean and sd values, adding +1 for formatting
d1 <- dnorm(x1, mean=mean(ECO.complete)+1, sd=sd(ECO.complete)+1,log=FALSE)
lines(x1,d1)

#ECDF Plots
plot(ecdf(ECO.complete), do.points=FALSE, verticals=TRUE) 
plot(ecdf(BDH.complete), do.points=FALSE, verticals=TRUE) 

#QQ Plots

qqnorm(ECO.complete); qqline(ECO.complete)
x <- rnorm(180, mean=50, sd=10)

qqnorm(x); qqline(x)


# print quantile-quantile plot of two variables
qqplot(ECO.complete, BDH.complete, xlab = "Q-Q plot for ECO.complete & BDH.complete") 
# print quantile-quantile plot for 2 variables
qqplot(epi.data$BDH.new, epi.data$BDH.old, xlab = "BDH.new", ylab = "BDH.old",  main = "Q-Q Plot for BDH.new & BDH.old")

## Statistical Tests

#Shapiro Tests
shapiro.test(ECO.complete)
shapiro.test(BDH.complete)

#Anderson-Darling test
ad.test(ECO.complete)
ad.test(BDH.complete)

#Identical distribution tests
ks.test(ECO.complete, BDH.complete)
wilcox.test(ECO.complete, BDH.complete)
#F & T Test
var.test(ECO.complete, BDH.complete)
t.test(ECO.complete, BDH.complete)

#Print overlapping histogram of ECO and BDH
hist(BDH.complete, col='mediumpurple1')
hist(ECO.complete, col='steelblue1', add=TRUE)







