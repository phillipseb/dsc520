# Assignment: 3.2 Exercise
# Name: Phillips, Emily
# Date: 2010-06-26

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/holmk/OneDrive/Desktop/DSC520 - Statistics/classRepo/dsc520/")

## Load the `data/r4ds/heights.csv` to
community_df <- read.csv("http://content.bellevue.edu/cst/dsc/520/id/resources/acs-14-1yr-s0201.csv")

#Fetching column names of the dataframe
names(community_df)

#Categories
#Geography -- states in the United States
unique(community_df[c("Geography")])
#POPGROUP.display.label -- only "Total population"
unique(community_df[c("POPGROUP.display.label")])

#What are the elements in your data?
#Id -- chr (character)
#Geography -- chr (character)
#POPGROUP.display.label -- chr (character)
#HSDegree -- numeric
#Id2 -- integer
#PopGroupID -- integer
#RacesReported -- integer
#BachDegree -- numeric

#Please provide the output from the following functions: str(); nrow(); ncol()
#Categories and data types of the elements in the dataset
str(community_df)
#Number of rows
nrow(community_df)
#Number of columns
ncol(community_df)

#Create a Histogram of the HSDegree variable using the ggplot2 package.
library(ggplot2)
#number of bins = 11 -- square root of 136 and round down
#Each row of data represents a different U.S. city
head(community_df)
#ggplot(community_df, aes(x=HSDegree)) + geom_histogram(bins = 11) + ggtitle('Histogram of HSDegree') + xlab('Percentage of Total City Population with High School Degrees') + ylab('Frequency')

# Based on what you see in this histogram, is the data distribution unimodal?
# Is it approximately symmetrical?
# Is it approximately bell-shaped?
# Is it approximately normal?
# If not normal, is the distribution skewed? If so, in which direction?
# Include a normal curve to the Histogram that you plotted.
# Explain whether a normal distribution can accurately be used as a model for this data.

#Summary statistics for the different column names
summary(community_df)

#Looking at the sample data for HSDegree with the summary() function, it already seems like most of the data
#points lie at the higher end of the percentage spectrum (closer to 100%) than the rest of the data.
#The mean value is 87.63, and the IQR goes from 85.5 - 90.75, which is where the majority of the values lie between.
#Given that this range is quite small, I would say that the normal distribution cannot be accurately used as a model
#for this data, since the sample data is already not normally distrubted, and is already showed skewness.

#The data distribution, according to the histogram, is unimodal. There is really only one peak
#or mode in the distribution of the HSDegree variable. 
#However, the distribution is not approximately symmetrical, bell-shaped or normal. 
#It is negatively skewed (to the left).
x <- community_df$HSDegree
#Histogram oof HSDegree variable
hist.HSDegree <- ggplot(community_df,aes(HSDegree)) + geom_histogram(aes(y=..density..),bins=11,colour="black",fill="white") + labs(x="Percentage of Total City Population with HSDegrees",y="Density")
hist.HSDegree

#overlaying normal curve
hist.HSDegree + stat_function(fun=dnorm,args=list(mean=mean(x),sd=sd(x)),colour="black",size=1)

#Create a Probability Plot of the HSDegree variable.
#ggplot(community_df, aes(sample=HSDegree))+stat_qq()
#Construction a normal probability plot
#Can evaluate how close the points on the probability plot fall to the normal line
# qqnorm(community_df$HSDegree)
qqplot.HSDegree <- qplot(sample=community_df$HSDegree,stat='qq')
qqplot.HSDegree

# Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
# If not normal, is the distribution skewed? If so, in which direction? Explain how you know.

#For analyzing the probability plot for the data distribution of HSDegree, I notice that most of the data points
#do not follow along the normal line. It has a general curve to it and a majority of the points outside of majority range
#fall away from the line and move away from the normality of the distribution. 

#The distribution is skewed to the left, as both ends of the normality plot bend below the hypothetical normal line that was drawn on the plot.

#stat.desc() function
#install.packages("pastecs")
library(pastecs)
stat.desc(community_df$HSDegree,norm=TRUE,p=0.95)

# In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. 
# In addition, explain how a change in the sample size may change your explanation?

#The negative skewness value of the HSDegree variable indicates that the data distribution is skewed to the left. With
#a value of -1.67, we can see that the skew is pretty strongly directed to the left for this series.
#A normal distribution has a kurtosis of exactly 3. The kurtosis for this data distribution is equal to 4.35,
#which being greater than 3, shows that it is platykurtic, which shows that the tails are shorter and thinner
#and the peak is lower and broader as we did see with the histogram. 
#The normtest.p value is 3.19364e-09, which is less than p = 0.05 for a 95% confidence interval.
#This demonstrates that the distribution of data is significantly from the normal distribution, so we can assume
#that the data distribution of HSDegree is not normal. This comes from the Shapiro-Wilk test of normality.

#The sample size of 136 was relatively small for a population representation of the United States, especially
#when it comes from Census Bureaue data. If the sample size was different, it may have changed my explanation
#as the distribution would be more representative of the country and give a better idea of the distribution. 