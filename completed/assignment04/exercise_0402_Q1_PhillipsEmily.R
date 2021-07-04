# Assignment: 4.2 Exercise Question 1
# Name: Phillips, Emily
# Date: 2010-07-02

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/phill/Desktop/DSC520/dsc520/")

## Load the `Scores.csv dataset
scores_df <- read.csv("http://content.bellevue.edu/cst/dsc/520/id/resources/scores.csv")

#Observationa units in this study
#The observational units in this study are the students' course grades and total points earned in the course.

#Variables mentioned in the narrative paragraph
#Course sections -- sports, other (categorical)
#Students' course grades (quantiative)
#Students' total points in the courses (quantitative)
str(scores_df)
summary(scores_df)
unique(scores_df[c("Section")])

#Regular section subset
regularSec_df <- subset(scores_df,scores_df$Section=="Regular")
#Sports section subset
sportSec_df <- subset(scores_df,scores_df$Section=="Sports")

#Plotting each sections' scores and the number of students achieving that score
#install.packages("ggplot2")
library(ggplot2)
regular.graph <- ggplot(regularSec_df,aes(factor(Score),Count))
regular.graph + geom_bar(stat="identity",fill="steelblue") + theme_minimal() + ggtitle("Plot of Count vs. Score for the Regular Section") + xlab("Total Course Score") + ylab("Count of Students")

sports.graph <- ggplot(sportSec_df,aes(factor(Score),Count))
sports.graph + geom_bar(stat="identity",fill="steelblue") + theme_minimal() + ggtitle("Plot of Count vs. Score for the Sports Section") + xlab("Total Course Score") + ylab("Count of Students")

# Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: Can you say that one section tended to score more points than the other? Justify and explain your answer.
#Both point distributions, regular and sports, are bimodial with two modes of student count values. The peaks for the regular section are at 40 & 30, and the peaks for the sports section are both at 30.
#The regular course section scores start up higher though in the range (265-380), whereas the sports section ranges from 200 - 395. 
#Also, since the regular section distribution is more positively-skewed than the sports point distribution, I would say that it could implied that the regular section
#tends to score more points than the other, since it has a higher range of overall scores and most of its count values are found at the higher end of the distribution.

# Did every student in one section score more points than every student in the other section? If not, explain what a statistical tendency means in this context.
#No, every student in one section did not score more points than every student in the other section. While the students in the regular section tended to score higher than those in the sports section,
#it cannot be concluded that every student in one section scored more points than every student in the other section because there is overlap in the point distributions for both sections.
#Since there is overlap between the two point distributions, a statistical tendency would allow us to find that central location of the two overlapping sections; it would give us a better idea about how 
#they come together in the overall picture of the students' scores in the course. 

# What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?
#It was mentioned in the narrative that "the sports themed section was advertised as such; so students knew which type of section they were enrolling in". 
#Usually, people will enroll in a class that they are interested in or have more prior knowledge about, or even that they think will be easier, which can influence the point distribution between two sections.
