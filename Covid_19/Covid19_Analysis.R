# remove variable store 
rm(list=ls())

#import package Hmisc
library(Hmisc)

data <- read.csv("C:/Users/anise/OneDrive/Desktop/R_Project/Covid_19/Covid19.csv")


# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# claim: people who die are older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# check is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
#  if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) 
mean(women$death_dummy, na.rm = TRUE) 
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)




