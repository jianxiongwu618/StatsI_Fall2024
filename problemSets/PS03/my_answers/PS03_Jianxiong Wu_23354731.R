#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd("/Users/wuhadou/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/my_answers")

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

#Question 1
#1.1
q1_regression <- lm(voteshare ~ difflog, data = inc.sub)
summary(q1_regression)

#1.2
png(file = "q1_regression.png")
plot(inc.sub$difflog, inc.sub$voteshare, 
     xlab = "difference in campaign spending between incumbent and challenger",
     ylab = "the incumbent's vote share",
     main = "scatterplot of voteshare & difflog")
abline(q1_regression, col = "red")
dev.off()

#1.3
q1_residuals <- q1_regression$residuals
q1_residuals

#1.4
#voteshare = intercept + coefficient * difflog
#voteshare = 0.579031 + 0.041666 * difflog



#Question 2
#2.1
q2_regression <- lm(presvote ~ difflog, data = inc.sub)
summary(q2_regression)

#2.2
png(file = "q2_regression.png")
plot(inc.sub$difflog, inc.sub$presvote, 
     xlab = "difference between incumbent and challenger's spending",
     ylab = "the vote share of the presidential candidate of the incumbent's party",
     main = "scatterplot of presvote & difflog")
abline(q2_regression, col = "red")
dev.off()

#2.3
q2_residuals <- q2_regression$residuals
q2_residuals

#2.4
#presvote = intercept + coefficient * difflog
#presvote = 0.507583 + 0.023837 * difflog



#Question 3
#3.1
q3_regression <- lm(voteshare ~ presvote, data = inc.sub)
summary(q3_regression)

#3.2
png(file = "q3_regression.png")
plot(inc.sub$presvote, inc.sub$voteshare, 
     xlab = "the vote share of the presidential candidate of the incumbent's party",
     ylab = "the incumbents electoral success",
     main = "scatterplot of voteshare & presvote")
abline(q3_regression, col = "red")
dev.off()

#3.3
#voteshare = intercept + coefficient * presvote
#voteshare = 0.441330 + 0.388018 * presvote



#Question 4
#4.1
q4_regression <- lm(q1_residuals ~ q2_residuals, data = inc.sub)
summary(q4_regression)

#4.2
png(file = "q4_regression.png")
plot(q2_residuals, q1_residuals, 
     xlab = "residuals from Question 2",
     ylab = "residuals from Question 1",
     main = "scatterplot of q1_residuals & q2_residuals")
abline(q4_regression, col = "red")
dev.off()

#4.3
#q1_residuals = intercept + coefficient * q2_residuals
#q1_residuals = (-1.942e-18) + (2.569e-01) * q2_residuals



#Question 5
#5.1
q5_regression <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(q5_regression)

#5.2
#voteshare = intercept + coefficient1 * difflog + coefficient2 * presvote
#voteshare = 0.44864 + 0.03554 * difflog + 0.25688 * presvote

#5.3
#The coefficient of presvote in the q5_regression in Question 5 is 
#the same as the coefficient of q2_residuals in the q4_regression in 
#Question 4, which is 0.2569. The reason for this is that in the q5_regression, 
#difflog is used as an explanatory variable along with presvote. And in 
#q4_regression, the residuals of voteshare ~ difflog and presvote ~ difflog are 
#used for the regression. So there is an independent effect of presvote on 
#vote share after controlling for difflog.
