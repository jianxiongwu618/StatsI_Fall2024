# Set Working Directory
setwd("/Users/wuhadou/Documents/GitHub/StatsI_Fall2024/problemSets/PS04/my_answers")

# remove objects
rm(list=ls())

install.packages("car")
library(car)
data(Prestige)
help(Prestige)

str(Prestige)

#Question 1
#(a)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
#Prestige$professional <- as.factor(Prestige$professional)
#table(Prestige$professional)
head(Prestige)

#(b)
Prestige_regression <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(Prestige_regression)
library(stargazer)
stargazer(Prestige_regression)

#(c)
#prestige = 21.1423 + 0.0032*income + 37.7812*professional - 0.0023*income*professional

#(d)
#The income coefficient for this regression model is 0.003, so when professional = 0, each unit 
#increase in income will increase the prestige of blue- and white-collar workers by 0.003 units.

#(e)
#The professional coefficient of this regression model is 37.781, so when income is zero, the prestige
#scores of blue- and white-collar workers are lower than the professionals' prestige score of 37.781 points.

#(f)
#prestige = 21.1423 + 0.0032*income + 37.7812*professional - 0.002*income*professional
#So when profession = 1
#prestige = 21.1423 + 0.0032*income + 37.7812 - 0.0023*income
#         = 58.9235 + 0.0009*income
#Income has a coefficient of 0.009, so when income increases by $1,000, the prestige score increases by 
#0.009 * 1000 = 0.9

#(g)
#prestige = 21.1423 + 0.0032*income + 37.7812*professional - 0.002*income*professional
#when profession = 1
#prestige = 21.1423 + 0.0032*6000 + 37.7812 - 0.0023*6000 = 64.3235
#when profession = 0
#prestige = 21.1423 + 0.0032*6000 = 40.3423
#64.3235 - 40.3423 = 23.9812
#At an income of $6,000, switching from non-professional to professional increases 
#the prestige score by about 23.98


#Question 2
#(a)
#H0:Lawn signs have no effect on vote share
#H1:Lawn signs have an effect on vote share
coef_a <- 0.042
se_a <- 0.016
ts_a <- coef_a/se_a
#ts_q2=2.625
p_value_a <- 2 * pt(abs(ts_a), 131-2-1, lower.tail = FALSE)
#p_value=0.0097
#Since the p-value of 0.0097 is less than 0.05, the null hypothesis is rejected, 
#so there is evidence that these yard signs in a precinct affects vote share.

#(b)
#H0:Being next to precincts with these yard signs does not affect vote share
#H1:Being next to precincts with these yard signs can affect vote share
coef_b <- 0.042
se_b <- 0.013
ts_b <- coef_b/se_b
#ts_q2=3.231
p_value_b <- 2 * pt(abs(ts_b), 131-2-1, lower.tail = FALSE)
#p_value=0.0015
#Since the p-value of 0.0015 is less than 0.05, the null hypothesis is rejected, 
#so there is evidence that being next to precincts with these yard signs can affect vote share.

#(c)
#The constant term has a coefficient of 0.302, which represents a projected Ken Cuccinell vote 
#of 30.2% in the absence of any yard signs and adjacent signs.

#(d)
#The value of R-squared in this question is 0.094, which means that only about 9.4% of Ken Cuccinell's 
#vote share is due to precinct assigned lawn signs and precinct adjacent to lawn signs, which is a 
#relatively low percentage, suggesting that lawn signs, although they have an impact on the vote share 
#though This relatively low percentage indicates that although lawn signs have some effect on vote share, 
#they are not the main factor affecting vote share and cannot fully explain the change in vote share; there 
#are still some other variables that affect vote share.