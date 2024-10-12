#Problem Set 2
#Question 1: Political Science
#(a)Calculate the χ2 test statistic
#fobserved = fo = observed frequency = the raw count
#fexpected = fe = what we would expect for independent samples
# = Row total / Grand total × Column total
rm(list=ls())
resulting_data_table <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
resulting_data_table
row_total <- rowSums(resulting_data_table)
col_total <- colSums(resulting_data_table)
grand_total <- sum(resulting_data_table)

row_total
col_total
grand_total

fe <- (row_total/grand_total) %*% t(col_total)
fe

chi_square <- sum((resulting_data_table - fe)^2/fe)
chi_square


#(b)Calculate the p-value from the test statistic
#df = (rows − 1)(columns − 1)
p_values <- pchisq(chi_square, df = (2-1)*(3-1), lower.tail = FALSE)
p_values
#The p-value is 0.1502306, greater than 0.1, so there is not enough evidence to reject the null hypothesis, 
#and we cannot conclude from these data that officers were more or less likely to solicit a bribe from drivers depending on their class.


#(c)Calculate the standardized residuals for each cel
#z = fobserved − fexpected / se = fobserve − fexpected/√fe(1 − row prop.)(1 − column prop.)
standardized_residuals <- ((resulting_data_table - fe) / 
                             (sqrt(fe * (1-(row_total/grand_total)) %*% (1-t(col_total/grand_total)))))
head(standardized_residuals)


#(d)How might the standardized residuals help you interpret the results?
#Since the absolute value of the standardised residuals in each cell does not exceed 2, 
#indicating that the observed wisdom is not significantly different from the expected value, 
#there is no significant effect of the driver's class on whether the police officer solicits bribes from the driver. 
#So the results can be interpreted as: there is no evidence of whether officers were more or less likely to solicit a bribe from drivers depending on their class.


#Question 2: Economics
#(a)State a null and alternative (two-tailed) hypothesis.
#Ho(null hypothesis):The reservation policy has no effect on the number of new or repaired drinking water facilities in the villages.
#H1(alternative hypothesis):The。reservation policy has effect on the number of new or repaired drinking water facilities in the villages.


#(b)Run a bivariate regression to test this hypothesis in R
women_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
model <- lm(water~reserved, data = women_data)
summary(model)
#Since the p-value = 0.0197 is less than 0.05, the null hypothesis is rejected, so the reservation policy has 
#effect on the number of new or repaired drinking water facilities in the villages.


#(c)Interpret the coefficient estimate for reservation policy.
#The coefficient estimate for the reservation policy is 9.252, indicating that the reservation policy is 
#positively correlated with the number of new or repaired drinking water facilities in the villages. As a result, 
#villages with the reservation policies had an average of 9.252 more new or repaired drinking water facilities 
#than villages without reservation policies.
