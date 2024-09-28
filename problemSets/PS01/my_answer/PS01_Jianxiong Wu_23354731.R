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

lapply(c("stargazer"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t.test(y, conf.level = 0.90, alternative = "two.sided")

t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

str(expenditure)

#Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations
#among them (you just need to describe the graph and the relationships among them)?
pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/scatter_plot1_X1&Y.pdf")
plot(expenditure$X1, expenditure$Y,
     xlab = "per capita personal income(X1)",
     ylab = "per capita expenditure on shelters/housing assistance(Y)",
     main = "The Relationship between X1 and Y")
dev.off()

pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/scatter_plot2_X2&Y.pdf")
plot(expenditure$X2,expenditure$Y,
     xlab = "Number of residents per 100,000 that are 'financially insecure'(X2)",
     ylab = "per capita expenditure on shelters/housing assistance(Y)",
     main = "The Relationship between X2 and Y")
dev.off()

pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/scatter_plot3_X3&Y.pdf")
plot(expenditure$X3,expenditure$Y,
     xlab = "Number of people per thousand residing in urban areas(X3)",
     ylab = "per capita expenditure on shelters/housing assistance(Y)",
     main = "The Relationship between X3 and Y")
dev.off()

pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/scatter_plot4_X1&X2.pdf")
plot(expenditure$X1,expenditure$X2,
     xlab = "per capita personal income(X1)",
     ylab = "pNumber of residents per 100,000 that are 'financially insecure'(X2)",
     main = "The Relationship between X1 and X2")
dev.off()

pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/scatter_plot5_X1&X3.pdf")
plot(expenditure$X1, expenditure$X3,
     xlab = "per capita personal income(X1)",
     ylab = "Number of people per thousand residing in urban areas(X3)",
     main = "The Relationship between X1 and X3")
dev.off()

pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/scatter_plot6_X2&X3.pdf")
plot(expenditure$X2, expenditure$X3,
     xlab = "pNumber of residents per 100,000 that are 'financially insecure'(X2)",
     ylab = "Number of people per thousand residing in urban areas(X3)",
     main = "The Relationship between X2 and X3")
dev.off()

cor(expenditure[,c("Y", "X1", "X2", "X3")])

#Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance? 
pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/boxplot_plot7_Y&Region.pdf")
boxplot(expenditure$Y ~ expenditure$Region, 
        main="per capita expenditure of Region",
        ylab="per capita expenditure",
        xlab="Region",
        names=c("Northeast", "North Central", "South", "West"))
dev.off()


#Please plot the relationship between Y and X1 ? Describe this graph and the rela-
#tionship. Reproduce the above graph including one more variable Region and display
#different regions with different types of symbols and colors.
pdf(file="/Users/wuhadou/Desktop/StatsI_Fall2024-main/problemSets/PS01/my_answer/scatter_plot8_X1&Y_Region.pdf")
plot(expenditure$X1, expenditure$Y,
     col = expenditure$Region,
     xlab = "per capita personal income(X1)",
     ylab = "per capita expenditure on shelters/housing assistance(Y)",
     main = "The Relationship between per capita personal income and per capita expenditure by Region")
legend("topleft", 
       legend = c("Northeast", "North Central", "South", "West"), 
       col = 1:4,
       pch = 1)
dev.off()

# run an example regression, to show how to save table
regression1 <- lm(Y~X1, data=expenditure)
regression1 <- lm(Y~X2, data=expenditure)
regression1 <- lm(Y~X3, data=expenditure)
regression1 <- lm(X1~X3, data=expenditure)
regression1 <- lm(X2~X3, data=expenditure)
regression1 <- lm(X2~X3, data=expenditure)
# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("regression_output1.tex", regression1)

regression2 <- lm(Y~Region, data=expenditure)
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
output_stargazer("regression_output2.tex", regression2)

