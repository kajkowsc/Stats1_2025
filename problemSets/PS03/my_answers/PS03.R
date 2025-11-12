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
setwd("/Users/carolinekajkowski/Desktop/Trinity/Statistical Analysis I/Stats2025/problemSets/PS03/my_answers")

# read in data
inc.sub <- read.csv("/Users/carolinekajkowski/Desktop/Trinity/Statistical Analysis I/Stats2025/datasets/incumbents_subset.csv")
install.packages("stargazer")
library(stargazer)

#Q1 

biregress_1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(biregress_1)

stargazer(biregress_1, 
          type = "latex",
          title = "Incumbent Vote Share explained by Difference of Spending",
          column.labels = "Coefficients (Reg 1)",
          covariate.labels = "difflog",
          dep.var.labels = "voteshare")

plot(inc.sub$difflog, inc.sub$voteshare,
     main = "Plot of Difference in Spending and Vote Share",
     xlab = "Difference of Spending between incumbent and challenger", 
     ylab = "Incumbent's Vote Share ", 
     abline(biregress_1, col = "red", lwd = 2))

res_1 <- biregress_1$residuals 

#Yi = 0.579031 + 0.041666Xi + ei

#Q2
biregress_2 <- lm(presvote ~ difflog, data = inc.sub)
summary(biregress_2)

stargazer(biregress_2, 
          type = "latex",
          title = "Presidential Candidate Vote Share explained by Difference of Spending",
          column.labels = "Coefficients (biregress 2)",
          covariate.labels = "difflog",
          dep.var.labels = "presvote")

plot(inc.sub$difflog, inc.sub$presvote,
     main = "Plot of Difference in Spending and Vote Share of the Incumbent Presidential Candidate",
     xlab = "Difference of Spending between incumbent and challenger", 
     ylab = "Imcumbent Presidential Candidate Vote Share", 
     abline(biregress_2, col = "red", lwd = 2))

res_2 <- biregress_2$residuals

# Yi = 0.507583 + 0.023837Xi + ei

#Q3
biregress_3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(biregress_3)

stargazer(biregress_3, 
          type = "latex",
          title = "Incumbents Vote Share explained by Incumbent Presidential Candidate Vote Share",
          column.labels = "Coefficients (biregress 3)",
          covariate.labels = "presvote",
          dep.var.labels = "voteshare")

plot(inc.sub$presvote, inc.sub$voteshare,
     main = "Relationship Between Presidential and Incumbent Party Vote Share",
     xlab = "Imcumbent Presidential Candidate Vote Share", 
     ylab = "Incumbent Party's Vote Share", 
     abline(biregress_3, col = "red", lwd = 2))

#Yi = 0.441330 + 0.388018Xi + ei

#Q4
resid_regress <- lm(res_1 ~ res_2)
summary(resid_regress)

stargazer(resid_regress, 
          type = "latex",
          title = "Residuals of Incumbents Vote Share explained by Residuals of Incumbent Presidential Candidate Vote Share",
          column.labels = "Coefficients (residuals)",
          covariate.labels = "res_2",
          dep.var.labels = "res_1")

plot(res_2, res_1,
     main = "Plot of Residuals from Question 1 and 2",
     xlab = "variation in presvote not explained by the difference in spending",
     ylab = "variation in voteshare not explained by the difference in spending",
     abline(resid_regress, col = "red", lwd = 2))

#Yi = -1.942e-18 + 0.2569Xi + ei

#Q5
multi_regress <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(multi_regress)

stargazer(multi_regress, 
          type = "latex",
          title = "Incumbents Vote Share explained by Difference in Spending and Incumbent Presidential Candidate Vote Share",
          column.labels = "Coefficients (residuals)",
          covariate.labels = c("difflog", "presvote"),
          dep.var.labels = "voteshare")

#Yi = 0.4486442 + 0.0355431X1i + 0.2568770X2i + ei