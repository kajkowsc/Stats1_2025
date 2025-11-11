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

#Q1 

biregress_1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(biregress_1)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.26832 -0.05345 -0.00377  0.04780  0.32749 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.579031   0.002251  257.19   <2e-16 ***
#  difflog     0.041666   0.000968   43.04   <2e-16 ***

#Residual standard error: 0.07867 on 3191 degrees of freedom
#Multiple R-squared:  0.3673,	Adjusted R-squared:  0.3671 
#F-statistic:  1853 on 1 and 3191 DF,  p-value: < 2.2e-16 

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
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.32196 -0.07407 -0.00102  0.07151  0.42743 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.507583   0.003161  160.60   <2e-16 ***
#  difflog     0.023837   0.001359   17.54   <2e-16 ***

#Residual standard error: 0.1104 on 3191 degrees of freedom
#Multiple R-squared:  0.08795,	Adjusted R-squared:  0.08767 
#F-statistic: 307.7 on 1 and 3191 DF,  p-value: < 2.2e-16

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
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.27330 -0.05888  0.00394  0.06148  0.41365 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.441330   0.007599   58.08   <2e-16 ***
#  presvote    0.388018   0.013493   28.76   <2e-16 ***

#Residual standard error: 0.08815 on 3191 degrees of freedom
#Multiple R-squared:  0.2058,	Adjusted R-squared:  0.2056 
#F-statistic:   827 on 1 and 3191 DF,  p-value: < 2.2e-16

plot(inc.sub$presvote, inc.sub$voteshare,
     main = "Relationship Between Presidential and Incumbent Party Vote Share",
     xlab = "Imcumbent Presidential Candidate Vote Share", 
     ylab = "Incumbent Party's Vote Share", 
     abline(biregress_3, col = "red", lwd = 2))

#Yi = 0.441330 + 0.388018Xi + ei

#Q4
resid_regress <- lm(res_1 ~ res_2)
summary(resid_regress)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.25928 -0.04737 -0.00121  0.04618  0.33126 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.942e-18  1.299e-03    0.00        1    
# res_2      2.569e-01  1.176e-02   21.84   <2e-16 ***
#  ---

#Residual standard error: 0.07338 on 3191 degrees of freedom
#Multiple R-squared:   0.13,	Adjusted R-squared:  0.1298 
#F-statistic:   477 on 1 and 3191 DF,  p-value: < 2.2e-16

plot(res_2, res_1,
     main = "Plot of Residuals from Question 1 and 2",
     xlab = "variation in presvote not explained by the difference in spending",
     ylab = "variation in voteshare not explained by the difference in spending",
     abline(resid_regress, col = "red", lwd = 2))

#Yi = -1.942e-18 + 0.2569Xi + ei

#Q5
multi_regress <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(multi_regress)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.25928 -0.04737 -0.00121  0.04618  0.33126 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.4486442  0.0063297   70.88   <2e-16
#difflog     0.0355431  0.0009455   37.59   <2e-16
#presvote    0.2568770  0.0117637   21.84   <2e-16

#Residual standard error: 0.07339 on 3190 degrees of freedom
#Multiple R-squared:  0.4496,	Adjusted R-squared:  0.4493 
#F-statistic:  1303 on 2 and 3190 DF,  p-value: < 2.2e-16

#Yi = 0.4486442 + 0.0355431X1i + 0.2568770X2i + ei