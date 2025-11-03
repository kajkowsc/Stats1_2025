#Q1 part (a)
#Ho = class and soliciting a bride are independent, Ha = class and soliciting a bride are dependent
class_bribe <- as.table(matrix(c(14,6,7,7,7,1), nrow = 2, byrow = TRUE,
                               dimnames = list(class = c("Upper class", "Lower class"),
                                               bribe = c("Not Stopped","Bribe requested", "Stopped/given warning"))))
print(class_bribe)

sum_class_rows <- rowSums(class_bribe)
print(sum_class_rows)
#Upper class Lower class 
#27          15 
sum_bribe_columns <- colSums(class_bribe)
print(sum_bribe_columns)
#Not Stopped       Bribe requested Stopped/given warning 
#21                    13                     8 
total_sum <- sum(class_bribe)
#[1] 42

expected_up_not <- (27/42)*21
expected_low_not <- (15/42)*21
expected_up_bribe <- (27/42)*13
expected_low_bribe <- (15/42)*13
expected_up_stop <- (27/42)*8                        
expected_low_stop <- (15/42)*8


expected_values_class_bribe <- matrix(c(expected_up_not, expected_up_bribe, expected_up_stop, expected_low_not, expected_low_bribe, expected_low_stop), nrow = 2, byrow = TRUE,
                                      dimnames = list(class = c("Upper class", "Lower class"),
                                                      bribe = c("Not Stopped","Bribe requested", "Stopped/given warning")))
print(expected_values_class_bribe)

chi_sq_class_bribe <- sum(((class_bribe - expected_values_class_bribe)^2)/expected_values_class_bribe)
#[1] 3.791168

#Q1 part (b) 
pchisq(chi_sq_class_bribe, df = 2, lower.tail = FALSE)
#[1] 0.1502306
#Fail to reject the null and there is insufficient evidence to support alternative

#Q1 part (c) 
chisq_result <- chisq.test(class_bribe, correct = FALSE)
standardized_residuals <- as.table(chisq_result$stdres)
print(standardized_residuals)


#Q2 part (a)
women_data <- read.csv("/Users/carolinekajkowski/Desktop/Trinity/my_answers/women.csv")
#Ho: p = 0, Ha: p =/ 0

#Q2 part (b)
summary(lm(women_data$water~women_data$reserved))
#Residuals:
# Min      1Q  Median      3Q     Max 
#-23.991 -14.738  -7.865   2.262 316.009 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           14.738      2.286   6.446 4.22e-10 ***
#  women_data$reserved    9.252      3.948   2.344   0.0197 *  
  ---
#Residual standard error: 33.45 on 320 degrees of freedom
#Multiple R-squared:  0.01688,	Adjusted R-squared:  0.0138 
#F-statistic: 5.493 on 1 and 320 DF,  p-value: 0.0197

#Q2 part (c)
