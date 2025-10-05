# Applied Statistical Analysis I      
# Tutorial 2: Hypothesis testing, experiments, difference in means                       

# Get working directory
getwd()

# Set working directory 
setwd("/Users/carolinekajkowski/Desktop/Trinity/Statistical Analysis I/Stats2025/datasets")
getwd()

df <-read.csv("fictional_data.csv")

# Agenda
# (a.) Descriptive analysis
# (b.) Confidence intervals
# (c.) Significance test for a mean
# (d.) Significance test for a difference in means

### Research Question -----------
# Is there a relationship between education and income?

# Load data 


# Selection of variables
# Education: University level education in years
# Income: Monthly net income
# Capital: Whether the person lives in capital or not

### (a.) Descriptive analysis ----------

# First step, look at data

View(df)
head(df)
str(df)

# Step by step

mean(df$income)
var(df$income)
sd(df$income)
sd(df$income)/sqrt(length(df$income))

# Get summary statistics for entire dataset


# Some quick visualizations, to look at distribution
hist(df$income,
     main = "monthy income",
     xlab = "euro")

plot(density(df$income),
     main = "monthy income",
     xlab = "euro")

# Which kind of inferences can we make with regards to the population,
# based on the sample data?

mean(df$income)

# Standard **error** (Sample standard deviation adjusted by sample size)
# is estimate for standard deviation of the sampling distribution

# Why do we need standard error? --> to calculate measures of
# uncertainty for our point estimate (e.g., confidence intervals, and p-values)

# (b.) Confidence intervals --------
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# What do we need?


# How to find the multiple?
# Looking at the normal distribution, we see that 
# 95% of observations lie within +/-1.96 (approximately 2)
# standard errors of point estimate 

# The **approximate** solution 
# Lower bound, 95 confidence level

upper_95 = mean(df$income) + (1.96*sd(df$income)/sqrt(length(df$income)))
lower_95 = mean(df$income) - (1.96*sd(df$income)/sqrt(length(df$income)))

# Print

lower_95
mean(df$income)
upper_95

# The **precise** solution, using normal distribution
# Lower bound, 95 confidence level


# Upper bound, 95 confidence level

# Step by step


# Print


# How to calculate 99% confidence intervals?
# When to use normal distribution and when to use t distribution?

t_score <- qt(0.995, df = length(df$income)-1)

lower_99_t <- mean(df$income) - (t_score) * (sd(df$income)/sqrt(length(df$income)))
upper_99_t <- mean(df$income) + (t_score) * (sd(df$income)/sqrt(length(df$income)))

# The **precise** solution, using t distribution

lower_99_t
mean(df$income)
upper_99_t

# Step by step 


# Print


# Update Histogram 

hist(df$income)
abline(v=mean(df$income), col = "black")
abline(v=lower_95, col ="black", lty = "dashed")
abline(v=upper_95, col ="black", lty = "dashed")
abline(v=lower_99_t, col = "blue", lty = "dashed")
abline(v=upper_99_t, col = "blue", lty = "dashed")

# Is there a relationship between education and income?

# Scatter plot 
plot(df$income, df$edu)

# Improve visualization and save


# Boxplot


# (c.) Significance test for a mean ------

# What is the average monthly income in Ireland
# According to a quick Google search, it is 3034
# How does our sample compare to the population, 
# being the working population in Ireland?

t.test(df$income, mu = 3034)

# We also found a much easier way to calculate the confidence intervals (!)
t.test(df$income, mu = 3034, alternative = "less")

# Let's double check


# (d.) Significance test for a difference in means ------

# On average, do people earn differently in the capital
# compared to people who do not reside in the capital?

# Calculate means for subgroups

mean(df[df$cap ==0, ]$income)
mean(df[df$cap==1, ]$income)
# Step by step

t.test(df$income ~ df$cap, alternative = "two.sided")

# t-test


# On average, do people earn more in the capital
# compared to people who do not reside in the capital?

# t-test


