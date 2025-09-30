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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
student_iqs <- y

# part 1: 90% CI
n <- length(na.omit(student_iqs))
sample_mean <- mean (student_iqs, na.rm = TRUE)
sample_sd <- sd(student_iqs, na.rm = TRUE)
z90 <- qnorm((1 - .90)/2, lower.tail = FALSE)
lower_90 <- sample_mean- (z90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (z90 * (sample_sd/sqrt(n)))
confint90 <- c(lower_90, upper_90)

# part 2: hypothesis test
#Ho = 100, Ha > 100
population_mean <- 100
alpha_value <- 0.05
df <- n - 1
student_iq_data <- rnorm(n = 25, mean = 98.44, sd = 13.0928733795654)
t_stat <- (sample_mean - population_mean) / (sample_sd / sqrt(n))
pt(t_stat, df, lower.tail = FALSE)


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
