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
t90 <- qt((1 - .90)/2, df = 24, lower.tail = FALSE)
lower_90 <- sample_mean- (t90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (t90 * (sample_sd/sqrt(n)))
confint_90 <- c(lower_90, upper_90)

# part 2: hypothesis test
#Ho = 100, Ha > 100
t.test(student_iqs, mu = 100, alternative = "greater")
#EXPLAIN

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

#part 1: plot the relationships among Y, X1, X2, and X3

Y_X1 <- plot(expenditure$Y,expenditure$X1,
     main = "Relationship between Shelters/housing Assistance 
     and Personal Income in state", 
     ylab = "Y expenditure per capita",
     xlab = "X1 personal income per capita"
     )

Y_X2 <- plot(expenditure$Y,expenditure$X2,
     main = "Relationship between Shelters/housing Assistance
     and Residents per 100,000 that are 
     'financially insecure' in state", 
     ylab = "Y expenditure per capita",
     xlab = "X2 residents per 100,000"
)

Y_X3 <- plot(expenditure$Y,expenditure$X3,
     main = "Relationship between Shelters/housing Assistance
     and People residing in Urban Areas in state",
     ylab = "Y expenditure per capita ",
     xlab = "X3 people per 1,000"
)

X1_X2 <- plot(expenditure$X1,expenditure$X2,
     main = "Relationship between Personal Income and 
     Residents that are 'financially insecure' in state",
     ylab = "X1 personal income per capita", 
     xlab = "X2 residents per 100,000"
)

X1_X3 <- plot(expenditure$X1,expenditure$X3,
     main = "Relationship between Personal Income 
     and People residing in Urban Areas in state",
     ylab = "X1 personal income per capita",
     xlab = "X3 people per 1,000"
)

X2_X3 <- plot(expenditure$X2,expenditure$X3,
     main = "Relationship between Residents that are'financially insecure'
     and People residing in Urban Areas in state",
     ylab = "X2 residents per 100,000",
     xlab = "X3 people per 1,000"
)


#part 2: plot relationship between Y and Region 

plot(expenditure$Y, expenditure$Region)
main = 
ylab =
xlab = 

      
      
      
#part 3: plot the relationship between Y and X1
  

      