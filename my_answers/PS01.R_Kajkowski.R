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

#We are 90% confident that the true population mean of student IQ in the school lie around 93.96 and 102.92. 

# part 2: hypothesis test
#Ho <= 100
#Ha > 100
t.test(student_iqs, mu = 100, alternative = "greater")
#For this hypothesis test, the null hypothesis is that the population average of student IQ is 100 or lower. However, the alternative hypothesis states that the average is higher than 100. Therefore, this question is looking for a one sided t test, testing if there is sufficient evidence to reject the null. Using the information about the sample that was found from the part above (mean and standard deviation), the t statistic can be calculated with this equation: t = (sample mean - hypothesized population mean)/(standard deviation/square root of the sample size) = (98.44 - 100) / (13.093/√25). In order to discover if the null hypothesis can be rejected, we need to find the p value. In the code above, it gives us the value. To find it outside of coding, you need the degrees of freedom and t table to look up a range of scores the t value falls under. If that range is less than the alpha, the one would reject the null and vise versa one would fail to reject the null. In this problem, the calculated p value is 0.7215 is higher than our alpha of 0.05, therefore, we fail to reject the null hypothesis and there is insufficient evidence to prove the alternative hypothesis. 

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

#part 1: plot the relationships among Y, X1, X2, and X3

pdf("combined_plots.pdf", width = 12, height = 8)
par(mfrow = c(2,3))
plot(expenditure$Y,expenditure$X1,
     main = "Y_X1",
     ylab = "Y expenditure per capita",
     xlab = "X1 personal income per capita"
)

plot(expenditure$Y,expenditure$X2,
     main = "Y_X2",
     ylab = "Y expenditure per capita",
     xlab = "X2 residents per 100,000"
)

plot(expenditure$Y,expenditure$X3,
     main = "Y_X3",
     ylab = "Y expenditure per capita ",
     xlab = "X3 people per 1,000"
)

plot(expenditure$X1,expenditure$X2,
     main = "X1_X2", 
     ylab = "X1 personal income per capita", 
     xlab = "X2 residents per 100,000"
)

plot(expenditure$X1,expenditure$X3,
     main = "X1_X3",
     ylab = "X1 personal income per capita",
     xlab = "X3 people per 1,000"
)

plot(expenditure$X2,expenditure$X3,
     main = "X2_X3",
     ylab = "X2 residents per 100,000",
     xlab = "X3 people per 1,000"
)

dev.off()

#Y_X1: the data is looking at the relationship between per capita expenditure on shelter/housing assistance and per capita personal income in state. The graph shows that as the personal income increases, so does the expenditure on shelter and housing assistance, therefore, we can say there is a correlation. However, the points aren't perfectly lined up so, the correlation is weak to moderate. 
#For the Y_X2 graph, the data is looking at the relationship between per capita expenditure on shelter/housing assistance and number of residents that are "financially insecure" per 100,000 residents. The graph shows that as "financially insecure" residents rise by 100,000, expenditure doesn't always increase as well. In certain areas were the expenditure is 300 and above there is more of a correlation, but lower than that the correlation is more scattered. 
#For the Y_X3 graph, the data is looking at the relationship between per capita expenditure on shelter/housing assistance and number of people per thousand residing in urban areas in state. The graph shows that as the number of people residing increases, the expenditure also increases. The scatter plot shows a weak to moderate positive linear correlation. 
#For the X1_X2 graph, 
#For the X1_X3 graph,
#For the X2_X3 graph, 

#part 2: 

par(mfrow = c(1,1))
boxplot(Y ~ Region, data = expenditure,
        main = "Per Capita Expenditure on Housing Assistance by Region",
        xlab = "Region (1=Northeast, 2=North Central, 3=South, 4=West)",
        ylab = "Y: Housing Expenditure per Capita")

#For this question, the best way to visualize the different averages for each region is by using box plots. By looking at the box plots, the West has the highest per capita expenditure on housing/shelter assistance. By looking at the dark line which symbolizes the mean/average, the West's mean is around 85, which is higher than the other regions' average values. 
      
#part 3: plot the relationship between Y and X1
  
plot(expenditure$Y,expenditure$X1,
     main = "Relationship between Expenditure on shelters/hosing assistance 
     and Personal income in state",
     ylab = "Expenditure per capita",
     xlab = "Personal income per capita"
)

regions <- unique(expenditure$Region)
colors <- c("forestgreen","skyblue","salmon","rosybrown")[1:length(regions)]
shapes <- c(16, 17, 18, 15)[1:length(regions)]
point_colors <- colors[match(expenditure$Region, regions)]
point_shapes <- shapes[match(expenditure$Region, regions)]
     
plot(expenditure$Y,expenditure$X1,
     col = point_colors,
     pch = point_shapes,
     main = "Relationship between Expenditure on shelters/hosing assistance 
     and Personal income in state",
     ylab = "Expenditure per capita",
     xlab = "Personal income per capita"
)
legend(
  "bottomright", 
  legend = region_names, 
  col = colors, 
  pch = shapes,
  inset = 0.0
  )

#The relationship between per capita expenditure on shelter/housing assistance and per capita personal income per state shows a weak to moderate postivie linear relationship. Meaning, as the personal income increase, the expenditure on housing/shelter assistance also tend to increase 
      