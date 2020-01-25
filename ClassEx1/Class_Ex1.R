library(dplyr)

#Initial regression model
data <- read.csv("bank.csv")

data$balance.f <- factor(data$balance)
is.factor(data$balance.f)

summary(lm(age~balance.f, data = data))

# F-test
res.ftest1 <- var.test(age ~ y, data =data)
res.ftest2 <- var.test(balance~y  , data =data)
res.ftest3 <- var.test(day ~ y, data =data)
res.ftest4 <- var.test(duration ~ y, data =data)
res.ftest5 <- var.test(campaign ~ y, data =data)
res.ftest6 <- var.test(previous ~ y, data =data)

res.ftest1
#The p-value of F-test is p = 2.682e-16 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

res.ftest2
#The p-value of F-test is p = 5.734e-11 which is greater than the significance level 0.05. 
#In conclusion, there is no significant difference between the two variances.

res.ftest3
#The p-value of F-test is p = 0.9707 which is greater than the significance level 0.05. 
#In conclusion, there is no significant difference between the two variances.

res.ftest4
#The p-value of F-test is p < 2.2e-16 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

res.ftest5
#The p-value of F-test is p < 2.2e-16 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

res.ftest6
#The p-value of F-test is p = 4.938e-14 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

#Resulting model 
data1 <- subset(data, select = c(6,10))

data1$balance.f <- factor(data1$balance)
is.factor(data1$balance.f)

summary(lm(balance~balance.f, data = data1))

#The resulting model ouputs the following-
#Residual standard error: 3.278e-10 on 2168 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#F-statistic: 1.62e+26 on 2352 and 2168 DF,  p-value: < 2.2e-16

#comparing it to the previous model's output -
#Residual standard error: 10.34 on 2168 degrees of freedom
#Multiple R-squared:  0.5417,	Adjusted R-squared:  0.04441 
#F-statistic: 1.089 on 2352 and 2168 DF,  p-value: 0.0212

#We can see that the Residual standard error, Multiple R-squared,
#Adjusted R-squared, and p-value went down and F-statistic value went up. 
#As the In general, a F-statistic is a ratio of two quantities that are 
#expected to be roughly equal under the null hypothesis, which produces an 
#F-statistic of approximately 1. So, the new model's F statistic is closer to 1
#thus the new model has a better F statistic and the new model is an improvement on 
#the previous model.  
