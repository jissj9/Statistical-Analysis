#Take the function from last week and modify it to create a function that performs 
#two-sample Z-tests and t-tests, with the assumption of equal variance. Make sure 
#to account for the case where you want to test proportions. (don't forget to comment 
#your modifications to the code)

#Definitions:
#let var Pop_D be the difference in means of the two populations under the null hypothesis
#       OR be the difference in proportions of the two populations under the null hypothesis
#let var n1 be the size of the first sample
#let var n2 be the size of the second sample
#let var x_bar1 be the mean of the first sample
#       OR be the proportion of the first sample
#let var x_bar2 be the mean of the second sample
#       OR be the proportion of the second sample
#let var sd1 be the known sd of the first population ,or our estimate of it from the first sample
#let var sd2 be the known sd of the second population ,or our estimate of it from the second sample
#let alpha be the significance level eg 0.05
#let string var tails be "two" "left" or "right" indicating a two-tailed test or direction of one-tailed

#Code for a two-sample hypothesis test function
TwoSampTest <-function(type=NULL, tails=NULL, proportion=NULL, alpha, Pop_D, n1, n2, x_bar1, x_bar2, sd1, sd2)
{
  #Calculate the difference between the sampled means(diff)
  diff = x_bar1-x_bar2
          
  #Calculate standard error(se) based on whether test is using proportions or not
  if(proportion=="no"){
    pooledSD = sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))
    se = pooledSD*sqrt(1/n1+1/n2) 
  }else if(proportion=="yes"){
    pooledPro = (x_bar1*n1+x_bar2*n2)/(n1+n2)
    se = sqrt(pooledPro*(1-pooledPro)*(1/n1+1/n2))
  }else{stop("please choose proportion as yes or no")}
  
  #Calculate the test statistic
  test_stat <- (x_bar1-x_bar2-Pop_D)/se
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n1+n2-2  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #Check if significant
  if (p_val < alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("Two Sample", type, "test.", tails, "tailed"), n1=n1, n2=n2, Pop_D=Pop_D, diff=diff , se_est=se, test_stat=test_stat, p = p_val, alpha = alpha, significance = sig )
  #Return the list
  return( ret )
}





#Simulate a vector of size 100 drawn from a Normal(4,5) distribution, and another 
#vector of size 80 drawn from a Normal(3.5,2) distribution. 

#Simulate a vector of size 100 drawn from a Normal(4,5) distribution
normalDistributionSample1 <- rnorm(100,4,5)
#Draw histogram to visulaise distribution
hist(normalDistributionSample1)
#Simulate a vector of size 80 drawn from a Normal(3.5,2) distribution
normalDistributionSample2 <- rnorm(80,3.5,2)
#Draw histogram to visualise distribution
hist(normalDistributionSample2)




#1)	H0: the difference in means of the populations are equal to 1. 
#   HA: the difference in means of the populations are not equal to 1. 
#   Use a Z-test with the true population SDs. Use alpha = 0.05.

#Record the size(n1 and n2) and the mean(x_bar1 and x_bar2) for each sample. 
#Also set the standard deviations(sd1 and sd2) to be the true population sds.
n1 <- length(normalDistributionSample1)
n2 <- length(normalDistributionSample2)
x_bar1 <- mean(normalDistributionSample1)
x_bar2 <- mean(normalDistributionSample2)
sd1 <- 5
sd2 <- 2

#Assign the significance level(alpha) and the difference in means under 
#the null hypothesis to variables(both given in question)
alpha <- 0.05
Pop_D <- 1

#Call the TwoSampTestFunction, passing in the following parameters;
#   "z" - perform a z-test
#   "two" - perform a two-tailed test
#   "no" - not a proportions test
#   alpha - use a 0.05 alpha level/significance level
#   Pop_D - difference in means of the two samples under the null hypothesis
#   n1 - size of the first sample
#   n2 - size of the second sample
#   x_bar1 - mean of the first sample
#   x_bar1 - mean of the second sample
#   sd1 - true population standard deviation of first population
#   sd2 - true population standard deviation of second population
TwoSampTest("z","two", "no", alpha, Pop_D, n1, n2, x_bar1, x_bar2, sd1, sd2)
#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - the type of test(type), 
#   - the size of the first sample(n1)
#   - the size of the second sample(n2)
#   - the difference in means of the two populationss under the null hypothesis(Pop_D)
#   - the difference between the sampled means(diff), 
#   - the standard error(se_est),
#   - the test statistic(test_stat)
#   - the p-value(p), 
#   - the alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance)

#My output for my normalDistributionSample was:
#   - type = "Two sample z test, two tailed"
#   - n1 = 100
#   - n2 = 80
#   - Pop_D = 1
#   - diff = 0.6486069
#   - se_est = 0.5939654
#   - test_stat = -0.5916054
#   - p = 0.5541149
#   - alpha = 0.05
#   - significance = "not significant"

#Test statistic = -0.5916054(this test statistic compares how different the difference in the sample means(diff) 
#is from the difference between the means of the two populations under the null hypothesis(Pop_D). If they were 
#both equal(i.e. if diff and Pop_D were equal), it would imply that there is no difference between them(i.e. the test 
#statistic would be 0). This would resultin the p-value being the maximum(i.e. 1) due to the p-value being based off 
#the z-score of the test statistic. In this case, the test statistic is quite small, resulting in a relatively large 
#p-value. This p-value is then compared with the alpha level chosen to determine significance.)

#p-value = 0.5541149(due to the small test statistic mentioned above, the p-value is quite large. It is larger 
#than 0.05(alpha level), meaning the result is not significant and hence we fail to reject the null hypothesis.)

#Conclusion:As the result is not significant(i.e. p > 0.05), we fail to reject the null hypothesis(H0) - that the
#difference in means of the populations are equal to 1. 








#2)	H0: the means of the populations are equal. 
#   HA: the means of the populations are not equal. 
#   Use a Z-test with the sample SDs. Use alpha = 0.05

#Record the size(n1 and n2) and the mean(x_bar1 and x_bar2) for each sample. 
#Also set the standard deviations(sd1 and sd2) to be the sample population sds
n1 <- length(normalDistributionSample1)
n2 <- length(normalDistributionSample2)
x_bar1 <- mean(normalDistributionSample1)
x_bar2 <- mean(normalDistributionSample2)
sd1 <- sd(normalDistributionSample1)
sd2 <- sd(normalDistributionSample2)

#Assign the significance level(alpha) and the difference in means hypothesised under 
#the null hypothesis to variables(both given in question)
alpha <- 0.05
Pop_D <- 0

#Call the TwoSampTestFunction, passing in the following parameters;
#   "z" - perform a z-test
#   "two" - perform a two-tailed test
#   "no" - not a proportions test
#   alpha - use a 0.05 alpha level/significance level
#   Pop_D - difference in means of the two samples under the null hypothesis
#   n1 - size of the first sample
#   n2 - size of the second sample
#   x_bar1 - mean of the first sample
#   x_bar1 - mean of the second sample
#   sd1 - sample population standard deviation of first sample
#   sd2 - sample population standard deviation of second sample
TwoSampTest("z","two", "no", alpha, Pop_D, n1, n2, x_bar1, x_bar2, sd1, sd2)
#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - the type of test(type), 
#   - the size of the first sample(n1)
#   - the size of the second sample(n2)
#   - the difference in means of the two populations under the null hypothesis(Pop_D)
#   - the difference between the sampled means(diff), 
#   - the estimate of the standard error(se_est),
#   - the test statistic(test_stat)
#   - the p-value(p), 
#   - the alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance)

#My output for my normalDistributionSample was:
#   - type = "Two sample z test, two tailed"
#   - n1 = 100
#   - n2 = 80
#   - Pop_D = 0
#   - diff = 0.6486069
#   - se_est = 0.5869198
#   - test_stat = 1.105103
#   - p = 0.269115
#   - alpha = 0.05
#   - significance = "not significant"

#Test statistic = 1.105103(this test statistic compares how different the difference in the sample means(diff) is
#from the difference between the means of the two populations under the null hypothesis(Pop_D). If they were both 
#equal(i.e. if diff and Pop_D were equal), it would imply that there is no difference between them(i.e. the test 
#statistic would be 0). This would result in the p-value being the maximum(i.e. 1) due to the p-value being based 
#off the z-score of the test statistic. In this case, the test statistic is quite small, resulting in a relatively 
#large p-value. This p-value is then compared with the alpha level chosen to determine significance.

#p-value = 0.269115(due to the small test statistic mentioned above, the p-value is quite large. It is larger than 
#0.05(alpha level), meaning the result is not significant and hence we fail to reject the null hypothesis.

#Conclusion:As the result is not significant(i.e. p > 0.05), we fail to reject the null hypothesis(H0) - that the 
#difference in means of the populations are equal to 0. 







#3)	H0: the means of the populations are equal. 
#   HA: the means of the populations are not equal. 
#   Use a t-test. Use alpha = 0.05.

#Record the size(n1 and n2) and the mean(x_bar1 and x_bar2) for each sample. 
#Also set the standard deviations(sd1 and sd2) to be the sample population sds
n1 <- length(normalDistributionSample1)
n2 <- length(normalDistributionSample2)
x_bar1 <- mean(normalDistributionSample1)
x_bar2 <- mean(normalDistributionSample2)
sd1 <- sd(normalDistributionSample1)
sd2 <- sd(normalDistributionSample2)

#Assign the significance level(alpha) and the difference in means hypothesised under 
#the null hypothesis to variables(both given in question)
alpha <- 0.05
Pop_D <- 0

#Call the TwoSampTestFunction, passing in the following parameters;
#   "t" - perform a t-test
#   "two" - perform a two-tailed test
#   "no" - not a proportions test
#   alpha - use a 0.05 alpha level/significance level
#   Pop_D - difference in means of the two samples under the null hypothesis
#   n1 - size of the first sample
#   n2 - size of the second sample
#   x_bar1 - mean of the first sample
#   x_bar1 - mean of the second sample
#   sd1 - sample population standard deviation of first sample
#   sd2 - sample population standard deviation of second sample
TwoSampTest("t","two","no", alpha, Pop_D, n1, n2, x_bar1, x_bar2, sd1, sd2)
#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - the type of test(type), 
#   - the size of the first sample(n1)
#   - the size of the second sample(n2)
#   - the difference in means of the two populations under the null hypothesis(Pop_D)
#   - the difference between the sampled means(diff), 
#   - the estimate of the standard error(se_est),
#   - the test statistic(test_stat)
#   - the p-value(p), 
#   - the alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance)

#My output for my normalDistributionSample was:
#   - type = "Two sample t test, two tailed"
#   - n1 = 100
#   - n2 = 80
#   - Pop_D = 0
#   - diff = 0.6486069
#   - se_est = 0.5869198
#   - test_stat = 1.105103
#   - p = 0.1353032
#   - alpha = 0.05
#   - significance = "not significant"

#Test statistic = 1.105103(this test statistic compares how different the difference in the sample means(diff) is
#from the difference between the means of the two populations under the null hypothesis(Pop_D). If they were both 
#equal(i.e. if diff and Pop_D were equal), it would imply that there is no difference between them(i.e. the test 
#statistic would be 0). This would result in the p-value being the maximum(i.e. 1) due to the p-value being based 
#off the z-score of the test statistic. In this case, the test statistic is quite small, resulting in a relatively 
#large p-value. This p-value is then compared with the alpha level chosen to determine significance.(Note that the 
#test statistic for question 2 and 3 are the same as the only difference is one uses a z-test while one uses a t-test)


#p-value = 0.1353032 - (due to the small test statistic mentioned above, the p-value is quite large. It is larger than 
#0.05(alpha level), meaning the result is not significant and hence we fail to reject the null hypothesis.(Note that 
#despite the test statistic being the same, the p-values were different for the z-test and t-test,  however, both lead 
#to the dame conclusion)

#Conclusion:As the result is not significant(i.e. p > 0.05), we fail to reject the null hypothesis(H0) - that the 
#difference in means of the populations are equal to 0. 







#4)	Repeat the previous task using the t.test function in the package {stats}

t.test(x = normalDistributionSample1, y = normalDistributionSample2, alternative = "two.sided", mu = Pop_D, var.equal = TRUE)
#My output from t.test function:
#   - t = 1.1051
#   - df = 178
#   - p-value = 0.2706
#   - mean of x(first sample) = 4.403451
#   - mean of y(second sample) = 3.754845
#   - 95% CI = [-0.5096095, 1.8068233]
#These values for the test statistic(t) and p-value correspond to the previous output generated by the function used
#previuosly on the same sample, both functions lead to the same conclusion - that the result is not significant
#(i.e. p > 0.05) and we fail to reject the null hypothesis. 







#5)	Repeat the previous task using the t.test function in the package {stats}, without the assumption of equal variance.
t.test(x = normalDistributionSample1, y = normalDistributionSample2, alternative = "two.sided", mu = Pop_D)
#My output from t.test function:
#   - t = 1.2011
#   - df = 133.03
#   - p-value = 0.2318
#   - mean of x(first sample) = 4.403451
#   - mean of y(second sample) = 3.754845 
#   - 95% CI = [-0.419505, 1.7167194]
# These values for the test statistic(t) and p-value correspond to the previous output generated by the function used
#previuosly on the same sample, both functions lead to the same conclusion - that the result is not significant
#(i.e. p > 0.05) and we fail to reject the null hypothesis.(As equal variance is not assumed, the function automatically
#assumes unequal variance, leading to a different t, df, p-value and hence different CI.) 





#Simulate a vector containing the results of 100 independent Bernoulli trials, each with probability of success 0.3 
#using the function rbern(n, prob). Simulate another vector containing the results of 85 independent Bernoulli trials, 
#each with probability of success 0.7. 

#Simulate a vector containing the results of 100 independent Bernouli trials, 
#each with a probability of success of 0.3.
bernouliDistributionSample1 <- rbern(100,0.3)
#Draw histogram to visualise distribution
hist(bernouliDistributionSample1)
#Simulate a vector containing the results of 85 independent Bernouli trials,
#each with a probability of success of 0.7
bernouliDistributionSample2 <- rbern(85,0.7)
#Draw histogram to visualise distribution
hist(bernouliDistributionSample2)






#6)	H0: the population proportions are equal. 
#   HA: the population proportions are not equal 

#Record the size(n1 and n2), the proportion(x_bar1 and x_bar2) for each sample and 
#the standard deviation(sd1 and sd2) for each sample 
n1 <- length(bernouliDistributionSample1)
n2 <- length(bernouliDistributionSample2)
x_bar1 <- mean(bernouliDistributionSample1)
x_bar2 <- mean(bernouliDistributionSample2)
sd1 <- sqrt((0.3)*(1-0.3))
sd2 <- sqrt((0.7)*(1-0.7))

#Assign the significance level(alpha) and the difference in proportions hypothesised 
#under the null hypothesis to variables(both given in question)
alpha <- 0.05
Pop_D <- 0

#Call the TwoSampTestFunction, passing in the following parameters;
#   "z" - perform a z-test
#   "two" - perform a two-tailed test
#   "yes" - is a proportions test
#   alpha - use a 0.05 alpha level/significance level
#   Pop_D - difference in proportions of the two populations under the null hypothesis
#   n1 - size of the first sample
#   n2 - size of the second sample
#   x_bar1 - proportion of the first sample
#   x_bar1 - proportion of the second sample
#   sd1 - sample population standard deviation of first sample
#   sd2 - sample population standard deviation of second sample
TwoSampTest("z","two", "yes", alpha, Pop_D, n1, n2, x_bar1, x_bar2, sd1, sd2)
#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - the type of test(type), 
#   - the size of the first sample(n1)
#   - the size of the second sample(n2)
#   - the difference in proportions of the two populations under the null hypothesis(Pop_D)
#   - the difference between the sampled proportions(diff), 
#   - the estimate of the standard error(se_est),
#   - the test statistic(test_stat)
#   - the p-value(p), 
#   - the alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance)

#My output for my normalDistributionSample was:
#   - type = "Two sample z test, two tailed"
#   - n1 = 100
#   - n2 = 85
#   - Pop_D = 0
#   - diff = -0.4076471
#   - se_est = 0.07376325
#   - test_stat = -5.526425
#   - p = 3.268225e-08
#   - alpha = 0.05
#   - significance = "significant"

#Test statistic = -5.526425(this test statistic compares how different the difference in the sample means(diff) is
#from the difference between the means of the two populations under the null hypothesis(Pop_D). If they were both 
#equal(i.e. if diff and Pop_D were equal), it would imply that there is no difference between them(i.e. the test 
#statistic would be 0). This would result in the p-value being the maximum(i.e. 1) due to the p-value being based 
#off the z-score of the test statistic. In this case, the test statistic is quite small, resulting in a relatively 
#large p-value. This p-value is then compared with the alpha level chosen to determine significance.

#p-value = 3.268225e-08(due to the large test statistic mentioned above, the p-value is quite small. It is smaller 
#than 0.05(alpha level), meaning the result is significant.

#Conclusion:As the result is significant(i.e. p > 0.05), we reject the null hypothesis(H0) and accept the alternative 
#hypothesis - that the population proportions are not equal.






#7)	Write code to calculate the 95% Confidence Interval for your estimate of the difference between
#the two population proportions.

diff = x_bar1-x_bar2
pooledPro = (x_bar1*n1+x_bar2*n2)/(n1+n2)
se_est = sqrt(pooledPro*(1-pooledPro)*(1/n1+1/n2))

#Calculate Z-score for 95% CI
Z_score <- qnorm(0.975)

#Calculate 95% CI for ? using known sd and z distributions
left_z95_kn <- diff-Z_score*se_est
right_z95_kn <- diff+Z_score*se_est

#Show confidence interval
paste("Z-distribution w/known pop sd:", left_z95_kn, right_z95_kn)

#My output:
# - "Z-distribution w/known pop sd: -0.552220378018745 -0.263073739628314"

#As the Pop_d(i.e. the difference in proportions of the two populations under the null hypothesis) is 0, this
#is outside of the confidence interval and is therefore significant.

