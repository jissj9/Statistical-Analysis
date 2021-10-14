##code for z-test
#Definitions:
#let var mu be the mean of the population under the null hypothesis
#let var n be the sample size
#let var x_bar be the mean of the sample
#let var sd be the known population sd , or our estimate of it from the sample
#let alpha be the significance level eg 0.05
#let string var tails be "two" "left" or "right" indicating a two-tailed test or direction of one-tailed

OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig )
  #return the list
  return( ret )
}



#Q1
#	H0: the mean of the population is equal to 0. 
# HA: the mean of the population is not 
#     equal to 0. Use a Z-test with the true population SD. 
#     Using alpha = 0.05


#Creating a vector 'normalDistribution' of length 100, drawn from a normal distribution with mean 4, sd 5.
normalDistribution <- rnorm(100,4,5)
#Drawing a histogram to visualize distribution
hist(normalDistribution)

#Recording the size(n), mean(x_bar) and sd of 'normalDistribution'
n <- length(normalDistribution)
x_bar <- mean(normalDistribution)
sd <- 5

#Assigning values to 'alpha'(significance level) as 0.05 and the mean of the population under null hypothesis as 0
alpha <- 0.05
mu <- 0
 
#Calling on the function OneSampleTest
#'z'- To perform a z test 
#'two'- To perform a two tail test 
#'alpha'- Use a 0.05 alpha level/significance level
#'mu'- The mean of the population under the null hypothesis
#'n'- The sample size
#'x_bar'- The mean of the sample
#'sd'- True population standard deviation
OneSampTest("z", "two", alpha, mu, n, x_bar, sd)
#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - type of test(type), 
#   - mean of the population under the null hypothesis(mu), 
#   - size of the sample(n), 
#   - mean of the sample(x_bar), 
#   - standard error(se), 
#   - p-value(p), 
#   - alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance)

#My output for my normalDistribution was(the output changes but for one of my outputs):
#   - type = "One sample z test, two tailed"
#   - mu = 0
#   - n = 100
#   - x_bar = 3.842252
#   - se = 0.5
#   - p = 1.535902e-14
#   - alpha = 0.05
#   - significance = "significant"

#Calculating the test statistic outside of thr function as it is not returned and would like to interpret it.
test_stat <- (x_bar-mu)/(sd/sqrt(n))

#Test statistic = 7.68450458238048 (this test statistic compares how different thesample mean(x_bar) is from the mean
#                 under the null hypothesis(mu). If they were both equal(i.e. if the sample mean and null hypothesis
#                 means were equal), it would imply that there is no difference between them(i.e. the test statistic
#                 would be 0). This would result in the p-value being the maximum(i.e. 1) due to the p-value being 
#                 based off the z-score of the test statistic. In this case however, the sample mean(x_bar) and mean 
#                 under the null hypothesis(H0) differ by a large degree, meaning the test statistic is quite large. 
#                 Hence, the corresponding p-value will be quite small.)

#p-value = 1.535902e-14 (due to the larger test statistic mentioned above, the p-value is quite small. It is smaller 
#          than 0.05, meaning the result is significant and hence the null hypothesis can be rejected and the 
#          alternative hypothesis accepted)

#Conclusion:As the result is significant(i.e. p < 0.05), the null hypothesis(H0) is rejected and the alternative 
#           hypothesis(HA) is accepted - that the mean of the population is not equal to 0





#Q2
# H0: the mean of the population is greater than or equal to 4.2. 
# HA: the mean of the population is less than 4.2. 
#     Use a t-test with the sample SD. 
#     Use alpha = 0.025

#Using the vector already created above 'normalDistribution'
#Recording the size(n), mean(x_bar) and sd of 'normalDistribution'
n <- length(normalDistribution)
x_bar <- mean(normalDistribution)
sd <- sd(normalDistribution)

#Assigning values to 'alpha'(significance level) as 0.05 and the mean of the population under null hypothesis as 0
alpha <- 0.025
mu <- 0

#Calling on the function OneSampleTest
#'t'- To perform a t test 
#"left" - To perform a one-tailed test(left tail)
#'alpha'- Use a 0.025 alpha level/significance level
#'mu'- The mean of the population under the null hypothesis
#'n'- The sample size
#'x_bar'- The mean of the sample
#'sd'- Sample standard deviation
OneSampTest("t", "left", alpha, mu, n, x_bar, sd)

#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - type of test(type), 
#   - mean of the population under the null hypothesis(mu), 
#   - size of the sample(n), 
#   - mean of the sample(x_bar), 
#   - standard error(se), 
#   - p-value(p), 
#   - alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance) 

#My output for my normalDistribution was:
#   - type = "One sample t test, left tailed"
#   - mu = 4.2
#   - n = 100
#   - x_bar = 3.834226
#   - se = 0.5173569
#   - p = 0.240612
#   - alpha = 0.025
#   - significance = "not significant"

#Calculate the test statistic outside of the function as it is not returned 
#and would like to interpret it.
test_stat <- (x_bar-mu)/(sd/sqrt(n))

#Test statistic = -0.707005358230935(this test statistic compares how different the sample mean(x_bar) is from the 
#                 mean under the null hypothesis(mu). If they were both equal(i.e. if the sample 
#                 mean and null hypothesis means were equal), it would imply that there is no 
#                 difference between them(i.e. the test statistic would be 0). This would result
#                 in the p-value being the maximum(i.e. 1) due to the p-value being based off the
#                 z-score of the test statistic. In this case however, the sample mean(x_bar) and
#                 mean under the null hypothesis(H0) differ by a small degree, meaning the test 
#                 statistic is also quite small. Hence, the corresponding p-value will be quite large.)

#p-value = 0.240612 (due to the small test statistic mentioned above, the p-value is quite large. It is larger 
#                    than 0.05, meaning the result is not significant and hence we fail to reject the null hypothesis)

#Conclusion:As the result is not significant(i.e. p > 0.05), we fail to reject the null hypothesis(H0) - that the 
#           mean of the population is greater than or equal to 4.2





#Q3
#3)	Repeat the previous task using the t.test function in the package {stats}
t.test(x = normalDistribution, alternative = "less", mu = mu)

#My output from t.test function:
#   - t = -0.70701
#   - df = 99
#   - p-value = 0.2406
#   - mean of x = 3.834226
# These values for the test statistic(t) and p-value correspond to the previous 
# output generated by the function used previuosly on the same sample, both functions 
# lead to the same conclusion - that the result is not significant and we fail to reject 
# the null hypothesis.# Repeat the previous task using the t.test function in the package {stats}







#Q4
#  	H0: the population proportion is equal to 0.28.
#   HA: the population proportion is not equal to 0.28. 
#   Use alpha = 0.05

##Simulate a vector containing the results of 100 independent Bernouli trials, 
#each with a probability of success of 0.3.
bernouliDistribution <- rbern(100,0.3)
#Draw histogram to visualise distribution
hist(bernouliDistribution)

#Record the size, mean and sd of bernouliDistribution
n <- length(bernouliDistribution)
x_bar <- mean(bernouliDistribution)
sd <- sqrt((0.3)*(1-0.3))

#Assign significance level(alpha) and proportion of the population under the null 
#hypothesis to variables(both given in question)
alpha <- 0.05
mu <- 0.28

#Call the OneSampTestFunction, passing in the following parameters;
#   "z" - perform a z-test
#   "two" - perform a two-tailed test
#   alpha - use a 0.025 alpha level/significance level
#   mu - proportion of the population under the null hypothesis
#   n - size of the sample
#   x_bar - proportion of the sample
#   sd - sample standard deviation
OneSampTest("z","two",alpha,mu,n,x_bar,sd)
#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - type of test(type), 
#   - mean of the population under the null hypothesis(mu), 
#   - size of the sample(n), 
#   - mean of the sample(x_bar), 
#   - standard error(se), 
#   - p-value(p), 
#   - alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance) 

#My output for my normalDistribution was:
#   - type = "One sample z test, two tailed"
#   - mu = 0.28
#   - n = 100
#   - x_bar = 0.31
#   - se = 0.04582576
#   - p = 0.5126908
#   - alpha = 0.05
#   - significance = "not significant"

#Calculate the test statistic outside of the function as it is not returned 
#and would like to interpret it.
test_stat <- (x_bar-mu)/(sd/sqrt(n))

#Test statistic = 0.654653670707977(this test statistic compares how different the sample mean(x_bar) is from the 
#                 mean under the null hypothesis(mu). If they were both equal(i.e. if the sample 
#                 mean and null hypothesis means were equal), it would imply that there is no 
#                 difference between them(i.e. the test statistic would be 0). This would result
#                 in the p-value being the maximum(i.e. 1) due to the p-value being based off the
#                 z-score of the test statistic. In this case however, the sample mean(x_bar) and
#                 mean under the null hypothesis(H0) differ by a small degree, meaning the test 
#                 statistic is also quite small. Hence, the corresponding p-value will be quite large.) 

#p-value = 0.5126908(due to the small test statistic mentioned above, the p-value is quite large. It is larger 
#                    than 0.05, meaning the result is not significant and hence we fail to reject the null hypothesis)

#Conclusion:As the result is not significant(i.e. p > 0.05), we fail to reject the null hypothesis(H0) - that 
#           the population proportion is equal to 0.28 






#Q5
# 	H0: the population proportion is less than or equal to 0.35. 
#   HA: the population proportion is greater than 0.35. 
#   Use alpha = 0.05

#Assign significance level(alpha) and proportion of the population under the null 
#hypothesis to variables(both given in question)
alpha <- 0.05
mu <- 0.35

#Call the OneSampTestFunction, passing in the following parameters;
#   "z" - perform a z-test
#   "right" - perform a one-tailed test(right tail)
#   alpha - use a 0.05 alpha level/significance level
#   mu - proportion of the population under the null hypothesis
#   n - size of the sample
#   x_bar - proportion of the sample
#   sd - sample standard deviation
OneSampTest("z","right",alpha,mu,n,x_bar,sd)
#This function returns the results of the specified test with respect to the 
#parameters mentioned above and prints out on the console the various results for:
#   - type of test(type), 
#   - mean of the population under the null hypothesis(mu), 
#   - size of the sample(n), 
#   - mean of the sample(x_bar), 
#   - standard error(se), 
#   - p-value(p), 
#   - alpha level/significance level(alpha) and 
#   - whether the result of the test is significant or not(significance) 

#My output for my normalDistribution was:
#   - type = "One sample z test, right tailed"
#   - mu = 0.35
#   - n = 100#and would like to interpret it.
test_stat <- (x_bar-mu)/(sd/sqrt(n))
#   - x_bar = 0.31
#   - se = 0.04582576
#   - p = 0.8086335
#   - alpha = 0.05
#   - significance = "not significant"

#Calculate the test statistic outside of the function as it is not returned 

#Test statistic = -0.872871560943969(this test statistic compares how different the sample mean(x_bar) is from the 
#                 mean under the null hypothesis(mu). If they were both equal(i.e. if the sample 
#                 mean and null hypothesis means were equal), it would imply that there is no 
#                 difference between them(i.e. the test statistic would be 0). This would result
#                 in the p-value being the maximum(i.e. 1) due to the p-value being based off the
#                 z-score of the test statistic. In this case however, the sample mean(x_bar) and
#                 mean under the null hypothesis(H0) differ by a small degree, meaning the test 
#                 statistic is also quite small. Hence, the corresponding p-value will be quite large.) 

#p-value = 0.8086335(due to the small test statistic mentioned above, the p-value is quite large. It is larger 
#                     than 0.05, meaning the result is not significant and hence we fail to reject the null hypothesis)

#Conclusion:As the result is not significant(i.e. p > 0.05), we fail to reject the null hypothesis(H0) - that 
#           the population proportion is less than or equal to 0.35.
   
