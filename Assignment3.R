
#Q1.Simulate a population of 10,000 with a variable from a Normal(4,5) distribution and a population 
#of 10,000 with a variable from an Exponential(1) distribution using the command rexp(10000,1) 

#Creating a vector 'normalDistribution' of length 10000, drawn from a normal distribution with mean 4, sd 5.
normalDistribution <- rnorm(10000,4,5)

#Creating a vector 'exponentialDistribution' of length 10000, drawn from a exponential distribution with lambda 1.
exponentialDistribution <- rexp(10000,1)

#Q2.Draw graphs to assess normality of each population as well as samples of size 10, 50 and 500 from each population.

#Making a qqplot and a histogram with normal density curve for 'normalDistribution'.
qqnorm(normalDistribution)
qqline(normalDistribution)
hist(normalDistribution, freq = FALSE)
xfit <- seq(min(normalDistribution), max(normalDistribution), length = 40) 
yfit <- dnorm(xfit, mean = mean(normalDistribution), sd = sd(normalDistribution))
lines(xfit, yfit)

#Making a qqplot and a histogram with normal density curve for 'exponentialDistribution'.
qqnorm(exponentialDistribution)
qqline(exponentialDistribution)
hist(exponentialDistribution, freq = FALSE)
xfit <- seq(min(exponentialDistribution), max(exponentialDistribution), length = 40) 
yfit <- dnorm(xfit, mean = mean(exponentialDistribution), sd = sd(exponentialDistribution))
lines(xfit, yfit)

#Taking a random sample of size 10 from 'normalDistribution'
normalSample10 <- sample(normalDistribution,10)
#Making a qqplot and a histogram with normal density curve for 'normalSample10'.
qqnorm(normalSample10)
qqline(normalSample10)
hist(normalSample10, freq = FALSE)
xfit <- seq(min(normalSample10), max(normalSample10), length = 40) 
yfit <- dnorm(xfit, mean = mean(normalSample10), sd = sd(normalSample10))
lines(xfit, yfit)

#Taking a random sample of size 50 from 'normalDistribution'
normalSample50 <- sample(normalDistribution,50)
#Making a qqplot and a histogram with normal density curve for 'normalSample50'.
qqnorm(normalSample50)
qqline(normalSample50)
hist(normalSample50, freq = FALSE)
xfit <- seq(min(normalSample50), max(normalSample50), length = 40) 
yfit <- dnorm(xfit, mean = mean(normalSample50), sd = sd(normalSample50))
lines(xfit, yfit)

#Taking a random sample of size 500 from 'normalDistribution'
normalSample500 <- sample(normalDistribution,500)
#Making a qqplot and a histogram with normal density curve for 'normalSample500'.
qqnorm(normalSample500)
qqline(normalSample500)
hist(normalSample500, freq = FALSE)
xfit <- seq(min(normalSample500), max(normalSample500), length = 40) 
yfit <- dnorm(xfit, mean = mean(normalSample500), sd = sd(normalSample500))
lines(xfit, yfit)

#Taking a random sample of size 10 from 'exponentialDistribution'
exponentialSample10 <- sample(exponentialDistribution,10)
#Making a qqplot and a histogram with normal density curve for 'exponentialSample10'.
qqnorm(exponentialSample10)
qqline(exponentialSample10)
hist(exponentialSample10, freq = FALSE)
xfit <- seq(min(exponentialSample10), max(exponentialSample10), length = 40) 
yfit <- dnorm(xfit, mean = mean(exponentialSample10), sd = sd(exponentialSample10))
lines(xfit, yfit)

#Taking a random sample of size 50 from 'exponentialDistribution'
exponentialSample50 <- sample(exponentialDistribution,50)
#Making a qqplot and a histogram with normal density curve for 'exponentialSample50'.
qqnorm(exponentialSample50)
qqline(exponentialSample50)
hist(exponentialSample50, freq = FALSE)
xfit <- seq(min(exponentialSample50), max(exponentialSample50), length = 40) 
yfit <- dnorm(xfit, mean = mean(exponentialSample50), sd = sd(exponentialSample50))
lines(xfit, yfit)


#Taking a random sample of size 500 from 'exponentialDistribution'
exponentialSample500 <- sample(exponentialDistribution,500)
#Making a qqplot and a histogram with normal density curve for 'exponentialSample500'.
qqnorm(exponentialSample500)
qqline(exponentialSample500)
hist(exponentialSample500, freq = FALSE)
xfit <- seq(min(exponentialSample500), max(exponentialSample500), length = 40) 
yfit <- dnorm(xfit, mean = mean(exponentialSample500), sd = sd(exponentialSample500))
lines(xfit, yfit)

#Q3.Calculate confidence intervals for the mean of the sample of size 50 from each distribution, using
# a) the Z-score and the known population SD, and 
# b) the t-distribution and the sample SD.

#Recording the size, mean and sd of 'normalDistribution'
N_norm <- length(normalDistribution)
mean_norm <- mean(normalDistribution)
sd_norm <- sd(normalDistribution)

#Recording the size, mean and sd of 'normalSample50'
n_N50 <- length(normalSample50)
mean_N50 <- mean(normalSample50)
sd_N50 <- sd(normalSample50)

#Calculating the standard error of mean_N50 (i.e mean of normalSample50) using known and estimated 
#sd (known sd = sd_norm, unknown sd = sd sd_N50)
se_kn_N <- sd_norm/sqrt(n_N50)
se_unkn_N <- sd_N50/sqrt(n_N50)

#Calculating Z-score for 95% CI
Z_score_N <- qnorm(0.975)

#Calculate critical t-value for 95% CI and x1
t_score_N <- qt(0.975, n_N50-1)

#Calculate 95% CIs for normalSample50 using known and unknown population sd, and t and z distributions
left_Z95_kn_N <- mean_N50-Z_score_N*se_kn_N
right_z95_kn_N <- mean_N50+Z_score_N*se_kn_N

left_t95_unkn_N <- mean_N50-t_score_N*se_unkn_N
right_t95_unkn_N <- mean_N50+t_score_N*se_unkn_N

#Showing results
paste("var: normalDistribution ", "mean:", mean_norm, "sd:", sd_norm)
paste("var: normalSample50", "mean:", mean_N50, "sd:", sd_N50, "se w/known pop sd:", se_kn_N, "se w/unknown pop sd:", se_unkn_N)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn_N, right_z95_kn_N)
paste("t-distribution w/unknown pop sd:", left_t95_unkn_N, right_t95_unkn_N)

#Recording the size, mean and sd of 'exponentialDistribution'
N_expo <- length(exponentialDistribution)
mean_expo <- mean(exponentialDistribution)
sd_expo <- sd(exponentialDistribution)

#Recording the size, mean and sd of 'exponentialSample50'
n_E50 <- length(exponentialSample50)
mean_E50 <- mean(exponentialSample50)
sd_E50 <- sd(exponentialSample50)

#Calculating the standard error of 'n_E50' using known and estimated sd_expo(ie the mean of the
#'exponentialSample50') using known and estimated sd(known sd = sd_expo, unknown sd = sd_E50)
se_kn_E <- sd_expo/sqrt(n_E50)
se_unkn_E <- sd_E50/sqrt(n_E50)

#Calculate Z-score for 95% CI
Z_score_E <- qnorm(0.975)

#Calculate critical t-value for 95% CI and 'exponentialSample50'
t_score_E <- qt(0.975, n_E50-1)

#Calculate 95% CIs for 'exponentialSample50' using known and unknown population sd, and t and z distributions
left_Z95_kn_E <- mean_E50-Z_score_E*se_kn_E
right_z95_kn_E <- mean_E50+Z_score_E*se_kn_E

left_t95_unkn_E <- mean_E50-t_score_E*se_unkn_E
right_t95_unkn_E <- mean_E50+t_score_E*se_unkn_E

#Showing results
paste("var: exponentialDistribution", "mean:", mean_expo, "sd:", sd_expo)
paste("var: exponentialSample50", "mean:", mean_E50, "sd:", sd_E50, "se w/known pop sd:", se_kn_E, "se w/unknown pop sd:", se_unkn_E)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn_E, right_z95_kn_E)
paste("t-distribution w/unknown pop sd:", left_t95_unkn_E, right_t95_unkn_E)





