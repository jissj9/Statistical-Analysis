

tbl <- table(survey$Smoke, survey$Exer)
chisq.test(tbl)

# 
#   1)	How were the degrees of freedom for this test calculated?

#       Degrees of freedom is calculated using the formula:
#       df = (r-1)(c-1) (where r is the number of rows and c is the number of columns)
#       df = (number of rows - 1)(number of columns - 1)
#       Smoke - 4 rows: Never, Occas, Regul, Heavy
#       Exer - 3 columns: None, Some, Freq
#       df = (4 - 1)*(3 - 1)  
#       df = 3*2
#       df = 6

#   2)	What are the assumptions of the chi square test?

#       The general assumptions of the Chi-Square test are:
#        1. All the observations must contribute to only one cell
#        2. In a 2x2 table, all the expected values must be greater than 5
#        3. In a larger table - the sample size must be greater than 20 
#                             - all the expected values must be greataer than 1 and no more than 20% of the expected
#                               values can be less than 5.
#          
         

#   3)	Why did the code give an error message?

#       The error message is given as a warning that the results may not be accurate as more than 20% of the expected 
#       values are less than 5. In addition some of the expected values are not greater than 1. These were the two of
#       the assumptions for a Chi-Square test to be accurate.
      
#      Two of the expected values are not greater than 1 which suggests that the sample size is not larger enough to 
#      conduct an accurate Chi-Square test. Nearly half which is 5 out of 12 of the expected values are less than 5.
#      The sample size is too small to make an accurate judgement of the association/relationship betweent the two 
#      vaiables. Using Fisher's exact Test may be more appropriate when dealing with small cell sizes as this one. 
#      For example (fisher.test(tbl)).
#


#   4)	What is the null hypothesis for this test and the alternative hypothesis for this test?

#       Null hypothesis(H0) - There is no association between how frequently someone smokes and the amount of exercise 
#       they do.
#       Alternative Hypothesis(H1)- There is an association between how frequently someone smokes and the amount of 
#       exercise they do.
#


#   5)	Using a significance level of 0.05, give your conclusions for the result of this test.

#       p-value = 0.4828(From Pearson's Chi-Squared Test)
#       As the p-value given by the Chi-Squared test of independenceis greater than the chosen significance level
#       (i.e p > 0.05), the result is not significant and we fail to reject the null hypothesis - which is that there is 
#       no association between how often someone smokes and the amount of exercise they do. As stated previously, this 
#       result may be inaccurate due to the smaple size that is being used. Chi-Squared tests are sensitive to sample 
#       size. A reasonably strong association may not become apparent if the sample size is relatively too small.
#       This may be what happens here.
        
        
        