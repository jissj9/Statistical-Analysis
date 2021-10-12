#Reading in the Lab1.csv dataset 
Lab1_true<-read.csv(file="Lab1.csv", header=TRUE)

#Summary statistics for the variable EARN
summary(Lab1_true$EARN)

#Displaying frequencies of variable Job.class
ftable(Lab1_true$Job.class)

#Three-way cross-tabulation of the proportions of variables Educational Level, Gender and Job.Class
ftable(Lab1_true$EDUC, Lab1_true$Gender, Lab1_true$Job.class) 

#Histogrmam of Variable Earn
hist(Lab1_true$EARN)

#Creating a basic boxplot of the variable EARN by Job Class
boxplot(Lab1_true$EARN~Lab1_true$Job.class)

#Creating a new variable EARNx10000 that is equal to Earnings divided by 10,000
Lab1_true$EARNx10000 = Lab1_true$EARN/10000

#Create a scatterplot with EARNx10000 on the x axis and AGE on the Y axis
plot(Lab1_true$EARNx10000, Lab1_true$AGE) 