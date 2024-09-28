#Question 1: Education
#remove objects
rm(list=ls())
##load dataset
dataset_edu<-c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

##1. calculate confidence interval 
length(dataset_edu) #=25

t_score <- qt(0.95, df=25-1)
conf_high <- mean(dataset_edu)+(t_score)*(sd(dataset_edu)/sqrt(25))
conf_low <- mean(dataset_edu)-(t_score)*(sd(dataset_edu)/sqrt(25))

conf_high
conf_low

###90% confidence interval: [93.96; 102.92]

##2. is the average IQ in the school higher than the average of all schools 

###H1: higher than average
###H0: lower than or equal to average
###--> one-sided test

t.test(dataset_edu, mu = 100, alternative = "greater")
###p-value = 0.7215, therefore we reject our H1


#Question 2: Political Economy
##set working directory
setwd("/Users/sinalangenscheidt/Documents/Applied Statistical Anaylsis I/GitHub/
      StatsI_Fall2024/datasets")

##load dataset
expenditure<-read.delim("expenditure.txt")

##1. relationships between X1, X2, X3 and Y
##create 3x2 grid
par(mfrow = c(3, 2))

#plot every relationship in grid
plot(expenditure$X1, expenditure$Y,  main = "Relationship between X1 and Y",  
     xlab = "per capita personal income in state", 
     ylab = "/capita expenditure shelters/housing assistance")
plot(expenditure$X2, expenditure$Y,  main = "Relationship between X2 and Y",  
     xlab = "number residents/100,000 ”financially insecure” in state", 
     ylab = "/capita expenditure shelters/housing assistance")
plot(expenditure$X3, expenditure$Y,  main = "Relationship between X3 and Y",  
     xlab = "number people/1000 residing in urban areas in state", 
     ylab = "/capita expenditure shelters/housing assistance")
plot(expenditure$X1, expenditure$X2, main = "Relationship between X1 and X2", 
     xlab = "per capita personal income in state", 
     ylab = "number residents/100,000 ”financially insecure”")
plot(expenditure$X1, expenditure$X3, main = "Relationship between X1 and X3", 
     xlab = "per capita personal income in state", 
     ylab = "number people/1000 residing in urban areas")
plot(expenditure$X2, expenditure$X3, main = "Relationship between X2 and X3", 
     xlab = "number residents/100,000 ”financially insecure”", 
     ylab = "number people/1000 residing in urban areas")

#2. relationship between Region and Y
boxplot(Y ~ Region, data = expenditure,
        xlab="Region",
        ylab="per capita expenditure on shelters/housing assistance in state",
        main="Relationship between Region and Y")

#3. relationship X1, Y and Region
##relationship X1 and Y
plot(expenditure$X1,expenditure$Y, 
     xlab = "per capita personal income in state",
     ylab = "per capita expenditure on shelters/housing assistance in state", 
     main = "Relationship between X1 and Y") 

##adding column "Region" with legend
plot(expenditure$X1, expenditure$Y,  col = c("blue","red", "green", "purple"),
     pch = c(0,1,2,3),
     main= "Relationship between X1, Region and Y",
     xlab="per capita personal income in state",
     ylab="per capita expenditure on shelters/housing assistance in state")
     
legend(1000,130, legend=c("Northeast", "North Central", "South", "West"),
            col = c("blue","red", "green", "purple"),
            pch= c(0, 1,2,3))