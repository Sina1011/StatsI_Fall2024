install.packages("car")
library(car)
data(Prestige)
help(Prestige)

# Question 1: Economics
##a. new variable "professional": recode type professionals = 1, blue + white collar = 0
Prestige$professional<-ifelse(Prestige$type=="prof", 1, 0)

##b. linear regression
prestige_interact <- lm(prestige ~ income + professional + income:professional, 
                        data=Prestige)
summary(prestige_interact)

stargazer(prestige_interact)

##c. equation
#Y = 21.142 + 0.003 * x_1 + 37.781 * x_2 - 0.002 * x_3
#prestige = 21.142 + 0.003 * income + 37.781 * professional - 0.002 * income:professional

##d.interpret income coefficient
# 1 unit increase in income leads to 0.003 unit increase in prestige, in reference category professionals

##e. interpret professional coefficient

##f. effect $1000 increase

#effect: 
#prestige_initial = 21.142 + 0.003 * income + 37.781 * 1 - 0.002 * (income*1)
#prestige_new = 21.142 + 0.003 * (income+1000) + 37.781 * professional - 0.002 * (income+1000)*1
#=21.142 + 0.003 * income + 3 + 37.781 - 0.002*income - 2

#initial = 21.142 + 37.781 + 0.003 * income - 0.002 * income
#new = 21.142 + 37.781 - 2 + 3 + 0.003 * income - 0.002 * income

#subtracted 
# -2 + 3 = 1

##g. effect different profession
#non-professional: prestige = 21.142 + 0.003 * 6000 + 37.781 * 0 - 0.002 * 6000*0
#professional : prestige = 21.142 + 0.003 * 6000 + 37.781 * 1 - 0.002 * 6000*1

#subtracted
#-37.781+12 
#-25.781


#Question 2
#a. hypothesis test assigned lawn signs
#H0 = having these yard signs in a precinct does not affect vote share
#H1 = having these yard signs in a precinct affects vote share --> two-tailed

#t-values 
#t*_yes = coefficient/SE = 0.042/0.016
t_yes <- 0.042/0.016

#degrees of freedom
# n-1-1
df <- 131-2-1

#p-values
#yard signs
2*pt(abs(t_yes), df = df, lower.tail=F)
#0.00972002 which is less than 0.05 so reject H0

#b. hypothesis test lawn sign near
#H0 = having these yard signs next to a precinct does not affect vote share
#H1 = having these yard signs next to a precinct affects vote share --> two-tailed

#t*_no = coefficient/SE = 0.042/0.013
t_no <- 0.042/0.013

#no yard signs
2*pt(abs(t_no), df = df, lower.tail=F)
#0.0015 which is less than 0.05 so reject H0

#c. interpret constant coefficient

#d. model fit 
#R^2 correlation coefficient is fairly small at 0.094, which implies that there 
#most is explained by other factors