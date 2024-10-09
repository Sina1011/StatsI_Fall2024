#Problem Set 2
#Question 1: Political Science
##a. calculate chi-square test statistic
#sum of (number of observances - number of expectance if fully independent samples)^2/number expected
#calculate f1e = expected if independent for upper class not stopped
#row total/grand total * column total

row_1_total<-14+6+7
row_2_total<-7+7+1
column_1_total<- 14+7
column_2_total<- 6+7
column_3_total<- 7+1
total<- 14+6+7+7+7+1

up_not<-row_1_total/total*column_1_total
up_bribe<-row_1_total/total*column_2_total
up_stop<-row_1_total/total*column_3_total
low_not<-row_2_total/total*column_1_total
low_bribe<-row_2_total/total*column_2_total
low_stop<-row_2_total/total*column_3_total

##chi-square stat
chi_square<-((14-up_not)^2/up_not)+
  ((6-up_bribe)^2/up_bribe)+
  ((7-up_stop)^2/up_stop)+
  ((7-low_not)^2/low_not)+((7-low_bribe)^2/low_bribe)+
  ((1-low_stop)^2/low_stop)
#3.79

##b. calculate p for chi-square
pchisq(chi_square, df=1*2, lower.tail = FALSE)
###larger than 0.1 (0.15) therefore not statistically significant result, evidence H0 variable are independent
#we do not have sufficient evidence to reject our (implicit) null-hypothesis that officers are not more or less likely to solicit
#a bribe from drivers depending on their class. There is not sufficient evidence that officers are more or less likely to..
#there is no association between the class of the driver and whether or not an officer solicits a bribe (i.e., the variables are independent)

##c. standardized residuals
## (fobserved-fexpected)/standard error (sqrt((1-row proportion)*(1- column proportion))
row_1_prop<-row_1_total/total
row_2_prop<-row_2_total/total
column_1_prop<-column_1_total/total
column_2_prop<-column_2_total/total
column_3_prop<-column_3_total/total

res_up_not <- (14-up_not)/sqrt(up_not*(1-row_1_prop)*(1-column_1_prop)) #0.32
res_up_bribe <- (6-up_bribe)/sqrt(up_bribe*(1-row_1_prop)*(1-column_2_prop)) #-1.64
res_up_stop <- (7-up_stop)/sqrt(up_stop*(1-row_1_prop)*(1-column_3_prop)) #1.52
res_low_not <- (7-low_not)/sqrt(low_not*(1-row_2_prop)*(1-column_1_prop)) #-0.32
res_low_bribe <- (7-low_bribe)/sqrt(low_bribe*(1-row_2_prop)*(1-column_2_prop)) #1.64
res_low_stop <- (1-low_stop)/sqrt(low_stop*(1-row_2_prop)*(1-column_3_prop)) #-1.52

res_up_not  # Adjusted residual for up class not stopped
res_up_bribe  # Adjusted residual for up class solicited for bribe
res_up_stop  # Adjusted residual for up class stopped
res_low_not  # Adjusted residual for low class not stopped
res_low_bribe  # Adjusted residual for low class solicited for bribe
res_low_stop  # Adjusted residual for low class stopped

##d. interpretation
#shows me how far away is each observed value from "expectation"
#None of the standardized residuals exceed the threshold of 
#±1.96, meaning there are no statistically significant deviations in any of the cells. 
#The observed and expected counts are generally close, implying that class does not seem to have a significant impact on whether officers solicit bribes or stop drivers.
#The residuals reinforce your earlier conclusion from the chi-square test: there's no strong evidence that the officers’ behavior (stopping or soliciting bribes) is influenced by the class of the drivers.
#good model

#2. Economics
##b. bivariate regression in R
women_df<-read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
lm(water~reserved, data = women_df)

#This coefficient specifically represents the estimated effect of the reservation policy (i.e., having a Gram Panchayat reserved for women) 
#on the number of new or repaired drinking water facilities in the villages. 
#In this case, it means that villages with the reservation policy are expected to have, on average, 9.252 more new or repaired drinking water facilities 
#compared to villages without the reservation.
#The reservation policy is associated with an average increase of 9.252 new or repaired drinking water facilities in villages 
#where the GP is reserved for women, compared to those where it is not.
