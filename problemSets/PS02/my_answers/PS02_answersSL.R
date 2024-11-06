#Problem Set 2
#Question 1: Political Science
##a. calculate chi-square test statistic
#sum of (number of observances-expected number (independent samples)^2/number expected
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

up_not
up_bribe
up_stop
low_not
low_bribe
low_stop

##chi-square stat
chi_square<-((14-up_not)^2/up_not)+
  ((6-up_bribe)^2/up_bribe)+
  ((7-up_stop)^2/up_stop)+
  ((7-low_not)^2/low_not)+((7-low_bribe)^2/low_bribe)+
  ((1-low_stop)^2/low_stop)
chi_square

##b. calculate p for chi-square
pchisq(chi_square, df=1*2, lower.tail = FALSE)
###larger than 0.1 (0.15) therefore not statistically significant result, 
#evidence H0 variable are independent

##c. standardized residuals
## (fobserved-fexpected)/standard error: 
#(sqrt(fexpected*(1-row proportion)*(1- column proportion))
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

#2. Economics
##b. bivariate regression in R
women_df<-read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
lm(water~reserved, data = women_df)
