getwd()

setwd("F:\\Analytix_Labs\\R for Data Science\\R Case Studies\\R - Credit card case study\\R case study 2 (Credit card)")


customer_acq <-read.csv("Customer Acqusition.csv")
repayment <- read.csv("Repayment.csv")
spend <- read.csv("spend.csv")

names(cusp)[2] <- "Customer"
names(cusp)[4] <- "Month"
names(cusp)[12] <- "Amount"

library(Hmisc)

Hmisc::describe(customer_acq)
head(customer_acq)
tail(customer_acq)
names(cusp2)
Hmisc::describe(repayment)
head(repayment)
tail(repayment)

Hmisc::describe(spend)
head(spend)
tail(spend)

#1. In the above dataset 
#a. Incase age is less than 18, replace it with mean of age values.

head(customer_acq)

mean(customer_acq$Age)
customer_acq$Age[customer_acq$Age < 18] <- mean(customer_acq$Age)

#A<-mean(customer_acq$Age)

#is.data.frame(customer_acq<-replace(customer_acq$Age, customer_acq$Age<=18, A))
customer_acq
head(customer_acq)

#b Incase spend amount is more than the limit, replace it with 50% of that customer's limit.
#(customer's limit provided in acquisition table is the per transaction limit on his card)


head(spend)
head(customer_acq)

library(dplyr)
#inner_join()

cusp <- merge.data.frame(x = customer_acq,y = spend,by.x ="No",by.y = "Sl.No.",all.x = TRUE )
view(cusp)

data.frame(cusp$Limit > cusp$Amount)

#limit1<-data.frame(cusp$Limit/2)

library(dplyr)
#case_when(cusp$Limit > cusp$Amount) 

#cusp[cusp['Limit'] >cusp['spend']]


#which(cusp$Amount>cusp$Limit)

#cusp<-ifelse(cusp$Limit>cusp$Amount,((50 * cusp$Limit)/(100)),cusp$Amount)
#library(dplyr)
#which(cusp['Amount'] > cusp['Limit'])

#if(cusp[, c('Amount')])> cusp[,c('Limit')]){
#       cusp[,c('Amount')] <- (50 * cusp$Limit)/100
#  }

  
  
  
#c Incase the repayment amount is more than the limit, replace the repayment with the limit  

#replace(cusp$Amount,cusp$)


for (i in 1:nrow(cusp)) {
  
  if (cusp$Amount[i]>=cusp$Limit)
    cusp$Limit[i]= cusp$Amount[i]}

#2. From the above dataset create the following summaries:
#a. How many distinct customers exist? 

unique(cusp[c("Customer.x")])
#b. How many distinct categories exist?
table(cusp$Product)
#c. What is the average monthly spend by customers? 
mean(cusp$Amount)
#d. What is the average monthly repayment by customers? 

mean(x = repayment$Amount,na.rm = TRUE)
class(repayment$Amount)
typeof(repayment$Amount)

#e. If the monthly rate of interest is 2.9%, 
#what is the profit for the bank for each month? (Profit is defined as interest earned on Monthly Profit.
#Monthly Profit = Monthly repayment - Monthly spend. Interest is earned only on positive profits and not
#on negative amounts)


Monthly_profit <- mean(x = repayment$Amount,na.rm = TRUE)- mean(x = spend$Amount)

Monthly_profit1<- (Monthly_profit*30*2.9)/100

Monthly_profit1


#f. What are the top 5 product types?
#top_n(customer_acq$Product,n = 5)
head(sort(customer_acq$Product,decreasing = TRUE),n = 5)

#g. Which city is having maximum spend?

cusp %>% group_by(City) %>% arrange(desc(Amount))

  library(dplyr)

#h. Which age group is spending more money? 
cusp%>% group_by(Age)%>%arrange(desc(Amount))


#i. Who are the top 10 customers in terms of repayment?

cusp1 <- merge.data.frame(x = customer_acq,y = repayment,by.x = "No",by.y = "SL.No.")



cusp1%>% head(sort(cusp1$Amount,decreasing = TRUE),n = 10)%>% group_by(Customer.x)

cusp1

#3. Calculate the city wise spend on each product on yearly basis.
#Also include a graphical representation for the same.
cusp$Month <- as.Date(x = cusp$Month,"%d-%b-%y")

cusp$Year <- format(cusp$Month, format="%y")
library(dplyr)

plot(cusp %>% group_by(City,Product)%>%group_by(Month))
class(cusp$Month)



#4. Create graphs for
#a. Monthly comparison of total spends, city wise

lines(cusp$Amount, col="red")
matplot(obs_v_exp, type='l')

#b. Comparison of yearly spend on air tickets
library(tidyverse)


ggplot(data = cusp,mapping = aes(x = Amount,y =Year ))

#c. Comparison of monthly spend for each product (look for any seasonality that exists in terms of spend)


plot(cusp%>% group_by(Month,Product))


#5. Write user defined R function to perform the following analysis: 
#You need to find top 10 customers for each city in terms of their repayment amount by
#different products and by different time periods i.e. year or month. The user should be able
#to specify the product (Gold/Silver/Platinum) and time period (yearly or monthly) and the function 
#should automatically take these inputs while identifying the top 10 customers.



library(lubridate)
Take_input=function()
{
 Year1 =readline("Enter year:")
  Month1 =readline("Enter month:")
  Product1 =readline("Enter the Product:")
  filter_1=filter(cusp,lubridate::year(cusp$Month)==Year1,cusp$Month==Month1,
                  cusp$Product==Product1)
sort1=filter_1 %>% dplyr::group_by(Customer,City,Product) %>% dplyr::summarise(sum(Amount))
   
sort2=arrange(sort,desc(sum(Amount)))  
  
}
Take_input()







