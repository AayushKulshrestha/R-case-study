
#R CASE STUDY 2 (Credit card)

customer_acq <- read.csv("Customer Acqusition.csv")
Repayment<- read.csv("Repayment.csv")
spend<- read.csv("spend.csv")


library(Hmisc)

Hmisc::describe(customer_acq)
head(customer_acq)
tail(customer_acq)

Hmisc::describe(Repayment)
head(repayment)
tail(Repayment)

Hmisc::describe(spend)
head(spend)
tail(spend)


#1. In the above dataset,
#a. Incase age is less than 18, replace it with mean of age values.

head(customer_acq)

mean(customer_acq$Age)
customer_acq$Age[customer_acq$Age < 18] <- mean(customer_acq$Age)


#b. Incase spend amount is more than the limit, replace it with 50% of that customer's limit. 
#(customer's limit provided in acquisition table is the per transaction limit on his card) 

head(spend)
head(customer_acq)
library(sqldf)

customer1 <- merge(x = customer_acq,y=spend,by = "Customer")

require(dplyr)

customer1 <- customer1 %>% relocate(No, .before = Customer)

ifelse(customer1$Limit>customer1$Amount,((50 * customer1$Limit)/(100)),customer1$Amount)


#c. Incase the repayment amount is more than the limit, replace the repayment with the limit.

customer2<-merge(x=customer_acq,y=Repayment,by="Customer")

#ifelse(customer2$Amount>customer2$Limit,replace(customer2$Amount,customer2$Limit),customer2$Amount)

replace(customer2$Amount,customer2$Amount>customer2$Limit,customer2$Limit)


#2. From the above dataset create the following summaries: 
#a. How many distinct customers exist? 
library(dplyr)

count(data.frame(unique(customer1$Customer)))

#b. How many distinct categories exist? 
library(dplyr)

customer1%>%group_by(Segment)%>%count(Segment)

#c. What is the average monthly spend by customers? 


colnames(customer1)[which(names(customer1) == "Month")] <- "Date"

customer1$Date <-as.Date(customer1$Date,format =  "%d-%b-%y")

customer1$Date

class(customer1$Date)
typeof(customer1$Date)

customer1$Year <- format(as.Date(customer1$Date),"%Y")
customer1$Month <- format(as.Date(customer1$Date),"%B")

library(dplyr)
customer1%>%group_by(Month,Year)%>%summarise(Average_Monthly_Spend=mean(Amount,na.rm=TRUE))


#d. What is the average monthly repayment by customers? 

colnames(customer2)[which(names(customer2) == "Month")] <- "Date"

customer2$Date <-as.Date(customer2$Date,format =  "%d-%b-%y")

customer2$Date

class(customer2$Date)
typeof(customer2$Date)

customer2$Year <- format(as.Date(customer2$Date),"%Y")
customer2$Month <- format(as.Date(customer2$Date),"%B")

library(dplyr)
customer2%>%group_by(Month,Year)%>%summarise(Average_Monthly_repayment=mean(Amount,na.rm=TRUE))

#e. If the monthly rate of interest is 2.9%, what is the profit for the bank for each month? 
#(Profit is defined as interest earned on Monthly Profit. Monthly Profit = Monthly repayment - Monthly spend.
#Interest is earned only on positive profits and not on negative amounts) 

Monthly_Profit <- (customer2$Amount)-(customer1$Amount)

print(Monthly_Profit)
Monthly_Profit[Monthly_Profit < 0] <- 0
Monthly_Profit<- (Monthly_Profit*30*2.9)/100

Monthly_Profit<- (Monthly_Profit/12)

Monthly_Profit

#f. What are the top 5 product types? 

library(dplyr)

data.frame(customer1%>%group_by(Type,Product)%>%count())%>%top_n(5)


#g. Which city is having maximum spend? 
library(dplyr)

data.frame(customer1%>%group_by(City)%>%summarise(Amount=sum(Amount))%>%top_n(1))

#h. Which age group is spending more money?
data.frame(customer1%>%group_by(Age)%>%summarise(Amount=sum(Amount))%>%top_n(1))


#i. Who are the top 10 customers in terms of repayment?
library(dplyr)
data.frame(customer2%>%group_by(Customer)%>%summarise(sum(Amount))%>%top_n(10))

#3. Calculate the city wise spend on each product on yearly basis. 
#Also include a graphical representation for the same.


city_wise <-customer1%>%group_by(City,Year,Product)%>%summarise(Amount=(sum(Amount)))

data.frame(city_wise)
as.data.frame(city_wise)

is.data.frame(city_wise)
library(ggplot2)
ggplot(data = city_wise,mapping = aes(x = City,y = Amount,fill=Product))+geom_bar(stat ="identity",position = "dodge")+
  facet_wrap(~ Year)

#4. Create graphs for 
#a. Monthly comparison of total spends, city wise
city_wise1<- customer1%>%group_by(City)%>%summarise(Spend=sum(Amount))

ggplot(data = city_wise1,mapping = aes(x=City,y=Spend))+geom_bar(stat ="identity",position = "dodge")


#b. Comparison of yearly spend on air tickets

yearly_spend_air <- customer1%>%group_by(Year)%>%filter(Type=="AIR TICKET")%>%summarise(air_amount=sum(Amount))
as.data.frame(yearly_spend_air)
yearly_spend_air

ggplot(yearly_spend_air,aes(x=Year,y=air_amount))+geom_bar(stat ="identity",position = "dodge")
#c. Comparison of monthly spend for each product (look for any seasonality that exists in terms of spend)

library(dplyr)
monthly <-customer1%>%group_by(Product,Month)%>%summarise(sum_month=sum(Amount))
monthly
library(ggplot2)
ggplot(data=monthly,mapping = aes(x=Product,y=sum_month,fill=Month))+geom_bar(stat ="identity",position = "dodge")
  

  
#5. Write user defined R function to perform the following analysis:
#You need to find top 10 customers for each city in terms of their repayment amount
#by different products and by different time periods i.e. year or month.
#The user should be able to specify the product (Gold/Silver/Platinum) 
#and time period (yearly or monthly) and the function should automatically take 
#these inputs while identifying the top 10 customers. 
  
#Q5) Modify the udf such that the yearly aggregated and monthly aggregated output are
#separate That is when user passes input as month then the output should have the monthly 
#aggregated data for the particular product.Hint: Use if() and else if() 

customer1$Year<- as.numeric(customer1$Year)

as.numeric(customer1$Year)
library("Hmisc")
typeof(customer1$Month)
class(customer1$Month)

take_input2<-function()

{
  Year=readline("Enter year:")
  Product=readline("Enter the Product:")
  Month=readline("Enter month:")
  
if (Year==Year&&Product==Product) 
  { filter_1=filter(customer1,customer1$Year==Year&customer1$Product==Product)
  sort1=filter_1 %>% dplyr::group_by(Customer,City,Product)%>%summarise(sum(customer1$Amount))%>%top_n(10)
  sort2=arrange(sort1,desc(sum(customer1$Amount)))  
  sort2} 
  else if (Month==Month&&Product==Product)
  {
  filter_2=filter(customer1,customer1$Month==Month&customer1$Product==Product)
  sort3=filter_2%>%dplyr::group_by(Customer,City,Product)%>%summarise(sum(customer1$Amount))%>%top_n(10)
  sort4=arrange(sort3,desc(sum(customer1$Amount)))
  sort4} 
}

take_input2()

