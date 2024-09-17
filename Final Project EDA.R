# Final Project 

# Customer Lifetime Value Prediction
# To predict the Customer Lifetime Value for an insurance company offering vehicle insurance.

library(tidyverse) 
library(car) 
library(zoo)
library(lmtest) 
library(dplyr) 
library(stringr)
library(caret)
library(ggplot2) 
library(timeDate)
library(plotly)
library(readxl)
library(gganimate)
library(corrplot)
library(Hmisc)
library(vtree)
library(DataExplorer)
library(caTools)
library(nortest)
library(modelr)

# EDA
library(readxl)
clv <- read_excel("CLV.xlsx")
head(clv)

Insurance <- data.frame(clv)
colnames(Insurance)
hist(Insurance$Customer.Lifetime.Value,
     breaks = 800,
     freq = FALSE,
     main = "Histogram of CLV", xlab = "CLV", border = "Blue")

Insurance %>% introduce()
Insurance %>% plot_intro()

# Histogram of the target variable (CLV)
hist(Insurance$Customer.Lifetime.Value,
     breaks = 800,
     freq = FALSE,
     main = "Histogram of CLV", xlab = "CLV", border = "Blue")

# Categorical Variables 
# To visualize the effect of state on CLV
ggplot(Insurance,aes (x=State ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="State",y = "Customer Life Time Value", fill="State") +
  ggtitle("Sum of CLV contribution by State ")

# To visualize the effect of Education on CLV
ggplot(Insurance,aes (x=Education ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Education",y = "Customer Life Time Value", fill="Education") +
  ggtitle("Contribution of CLV by Education")

# To visualize the effect of Coverage on CLV
ggplot(Insurance,aes (x=Coverage ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Coverage",y = "Customer Life Time Value", fill="Coverage") +
  ggtitle("Contribution to CLV by Coverage")

# To visualize the effect of Employment Status on CLV
ggplot(Insurance,aes (x=EmploymentStatus ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Employment Status",y = "Customer Life Time Value", fill="Employment Status") +
  ggtitle("Contribution to CLV by Employment Status")

# To visualize the effect of Location Code on CLV
ggplot(Insurance,aes (x=Location.Code ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Location Code",y = "Customer Life Time Value", fill="Location Code") +
  ggtitle("Contribution to CLV by Location Code")

# Effect on CLV by State and Location Code
p1<-plot_ly(Insurance, x =~State, y =~Insurance$`Customer.Lifetime.Value`,type='bar',color=~Insurance$`Location.Code`)
layout(p1, title ='CLV w.r.t State and Location Code', yaxis = list(title = 'CLV '))

# To visualize the effect of Marital Status on CLV
ggplot(Insurance,aes (x=Insurance$"Marital.Status", y=Insurance$"Customer.Lifetime.Value")) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue") +
  labs(x="Marital Status",y = "Customer Life Time Value", fill="Marital Status") +
  ggtitle("Visualization of CLV wrt Marital Status")

# To visualize the effect of Policy Type on CLV
ggplot(Insurance,aes (x=Policy.Type ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Policy Type",y = "Customer Life Time Value", fill="Policy Type") +
  ggtitle("Contribution to CLV by Policy Type")

# To visualize the effect of gender on CLV
ggplot(Insurance,aes (x=Gender ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Gender",y = "Customer Life Time Value", fill="Gender") +
  ggtitle("Contribution to CLV by Gender")

# To visualize the effect of Sales Channel on CLV.
ggplot(Insurance,aes (x=Sales.Channel ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Sales Channel",y = "Customer Life Time Value", fill="Sales Channel") +
  ggtitle("Contribution to CLV by Sales Channel")

# To visualize the effect of Vehicle Class on CLV
ggplot(Insurance,aes (x=Vehicle.Class ,
                              y=Customer.Lifetime.Value)) + geom_bar(stat="summary",fun="sum", width=0.5, fill = "Blue")+
  labs(x="Vehicle Class",y = "Customer Life Time Value", fill="Vehicle Class") +
  ggtitle("Contribution to CLV by Vehicle Class")

# To visualize the correlation between the numerical variables
autoCorr <- Insurance[,c(3,10,13:17,22)]
colnames(autoCorr) <- c("Customer.Lifetime.Value", "Income", "Monthly.Premium.Auto", "Months.Since.Last.Claim", "Months.Since.Policy.Inception",
                        "Number.of.Open.Complaints", "Number.of.Policies", "Total.Claim.Amount")
plot_correlation(autoCorr)


