library(car)
library(tidyverse)
library(pROC)
library(plotrix)

# drop the ID column, make senior citizen a factor variable, and divide totalcharges by 1000
telco<-telco[-c(1)]
telco$SeniorCitizen<-as.factor(telco$SeniorCitizen)
telco$TotalCharges<-telco$TotalCharges/1000

# Change Churn from "no" "yes" to 0 1
telco <- telco %>%
  mutate(Churn = ifelse(Churn == "No",0,1))

rbar<-mean(telco$Churn)

par(mai=c(.9,.8,.2,.2))
hist(telco$tenure, main = "", xlab="Tenure (# months a customer)", breaks = 71)


churn_tenure<-telco %>% as.data.frame() %>% group_by(tenure) %>% summarize(tenure=mean(tenure), p_churn=mean(Churn), n_churners=sum(Churn), n=n(), p_churn_se= sqrt((p_churn)*(1-p_churn)/n)) %>% mutate(lower_CI_pchurn = p_churn - 1.96*p_churn_se, upper_CI_pchurn = p_churn + 1.96*p_churn_se) 

head(churn_tenure)

par(mai=c(.9,.8,.2,.2))
plot(x = churn_tenure$tenure, y = churn_tenure$p_churn, main="Proportion of customers who churn by tenure", xlab="Tenure (# months a customer)", ylab="proportion of customer churning")
