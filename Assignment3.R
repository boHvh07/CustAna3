library(readr)
library(car)
library(tidyverse)
library(pROC)
library(plotrix) 

telco <- telco_test
telco$gender<-as.factor(telco$gender)
telco$PaymentMethod<-as.factor(telco$PaymentMethod)
telco$SeniorCitizen<-as.factor(telco$SeniorCitizen)
telco$Partner<-as.factor(telco$Partner)
telco$Dependents<-as.factor(telco$Dependents)
telco$PhoneService<-as.factor(telco$PhoneService)
telco$MultipleLines<-as.factor(telco$MultipleLines)
telco$OnlineSecurity<-as.factor(telco$OnlineSecurity)
telco$OnlineBackup<-as.factor(telco$OnlineBackup)
telco$DeviceProtection<-as.factor(telco$DeviceProtection)
telco$TechSupport<-as.factor(telco$TechSupport)
telco$StreamingTV<-as.factor(telco$StreamingTV)
telco$StreamingMovies<-as.factor(telco$StreamingMovies)
telco$PaperlessBilling<-as.factor(telco$PaperlessBilling)

telco <- telco %>%
  mutate(Churn = ifelse(Churn == "No",0,1))


churn_credit<-telco %>% as.data.frame() %>% group_by(telco$PaymentMethod) %>% summarize(p_churn=mean(Churn), n_churners=sum(Churn), n=n(), p_churn_se= sqrt((p_churn)*(1-p_churn)/n)) %>% mutate(lower_CI_pchurn = p_churn - 1.96*p_churn_se, upper_CI_pchurn = p_churn + 1.96*p_churn_se) 
head(churn_credit)
#q1.      0.151
#q2.      0.173


model_q3<-glm(Churn ~ telco$gender+telco$tenure+telco$SeniorCitizen+telco$PaymentMethod+telco$tenure*telco$PaymentMethod, data=telco, family = binomial(link="logit"))
summary(model_q3)
-0.0246095*100 
#q3       -2

#q4.      ???


telco_holdout <- `telco_holdout.(1)`
confusion_matrix <- (table(telco_holdout$Churn, prob > 0.5))
confusion_matrix <- as.data.frame.matrix(confusion_matrix)
colnames(confusion_matrix) <- c("No", "Yes")
confusion_matrix$Percentage_Correct <- confusion_matrix[1,]$No/(confusion_matrix[1,]$No+confusion_matrix[1,]$Yes)*100
confusion_matrix[2,]$Percentage_Correct <- confusion_matrix[2,]$Yes/(confusion_matrix[2,]$No+confusion_matrix[2,]$Yes)*100
print(confusion_matrix)
#q5.      ???
