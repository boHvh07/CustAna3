# stargazer(model_1, type = "text")

model_2 <- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+PhoneService
               +MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
                 DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+
                 PaperlessBilling+PaymentMethod+MonthlyCharges+
                 TotalCharges+as.factor(tenure)
               ,data=telco,family="binomial")

# a shorter way is to subtract tenure from everything else and add as.factor(tenure) back: except var1 . - var1

model_2 <- glm(Churn ~ . +as.factor(tenure) -tenure , data=telco, family="binomial")
summary(model_2, digits=3)

# model 3 is too tedious to write out the long way. 
# the short way is to understand that var1*var2 = var1 + var2 + var1*var2. 
# So we remove tenure and payment method and add them with the star in betwen. 

model_3 <- glm(Churn ~ . +as.factor(tenure)*as.factor(PaymentMethod) -tenure -PaymentMethod, data=telco, family="binomial")
summary(model_3, digits=3)