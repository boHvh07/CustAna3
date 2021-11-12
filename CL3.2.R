# fit 
model_0<-glm(Churn ~ tenure, data=telco, family = binomial(link="logit"))

# show us coefficients and other model fit statistics
summary(model_0)

# predict a single observation with tenure = 35
pred <- predict(model_0, newdata = data.frame(tenure=35), se.fit = TRUE, type = "response")

pred$fit

# by hand, intercept is 1 and tenure = 35
C <- c(1, 35)

# coef(model_0) gives us the coefficients, 
# coef(model_0)%*%C gives us \beta_0 * 1 + \beta_1 * Tenure

exp(coef(model_0)%*%C)/(1+exp(coef(model_0)%*%C))

# create data set of tenure from 1 to 72
plotdat <- data.frame(tenure=(1:72))

# put predictions and 95% confidence intervals of those 
preddat <- predict(model_0,
                   type = "link",
                   newdata=plotdat,
                   se.fit=TRUE) %>% 
  as.data.frame() %>% 
  mutate(tenure=(1:72), 
         # model object model_0 has a component called linkinv that 
         # is a function that inverts the link function of the GLM:
         lower = model_0$family$linkinv(fit - 1.96*se.fit), 
         point.estimate = model_0$family$linkinv(fit), 
         upper = model_0$family$linkinv(fit + 1.96*se.fit)) 

# plot actual vs. logistic regression
par(mai=c(.9,.8,.2,.2))
plot(x = churn_tenure$tenure, y = churn_tenure$p_churn, main="Proportion of customers who churn by tenure", xlab="Tenure (# months a customer)", ylab="proportion of customer churning")
lines(x=preddat$tenure, y=preddat$point.estimate, col="red", lwd=2)
legend('topright',legend=c("churn proportion", "logistic regression"),col=c("black","red"),pch=c(1,NA),lty=c(NA,1), lwd=c(NA,2))

eq <- paste0("logit(p) = ",round(coef(model_0)[1],4),
             ifelse(coef(model_0)[2]<0,round(coef(model_0)[2],4),
                    paste("+",round(coef(model_0)[2],4))),
             paste(" tenure"))
# puts equation in figure
mtext(eq, 1,-3)

\\\

par(mai=c(.9,.8,.2,.2))
plotCI(x = churn_tenure$tenure,               # plotrix plot with confidence intervals
       y = churn_tenure$p_churn,
       li = churn_tenure$lower_CI_pchurn,
       ui = churn_tenure$upper_CI_pchurn, main="Proportion of customers who churn by tenure", xlab="Tenure (# months a customer)", ylab="proportion of customer churning")

lines(x=preddat$tenure, y=preddat$point.estimate, col="red", lwd=2, type = "l")
lines(x=preddat$tenure, y=preddat$lower, col="red", lty=2, lwd=1, type = "l")
lines(x=preddat$tenure, y=preddat$upper, col="red", lty=2, lwd=1, type = "l")

\\\

options(width = 200)
model_1 <- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+PhoneService
               +MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
                 DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+
                 PaperlessBilling+PaymentMethod+MonthlyCharges+
                 TotalCharges+tenure
               ,data=telco,family="binomial")

# another way of writing this is "~ ." which means regress Churn on everything else in the data set.

model_1 <- glm(Churn ~ . , data=telco, family="binomial")

summary(model_1)
