###PREDICT
newdata = data.frame(gender = "Male", SeniorCitizen=as.factor(1),Partner="No",Dependents="No", tenure=72,PhoneService="Yes",MultipleLines="No", InternetService="DSL", OnlineSecurity="No", OnlineBackup="No", DeviceProtection="No", TechSupport="Yes", StreamingTV="Yes", StreamingMovies="No", Contract="One year", PaperlessBilling="No", PaymentMethod="Mailed check", MonthlyCharges=30,TotalCharges=1)

predict(model_1,newdata,type="response")

###HOLDOUT SAMPLE

# ID column don't need to drop.
# make senior citizen a factor variable, and divide totalcharges by 1000
holdout_telco$SeniorCitizen<-as.factor(holdout_telco$SeniorCitizen)
holdout_telco$TotalCharges<-holdout_telco$TotalCharges/1000

# Change Churn from "no" "yes" to 0 1
holdout_telco <- telco_holdout
holdout_telco <- holdout_telco %>%
  mutate(Churn = ifelse(Churn == "No",0,1))

n_churners<-sum(holdout_telco$Churn)
head(holdout_telco)

rbar_ho<-mean(holdout_telco$Churn)


# predicted x'beta part of 
xb <- predict(model_1, type = "link", newdata=holdout_telco)
# the predicted probability 
prob <- predict(model_1, type = "response", newdata=holdout_telco)

head(cbind(xb,prob))


# order customers from least likely to churn (according to model) to most likely
ind<-order(prob)
head(prob[ind])


par(mai=c(.9,.8,.2,.2))
plot(xb[ind],holdout_telco$Churn[ind], pch=4,cex=0.3,col="blue", xlab="x'beta",ylab="P(Churn) on holdout data")
lines(x=xb[ind], y=prob[ind], col="red", lwd=2)
legend('left',legend=c("actual", "predicted (model 1)"),col=c("blue","red"), pch=c(1,NA),lty=c(NA,1), lwd=c(NA,2))


###CONFUSION MATRIX

confusion_matrix <- (table(holdout_telco$Churn, prob > 0.5))
confusion_matrix <- as.data.frame.matrix(confusion_matrix)
colnames(confusion_matrix) <- c("No", "Yes")
confusion_matrix$Percentage_Correct <- confusion_matrix[1,]$No/(confusion_matrix[1,]$No+confusion_matrix[1,]$Yes)*100
confusion_matrix[2,]$Percentage_Correct <- confusion_matrix[2,]$Yes/(confusion_matrix[2,]$No+confusion_matrix[2,]$Yes)*100
print(confusion_matrix)

cat('Overall Percentage:', (confusion_matrix[1,1]+confusion_matrix[2,2])/nrow(holdout_telco)*100)


###ROC CURVES

par(mai=c(.9,.8,.2,.2))
plot(roc(holdout_telco$Churn, prob), print.auc=TRUE, 
     col="black", lwd=1, main="ROC curve", xlab="Specificity: true negative rate", ylab="Sensitivity: true positive rate", xlim=c(1,0))

text(confusion_matrix$Percentage_Correct[[1]]/100, confusion_matrix$Percentage_Correct[[2]]/100, ".5 threshold")
abline(h=confusion_matrix$Percentage_Correct[[2]]/100, col="red",lwd=.3)
abline(v=confusion_matrix$Percentage_Correct[[1]]/100, col="red",lwd=.3)


###LIFT CURVES

ntiles <- function(x, bins) {
  quantiles = seq(from=0, to = 1, length.out=bins+1)
  cut(ecdf(x)(x),breaks=quantiles, labels=F)
}
# create deciles
prob_decile = ntiles(prob, 10)

# prob, decile and actual
pred<-data.frame(cbind(prob,prob_decile, holdout_telco$Churn))
colnames(pred)<-c("predicted","decile", "actual")

# create lift table by decile
# average churn rate by decile

# lift is the actual churn rate in the decile divided by average overall churn rate

lift_table<-pred %>% group_by(decile) %>%  summarize(actual_churn = mean(actual), lift = actual_churn/rbar_ho, n_customers=n()) %>% arrange(desc(decile)) %>% mutate(cum_customers=cumsum(n_customers)) %>% mutate(cum_lift=cumsum(actual_churn)/sum(actual_churn)*100)

lift_table



# order from highest to smallest in terms of prob
# percentage of churners from beginning to end.

pred<-pred %>% arrange(desc(predicted)) %>% mutate(prop_churn = cumsum(actual)/sum(actual)*100, prop_cust = seq(nrow(pred))/nrow(pred)*100)

head(pred)


# Plotting percentage of churners as a function of percentage of customers
par(mai=c(.9,.8,.2,.2))
plot(pred$prop_cust,pred$prop_churn,type="l",xlab="% of customers targeted using model",ylab="% of churners accounted for",xlim = c(0,100), ,ylim = c(0,100),col="blue")
legend('topleft', legend=c("Naive", "Logistic"), col=c("red", "blue"), lty=1:1, cex=0.8)
abline(a=0,b=1,col="red")
points(x=30, y= lift_table$cum_lift[3], pch=4, col="red",  cex=2, lwd=2)
text(x = 28,y= lift_table$cum_lift[3]+5, paste(round(lift_table$cum_lift[3],0), "%" ))


###SELECTING DECILES TO TARGET

gamma = 0.1  # probability that customer is rescued if he or she is a churner
LTV = 500   # lifetime value of rescued customer
delta = 50  # cost of incentive
c = 0.50  # cost of contact

# re-order lift from highest to lowest
# add columns to our lift table

profit_table<-lift_table %>% mutate(
  cum_prop_churners = cumsum(actual_churn*n_customers)/cum_customers, 
  profit = cum_customers*((gamma*LTV+delta*(1-gamma))*cum_prop_churners-delta-c),
  decile=11-decile)

profit_table


par(mai=c(.9,.8,.2,.2))
bp<-barplot(profit_table$profit ~ profit_table$decile, main="expected profits by # of deciles targeted", xlab="# deciles targeted", ylab="expected profits")

