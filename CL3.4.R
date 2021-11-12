models <- paste0("model_", 0:3) # list of models
D<-sapply(models, function(x) get(x)$deviance) # get deviance D for each

D0<-model_0$null.deviance # D_0 is the same for all models

R2<-1-D/D0


par(mai=c(.9,.8,.2,.2))
barplot(R2, names.arg = c("model 0","model 1", "model 2", "model 3"), main=expression(paste("In-Sample R"^"2")), xlab="Model", ylab=expression(paste("R"^"2")))



\\\# you don't need to know how to write this code.
set.seed(19103)
n = nrow(telco)
K = 10 # # folds
foldid = rep(1:K, each=ceiling(n/K))[sample(1:n)]
# foldid[1:10]
OOS <- data.frame(model0=rep(NA, K), model1=rep(NA,K), model2=rep(NA,K), model3=rep(NA,K))


## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}  

# this part will take several minutes, fitting 3 models K times each

for(k in 1:K){
  train = which(foldid!=k) # data used to train
  
  # fit regressions
  model_0<- glm(Churn ~ tenure, data=telco[train,], family="binomial")
  summary(model_0)
  
  model_1 <- glm(Churn ~ . , data=telco[train,], family="binomial")
  summary(model_1)
  
  model_2 <- glm(Churn ~ . +as.factor(tenure) -tenure, data=telco[train,], family="binomial")
  summary(model_2)
  
  model_3 <- glm(Churn ~ . +as.factor(tenure)*as.factor(PaymentMethod) -tenure -PaymentMethod, data=telco[train,], family="binomial")
  summary(model_3)
  
  
  # predict on holdout data (-train)
  pred0<- predict(model_0, newdata=telco[-train,], type = "response")
  pred1<- predict(model_1, newdata=telco[-train,], type = "response")
  pred2<- predict(model_2, newdata=telco[-train,], type = "response")
  pred3<- predict(model_3, newdata=telco[-train,], type = "response")
  
  # calculate R2
  OOS$model0[k]<-R2(y = telco$Churn[-train],pred=pred0, family="binomial")
  OOS$model1[k]<-R2(y = telco$Churn[-train],pred=pred1, family="binomial")
  OOS$model2[k]<-R2(y = telco$Churn[-train],pred=pred2, family="binomial")
  OOS$model3[k]<-R2(y = telco$Churn[-train],pred=pred3, family="binomial")
  
  # print progress
  cat(k, "  ")
  
}
\\\# you don't need to know how to write this code.


par(mai=c(.9,.8,.2,.2))  
boxplot(OOS[,1:4], data=OOS, main=expression(paste("Out-of-Sample R"^"2")),
        xlab="Model", ylab=expression(paste("R"^"2")))

summary(model_1)

cat('coefficient:', coef(model_1)["SeniorCitizen1"],"\n")

cat('multiplicative effect on odds, exp(coefficient):', exp(coef(model_1)["SeniorCitizen1"]),"\n")

ocoef<-round(exp(coef(model_1)["SeniorCitizen"]),2)
cat('percent change in odds: exp(coefficient)-1:', exp(coef(model_1)["SeniorCitizen1"])-1,"\n")

ecoef<-round(exp(coef(model_1)["SeniorCitizen1"])-1,2)*100

linearHypothesis(model_1, c("ContractOne year = 0", "ContractTwo year = 0"))




