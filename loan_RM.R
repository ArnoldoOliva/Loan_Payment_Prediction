###Libraries & load data
library(readr)
library(caret)
library(dplyr)
library(tidyverse)
library(ResourceSelection)


loan <- read_csv("C:/Users/HUGO/Downloads/loan_data.csv", col_names = T)
View(loan)

########data cleaning

####class & structure  of columns
sapply(loan, class)

str(loan)

###working with the character column:
loan$purpose=as.factor(loan$purpose)  #is a factor, but not a number

#convert purpose to dummies
loan_dmy=dummyVars("~purpose", data=loan,fullRank = T)
loan_dummy=data.frame(predict(loan_dmy,newdata=loan))

#merge the two dataframes:
loan_m=cbind(loan,loan_dummy)
View(loan_m)

#we dispose the character column
loan_m$purpose=NULL

#count nulls== False, 0 nulls out of 181982
table(is.na(loan_m))

###we change the composition of the variables:
#the factoric ones aka dummies:

#credit policy
loan_m[loan_m$credit.policy==0,]$credit.policy="customer do NOT meet credit criteria"
loan_m[loan_m$credit.policy==1,]$credit.policy="customer DO meet credit criteria"
loan_m$credit.policy=as.factor(loan_m$credit.policy)

#not fully paid (DEPENDENT ONE):
loan_m[loan_m$not.fully.paid==0,]$not.fully.paid="loan paid totally"
loan_m[loan_m$not.fully.paid==1,]$not.fully.paid="loan NOT paid totally"
loan_m$not.fully.paid=as.factor(loan_m$not.fully.paid)

#the dummies for purpose of the credit
loan_m$purpose.credit_card=as.factor(loan_m$purpose.credit_card)
loan_m$purpose.debt_consolidation=as.factor(loan_m$purpose.debt_consolidation)
loan_m$purpose.educational=as.factor(loan_m$purpose.educational)
loan_m$purpose.home_improvement=as.factor(loan_m$purpose.home_improvement)
loan_m$purpose.major_purchase=as.factor(loan_m$purpose.major_purchase)
loan_m$purpose.small_business=as.factor(loan_m$purpose.small_business)

#we rewatch again the str of the data
str(loan_m)

#check for imbalance
xtabs(~not.fully.paid + purpose.debt_consolidation,data = loan_m)



#first logistic model:
loan_log1=glm(not.fully.paid~credit.policy+int.rate+installment+fico, data = loan_m, family = "binomial")
summary(loan_log1)

####2nd model

###check for multicollinearity & 2nd larger model:
loan_logT=glm(not.fully.paid~., data = loan_m, family = "binomial")
summary(loan_logT)

vif_model=car::vif(loan_logT)  #vif is right

###check pseudo Rsquared of McFadden:

psR2=function(model){
  ll.null=model$null.deviance/-2
  ll.proposed=model$deviance/-2
  psR2=(ll.null-ll.proposed)/ll.null
  return(psR2)
}

##total model pseudo r2=   0.067
##chi squared for that R2:  #0, so small that is rounded to zero
chiR=1-pchisq(2*(ll.proposed-ll.null), df=(length(loan_logT$coefficients)-1))

##probabilities:
logit2prob=function(logitmodel){
  odd_exponents=exp(logitmodel)
  probs=odd_exponents/(1+odd_exponents)
  return(probs)
}

probs=round(logit2prob(loan_logT$coefficients), digits = 2)

#residuals:
plot(loan_logT$residuals)


##test Goodness of fit Hosmer-Lemeshow
hoslem.test(loan_m$not.fully.paid,fitted(loan_logT))  #pvalue 2.2e-16
#Ho: the model adjusts the data, we reject it, so the model DO NOT adjust the data


##accuracy:
#predict
predictions=predict(loan_logT, newdata = loan_m, type = "response")

loan$not.fully.paid=as.factor(loan$not.fully.paid)
prediclev=as.numeric(predictions>0.86157)  #
prediclev=as.factor(prediclev)
cm=confusionMatrix(prediclev,loan$not.fully.paid)
cm 

#Accuracy 43.41%, pos pred val: .774, neg pred val: .094

