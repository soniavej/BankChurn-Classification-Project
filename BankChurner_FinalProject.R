str(BankChurn)
summary(BankChurn)
table(BankChurn$Attrition_Flag)
prop.table(table(BankChurn$Attrition_Flag))


## Customer churn
piepercent=prop.table(table(BankChurn$Attrition_Flag))*100
cast.names=c("Attrited","Existing")
pie3D(table(BankChurn$Attrition_Flag), labels = paste(cast.names,"-(",round(prop.table(table(BankChurn$Attrition_Flag))*100),"%)", sep = ""), 
    col = heat.colors(2), main = "Customer Churn",radius=1,explode=0.3,labelcex=1.25)


### Checking missing values
is.na(BankChurn)
colSums(is.na(BankChurn))
BankChurn%>%
  group_by(Education_Level)%>%
  summarise(n())

BankChurn%>%
  group_by(Marital_Status)%>%
  summarise(n())

BankChurn%>%
  group_by(Income_Category)%>%
  summarise(n())



unique(BankChurn$Education_Level)
unique(BankChurn$Marital_Status)
unique(BankChurn$Income_Category)
unique(BankChurn$Card_Category)

bank=BankChurn%>%
  select(age="Customer_Age",gender="Gender",dependent="Dependent_count",education="Education_Level",
         marital="Marital_Status",income="Income_Category",card="Card_Category",months="Months_on_book",relation="Total_Relationship_Count",inactive="Months_Inactive_12_mon",contacted="Contacts_Count_12_mon",
         creditlmt="Credit_Limit",balance="Total_Revolving_Bal",avgopen="Avg_Open_To_Buy",amt_change="Total_Amt_Chng_Q4_Q1",trans_amt="Total_Trans_Amt",
         trans_count="Total_Trans_Ct",ct_change="Total_Ct_Chng_Q4_Q1",util_ratio="Avg_Utilization_Ratio",flag="Attrition_Flag")%>%
  mutate(flag=recode(flag,"Existing Customer"=0,"Attrited Customer"=1))%>%
  mutate(education=recode(education,"Uneducated"="Uneducated","High School"="High_School","Unknown"="Graduate","Graduate"="Graduate","College"="Graduate","Post-Graduate"="Post_Graduate","Doctorate"="Phd"))%>%
  mutate(income=recode(income,"Unknown"="below40","Less than $40K"="below40","$40K - $60K"="40to60","$60K - $80K"="60to80","$80K - $120K"="80to120","$120K +"="above120"))%>%
  mutate(marital=recode(marital,"Unknown"="Married"))
  
View(bank)
  
unique(bank$education)
unique(bank$marital)
unique(bank$income)
unique(bank$card)

str(bank)

bank$flag=as.factor(bank$flag)
bank$gender=as.factor(bank$gender)
bank$education=as.factor(bank$education)
bank$marital=as.factor(bank$marital)
bank$card=as.factor(bank$card)
bank$income=as.factor(bank$income)

library(purrr)
library(tidyr)
library(ggplot2)


plot4 <- ExpNumViz(bank,target="flag",type=1,nlim=2,fname=NULL,col=c("darkgreen","springgreen3","springgreen1"),Page=c(3,2),sample=NULL)
plot4[[1]]


#########    Visualisation   ############

##1. Churn vs Income Level###########################

pieper=prop.table(table(bank$income))*100
cast.names=c(c("40to60","60to80","80to120","above120","below40"))
pie3D(table(bank$income), labels = paste(cast.names,"-(",round(pieper),"%)", sep = ""), 
      col = heat.colors(5), main = "Customer Churn",radius=1,explode=0.3,labelcex=1.25)

####by attrition
bank %>% 
  #group_by(income)%>%
  count(income = factor(income,levels=c("below40","40to60","60to80","80to120","above120")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = income, y = Percentage, fill = flag, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Churn as per Income Level")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))


##by group
bank %>% 
  group_by(income)%>%
  count(income = factor(income,levels=c("below40","40to60","60to80","80to120","above120")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n))%>%
  ggplot(aes(income,Percentage,fill=flag))+geom_col(color="Blue",width=0.6)+
  geom_text(aes(label =scales::percent(Percentage)),position = position_stack(vjust=0.25),size=3.5,check_overlap =TRUE)+
  scale_y_continuous(label = scales::percent)%>%
  labs(title="Churn vs Income Level")+xlab("Income Level")+ylab("Percentage")+
  theme(panel.background = element_rect(fill = "grey", colour = "Blue",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))



#Chi-square
Tab3=table(bank$flag,bank$income)
chisq.test(Tab3)


##As the P value is less than 0.05, we can say that attrition flag is 
##dependent on the income level of the customer.

## 2. Churn as per Education level
pieper=prop.table(table(bank$education))*100
cast.names=c("Graduate","High_School","Phd","Post_Graduate","Uneducated")
pie3D(table(bank$education), labels = paste(cast.names,"-(",round(prop.table(table(bank$education))*100),"%)", sep = ""), 
      col = heat.colors(5), main = "Customer Churn",radius=1.5,explode=0.4,labelcex=0.8)

#######
####by attrition
bank %>% 
  #group_by(education)%>%
  count(education = factor(education,levels=c("Uneducated","High_School","Graduate","Post_Graduate","Phd")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = education, y = Percentage, fill = flag, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Churn as per Education Level")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))

##by group
bank %>% 
  group_by(education)%>%
  count(education = factor(education,levels=c("Uneducated","High_School","Graduate","Post_Graduate","Phd")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n))%>%
  ggplot(aes(education,Percentage,fill=flag))+geom_col(color="Blue",width=0.6)+
  geom_text(aes(label =scales::percent(Percentage)),position = position_stack(vjust=0.25),size=3.5,check_overlap =TRUE)+
  scale_y_continuous(label = scales::percent)%>%
  labs(title="Churn vs Education Level")+xlab("Education Level")+ylab("Percentage")+
  theme(panel.background = element_rect(fill = "grey", colour = "Blue",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))





##Chi-square test
#education
Tab1=table(bank$flag,bank$education)
chisq.test(Tab1)

##As the P value is less than 0.05, we can say that attrition flag is 
##dependent on the education level of the customer.

### 3.Churn as per Marital Status 
pieper=prop.table(table(bank$marital))*100
cast.names=c("Divorced","Married","Single")
pie3D(table(bank$marital), labels = paste(cast.names,"-(",round(prop.table(table(bank$marital))*100),"%)", sep = ""), 
      col = heat.colors(3), main = "Customer Churn",radius=1,explode=0.3,labelcex=1.25)

##by attrition
bank %>% 
  count(marital = factor(marital,levels=c("Single","Married","Divorced")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = marital, y = Percentage, fill = flag, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Churn as per Marital Status")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))

##by group
bank %>%
  group_by(marital)%>%
   count(marital = factor(marital,levels=c("Single","Married","Divorced")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n))%>%
  ggplot(aes(marital,Percentage,fill=flag))+geom_col(color="Blue",width=0.6)+
  geom_text(aes(label =scales::percent(Percentage)),position = position_stack(vjust=0.25),size=3.5,check_overlap =TRUE)+
  scale_y_continuous(label = scales::percent)%>%
  labs(title="Churn vs Marital Status")+xlab("Marital Status")+ylab("Percentage")+
  theme(panel.background = element_rect(fill = "grey", colour = "Blue",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))



#3.marital
Tab2=table(bank$flag,bank$marital)
chisq.test(Tab2)
summary(Tab2)


## 4. Churn as per gender
pieper=prop.table(table(bank$gender))*100
cast.names=c("F","M")
pie3D(table(bank$gender), labels = paste(cast.names,"-(",round(prop.table(table(bank$gender))*100),"%)", sep = ""), 
      rainbow.colors=2, main = "Customer Churn",radius=1,explode=0.3,labelcex=1.25)
###
bank%>%
  group_by(gender)%>%
  summarise(n())

bank %>% 
  #group_by(gender)%>%
  count(gender = factor(gender,levels=c("M", "F")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = gender, y = Percentage, fill = flag, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Churn as per gender")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))

bank %>% 
  group_by(gender)%>%
  count(gender = factor(gender,levels=c("M","F")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n))%>%
  ggplot(aes(gender,Percentage,fill=flag))+geom_col(color="Blue",width=0.6)+
  geom_text(aes(label =scales::percent(Percentage)),position = position_stack(vjust=0.25),size=3.5,check_overlap =TRUE)+
  scale_y_continuous(label = scales::percent)%>%
  labs(title="Churn vs Gender")+xlab("Gender")+ylab("Percentage")+
  theme(panel.background = element_rect(fill = "grey", colour = "Blue",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

#4.gender
Tab=table(bank$flag,bank$gender)
chisq.test(Tab,correct=FALSE)



##5. churn vs card

pieper=prop.table(table(bank$card))*100
cast.names=c("Blue","Gold","Platinum",'Silver')
pie3D(table(bank$card), labels = paste(cast.names,"-(",round(prop.table(table(bank$card))*100),"%)", sep = ""), 
      rainbow.colors=4,explode=0.5, main = "Customer Churn as per Card Type",radius=1.5,labelcex=0.8)

##by attrition
bank %>% 
  #group_by(card)%>%
  count(card = factor(card,levels=c("Blue","Gold","Silver","Platinum")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = card, y = Percentage, fill = flag, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Churn as per Card Type")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))

bank%>%
  group_by(card,flag)%>%
  summarise(n())
##by group
bank %>% 
  group_by(card)%>%
  count(card = factor(card,levels=c("Blue","Gold","Silver","Platinum")), flag = factor(flag)) %>% 
  mutate(Percentage = prop.table(n))%>%
  ggplot(aes(card,Percentage,fill=flag))+geom_col(color="Blue",width=0.6)+
  geom_text(aes(label =scales::percent(Percentage)),position = position_stack(vjust=0.25),size=3.5,check_overlap =TRUE)+
  scale_y_continuous(label = scales::percent)%>%
  labs(title="Churn vs Gender")+xlab("Gender")+ylab("Percentage")+
  theme(panel.background = element_rect(fill = "grey", colour = "Blue",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))


#5.card

Tab=table(bank$flag,bank$card)
chisq.test(Tab,correct = FALSE)
#X-squared = 2.2342, df = 3, p-value = 0.5252

### Correlation (Numeric Variables)

## Plot for all numerical variables
library(SmartEDA)
plot4 <- ExpNumViz(bank,target="flag",type=1,nlim=2,fname=NULL,col=c("darkgreen","springgreen3","springgreen1"),Page=c(3,2),sample=NULL)
plot4[[1]]
#Age
ggplot(bank,aes(x=flag,y=age))+geom_violin(aes(fill=flag))+
  geom_boxplot(width=0.2)+geom_point()+theme_classic()+labs(title="Churn as per Customer Age")+ xlab("Flag")+ylab("Age")+
  theme(plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"), axis.line = element_line(color='black'))


one.way =aov(age~flag, data = bank)

summary(one.way)
##P value 0.067>0.05, which indicates that age and flag are independent of each other.

##Dependents
ggplot(bank,aes(x=flag,y=dependent))+geom_boxplot(aes(width=0.2,fill=flag))+
  geom_point()+theme_classic()+labs(title="Churn as per Number of Dependents")+ xlab("Flag")+ylab("Dependents")+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"), axis.line = element_line(color='black'))
one.way =aov(dependent~flag, data = bank)
summary(one.way)
##P value 0.056>0.05, which indicates that age and flag are independent of each other.

#Months
ggplot(bank,aes(x=flag,y=months))+geom_boxplot(aes(width=0.2,fill=flag))+
theme_classic()+labs(title="Churn as per Months on Book")+ xlab("Flag")+ylab("Months")+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"), axis.line = element_line(color='black'))
one.way =aov(months~flag, data = bank)
summary(one.way)
##P value 0.168>0.05, therefore both are independent of each other.

### Correlation


library(psych)
pairs.panels(bank[c("age","dependent","months","relation","inactive","contacted","creditlmt","balance","avgopen","amt_change","trans_amt","trans_count","ct_change","util_ratio","flag")],scale=FALSE,stars =TRUE,cex.cor=7,cex.labels=1)

#Dummy creation
bank1=bank 
bank1=dummy_cols(bank,select_columns =c("gender","education","card","marital","income"),remove_selected_columns = TRUE)
View(bank1)


# Train and test dataset
set.seed(100)
n=nrow(bank1)
shuffle=bank1[sample(n),]

#split the data
train_indices=1:round(0.7*n)
test_indices=(round(0.7*n)+1):n

#Making the new data set
train=shuffle[train_indices,]
test=shuffle[test_indices,]

#Tables
table(train$flag)
table(test$flag)
prop.table(table(train$flag))
prop.table(table(test$flag))

#logistic Regression
#Model Creation

glm.fit=glm(flag~.,family=binomial,data=train)
summary(glm.fit)


#Predicting test data
result=predict(glm.fit,newdata=test,type="response")
result=ifelse(result>0.5,1,0)
result

(conf=table(test$flag,result))
TP=conf[1,1]
FN=conf[1,2]
FP=conf[2,1]
TN=conf[2,2]

#calculate and print the accuracy
acc=(TP+TN)/(TP+FN+FP+TN)
acc*100

#calculate and print the precision
prec=TP/(TP+FP)
prec*100

#calculate and print the recall
rec=TP/(TP+FN)
rec*100

##F1
F1=(2*prec*rec)/(prec+rec)
F1*100

#### Model 2

bank2=bank%>%
  select(-c(age,months,util_ratio,education,avgopen,card))
bank2=dummy_cols(bank2,select_columns =c("gender","marital","income"),remove_selected_columns = TRUE)

# Train and test dataset
set.seed(100)
n=nrow(bank2)
shuffle=bank2[sample(n),]

#split the data
train_indices1=1:round(0.7*n)
test_indices1=(round(0.7*n)+1):n

#Making the new data set
train1=shuffle[train_indices1,]
test1=shuffle[test_indices1,]

#Tables
table(train1$flag)
table(test1$flag)
prop.table(table(train1$flag))
prop.table(table(test1$flag))

#logistic Regression
#Model Creation

glm.fit1=glm(flag~.,family=binomial,data=train1)
summary(glm.fit1)



#Predicting test data
result1=predict(glm.fit1,newdata=test1,type="response")
result1=ifelse(result1>0.5,1,0)
result1

(conf1=table(test1$flag,result1))
TP1=conf1[1,1]
FN1=conf1[1,2]
FP1=conf1[2,1]
TN1=conf1[2,2]

#calculate and print the accuracy
acc1=(TP1+TN1)/(TP1+FN1+FP1+TN1)
acc1*100

#calculate and print the precision
prec1=TP1/(TP1+FP1)
prec1*100

#calculate and print the recall
rec1=TP1/(TP1+FN1)
rec1*100

#F1 SCORE
F1=(2*prec1*rec1)/(prec1+rec1)
F1*100

#### Model 3

bank3=bank%>%
  select(-c(income,age,months,util_ratio,education,avgopen,card,creditlmt))
bank3=dummy_cols(bank3,select_columns =c("gender","marital"),remove_selected_columns = TRUE)

# Train and test dataset
set.seed(100)
n=nrow(bank3)
shuffle=bank3[sample(n),]

#split the data
train_indices2=1:round(0.7*n)
test_indices2=(round(0.7*n)+1):n

#Making the new data set
train2=shuffle[train_indices2,]
test2=shuffle[test_indices2,]

#Tables
table(train2$flag)
table(test2$flag)
prop.table(table(train2$flag))
prop.table(table(test2$flag))

#logistic Regression
#Model Creation

glm.fit2=glm(flag~.,family=binomial,data=train2)
summary(glm.fit2)



#Predicting test data
result2=predict(glm.fit2,newdata=test2,type="response")
result2=ifelse(result2>0.5,1,0)
result2

(conf2=table(test2$flag,result2))
TP2=conf1[1,1]
FN2=conf1[1,2]
FP2=conf1[2,1]
TN2=conf1[2,2]

#calculate and print the accuracy
acc2=(TP2+TN2)/(TP2+FN2+FP2+TN2)
acc2*100

#calculate and print the precision
prec1=TP1/(TP1+FP1)
prec1*100

#calculate and print the recall
rec1=TP1/(TP1+FN1)
rec1*100

#F1 SCORE
F1=(2*prec1*rec1)/(prec1+rec1)
F1*100







###ROC and AUC Curve

roc(test$flag~result,plot=TRUE,legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="Red",lwd=4,print.auc=TRUE,print.auc.y=72)
plot.roc(test1$flag~result1,percent=TRUE,add=TRUE,col="green",lwd=4,print.auc=TRUE,print.auc.y=65)
plot.roc(test2$flag~result2,percent=TRUE,add=TRUE,col="blue",lwd=4,print.auc=TRUE,print.auc.y=58)
legend("bottomright",legend=c("LR-Model 1","LR-Model 2","LR-Model 3"),col=c("red","green","blue"),lwd=4)


###Decision Tree
install.packages("rattle")
library(RColorBrewer)
library(rattle)
library(rpart)
library(rpart.plot)
tree=rpart(flag~.,train,method="class")
fancyRpartPlot(tree,cex=0.60)


# Creating Confusion Matrix

pred2=predict(tree, test,type="class")
t=table(predictions=pred2,actual=test$flag)
predwithprob=predict(tree, test, type = "prob")
conf2=table(test$flag, pred2)
print(conf2)
TP2=conf2[1,1]
FN2=conf2[1,2]
FP2=conf2[2,1]
TN2=conf2[2,2]

#Calculating Accuracy, precision,and recall
acc2=(TP2+TN2)/(TP2+FN2+FP2+TN2)
acc2*100
#calculate and print the precision
prec2=TP2/(TP2+FP2)
prec2*100
#calculate and print the recall
rec2=TP2/(TP2+FN2)
rec2*100
#F1 SCORE
F1=(2*prec2*rec2)/(prec2+rec2)
F1*100
#Random Forest

train$flag=as.factor(train$flag)
fit=randomForest(flag ~.,data=train,ntree=1000,na.action=na.roughfix)
fit1=predict(fit, test)
test$fit1=fit1
View(test)
predrf=predict(fit, test, type = "prob")
View(train)
#Creating Confusion matrix
(conf3=table(test$flag, fit1))
TP3=conf3[1, 1]
FN3<-conf3[1, 2]
FP3<-conf3[2, 1]
TN3<-conf3[2, 2]

#Calculating Accuracy
acc3<-(TP3+TN3)/(TP3+FN3+FP3+TN3)
acc3*100
#Calculate and print out the precision: prec
prec3<-TP3/(TP3+FP3)
prec3*100

#Calculate and print out the recall: rec
rec3<-TP3/(TP3+FN3)
rec3*100

#F1 SCORE
F1=(2*prec3*rec3)/(prec3+rec3)
F1*100


 
##load the package class 
library(class) 
##run knn function
knnm <- knn(train,test,cl=train$flag,k=3)  
##create confusion matrix 
(tab <- table(knnm,iris_test_category))  
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is. 
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100} accuracy(tab)
#### Final ROC & AUC Curve

roc(test$flag~result,plot=TRUE,legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="Red",lwd=4,print.auc=TRUE,print.auc.y=75)
plot.roc(test1$flag~result1,percent=TRUE,add=TRUE,col="green",lwd=4,print.auc=TRUE,print.auc.y=70)
plot.roc(test$flag,predrf[,2],percent=TRUE,col="black",lwd=4,print.auc=TRUE,add=TRUE,print.auc.y=60)
plot.roc(test$flag,predwithprob[,2],percent=TRUE,add=TRUE,col="orange",lwd=4,print.auc=TRUE,print.auc.y=65)
legend("bottomright",legend=c("LR-Model 1","LR-Model 2","Decision Tree","Random Forest"),col=c("red","green","orange","Black"),lwd=2,cex=0.8)

varImpPlot(fit)
new_data=data.frame(age="45",gender_M="1",gender_F="0",dependent="3",education_Graduate="0",education_High_School="1",education_Phd="0",education_Post_Graduate="0",education_Uneducated="0",card_Blue="1",card_Gold="0", card_Platinum ="0",
                    card_Silver="0", marital_Divorced ="0",
                    marital_Married="1", marital_Single="0",income_40to60="0", income_60to80="1",
                    income_80to120="0", income_above120="0",
                    income_below40="0",months="39",relation="5",inactive="1",contacted="3",creditlmt="12691",balance="777",avgopen="11914",amt_change="1.335",trans_amt="1144",trans_count="42",ct_change="1.625",util_ratio="0.061")
predict(fit,new_data)

new_data1=data.frame(age="62",gender_M="0",gender_F="1",dependent="0",education_Graduate="1",education_High_School="0",education_Phd="0",education_Post_Graduate="0",education_Uneducated="0",card_Blue="1",card_Gold="0", card_Platinum ="0",
                    card_Silver="0", marital_Divorced ="0",
                    marital_Married="1", marital_Single="0",income_40to60="0", income_60to80="0",
                    income_80to120="0", income_above120="0",
                    income_below40="1",months="49",relation="2",inactive="3",contacted="3",creditlmt="1438.3",balance="0",avgopen="1438.3",amt_change="1.047",trans_amt="692",trans_count="16",ct_change="0.6",util_ratio="0")
predict(fit,new_data1)
View(test)
new_data3=data.frame(age="58",gender_M="0",gender_F="1",dependent="1",education_Graduate="0",education_High_School="0",education_Phd="1",education_Post_Graduate="0",education_Uneducated="0",card_Blue="1",card_Gold="0", card_Platinum ="0",
                    card_Silver="0", marital_Divorced ="0",
                    marital_Married="1", marital_Single="0",income_40to60="0", income_60to80="0",
                    income_80to120="0", income_above120="0",
                    income_below40="1",months="48",relation="4",inactive="2",contacted="2",creditlmt="1438.3",balance="801",avgopen="637.3",amt_change="0.595",trans_amt="4223",trans_count="60",ct_change="0.579",util_ratio="0.577")
predict(fit,new_data3)