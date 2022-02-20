str(income_ev)
summary(income_ev)
table(income_ev$income)
prop.table(table(income_ev$income))
income_ev[] = lapply(income_ev, gsub, pattern='-', replacement='_')

## Recoding
income1=income_ev%>%
  select(incomelevel="income",age,workclass,fnlwgt,education,edu_num="education-num",marital="marital-status",occupation,relationship,race,sex,cap_gain="capital-gain",cap_loss="capital-loss",hpw="hours-per-week",country="native-country")%>%
  mutate(incomelevel=recode(incomelevel,"<=50K"=0,">50K"=1))%>%
  mutate(workclass=recode(workclass,"State_gov"="govt","Federal_gov"="govt","Local_gov"="govt",
                          "Self_emp_not_inc"="Self_employed","Self_emp_inc"="Self_employed",
                          "Private"="Private","Without_pay"="others","?"="Private",
                          "Never_worked"="others"))%>%
    mutate(education=recode(education,"12th"="Schooling","Preschool"="Schooling","1st_4th"="Schooling",
                          "10th"="Schooling","5th_6th"="Schooling","7th_8th"="Schooling",
                          "9th"="Schooling","11th"="Schooling","Bachelors"="Graduate",
                          "HS_grad"="Graduate","Some_college"="Graduate","Masters"="Masters",
                          "Assoc_acdm"="Masters","Assoc_voc"="Masters","Prof_school"="Masters",
                          "Doctorate"="Phd"))%>%                   
  mutate(marital=recode(marital,"Never_married"="Single","Married_civ_spouse"="Married",
                        "Married_spouse_absent"="Married","Married_AF_spouse"="Married",
                        "Divorced"="Seperated","Separated"="Seperated","Widowed"="Seperated"))%>%            
  mutate(occupation=recode(occupation,"Adm_clerical"="Admin/Clerical","Sales"="Admin/Clerical","Handlers_cleaners"="Admin/Clerical",
                           "Craft_repair"="Admin/Clerical","Transport_moving"="Admin/Clerical","Farming_fishing"="others",
                           "Machine_op_inspct"="Technical","Tech_support"="Technical",
                           "Protective_serv"="others","Armed_Forces"="others","Priv_house_serv"="others",
                           "Exec_managerial"="Managerial","Prof_specialty"="Managerial",
                           "Other_service"="others","?"="Managerial"))%>%            
  mutate(country=recode(country,"United_States"="US","Cuba"="US","Jamaica"="US","India"="Asia",
                        "Mexico"="US","South"="US","Puerto_Rico"="US","Honduras"="US","England"="UK",
                        "Canada"="US","Germany"="Europe","Iran"="Asia","Philippines"="Asia",
                        "Italy"="Europe","Poland"="Europe","Columbia"="Asia","Cambodia"="Asia",
                        "Thailand"="Asia","Ecuador"="US","Laos"="Asia","Taiwan"="Asia","Haiti"="US",
                        "Portugal"="Europe","Dominican_Republic"="US","El_Salvador"="US",
                        "France"="Europe","Guatemala"="US","China"="Asia","Japan"="Asia",
                        "Yugoslavia"="Asia","Peru"="US","Outlying_US(Guam_USVI_etc)"="US",
                        "Scotland"="UK","Trinadad&Tobago"="US","Greece"="Europe",
                        "Nicaragua"="US","Vietnam"="Asia","Hong"="Asia","Ireland"="UK",
                        "Hungary"="Europe","Holand_Netherlands"="Europe","?"="US"))%>%
mutate(relationship=recode(relationship,"Unmarried"="others","Not_in_family"="others","Other_relative"="others","Own_child"="Parent"))
  

##Data Cleaning

unique(income1$workclass)
unique(income1$education)
unique(income1$marital)
unique(income1$occupation)
unique(income1$relationship)
unique(income1$race)
unique(income1$sex)
income1%>%
  group_by(country)%>%
  summarise(length(country))




str(income1)
income1$workclass=as.factor(income1$workclass)
income1$education=as.factor(income1$education)
income1$marital=as.factor(income1$marital)
income1$occupation=as.factor(income1$occupation)
income1$relationship=as.factor(income1$relationship)
income1$race=as.factor(income1$race)
income1$sex=as.factor(income1$sex)
income1$country=as.factor(income1$country)
income1$incomelevel=as.factor(income1$incomelevel)

str(income1)
income1$age=as.numeric(income1$age)
income1$fnlwgt=as.numeric(income1$fnlwgt)
income1$edu_num=as.numeric(income1$edu_num)
income1$cap_gain=as.numeric(income1$cap_gain)
income1$cap_loss=as.numeric(income1$cap_loss)
income1$hpw=as.numeric(income1$hpw)

income=income1%>%
  mutate(cap_change=cap_gain-cap_loss)%>%
  select(-c(cap_gain,cap_loss))
  
  
  
##xtabs(~incomelevel+workclass,data=income1)

View(income1)
#Visualisation
##### 1. workclass and incomelevel ####
income1 %>% 
  count(Workclass = factor(workclass,levels=c("govt","Private","Self_employed","others")), incomelevel = factor(incomelevel)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = Workclass, y = Percentage, fill = incomelevel, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Income Level as per Work Class")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                      panel.background = element_rect(fill = "white"),
                                                      axis.line = element_line(color='black'))



## 2. Income level as per Education level
income1 %>% 
  count(education = factor(education,levels=c("Schooling","Graduate","Masters","Phd")), incomelevel = factor(incomelevel)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = education, y = Percentage, fill = incomelevel, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Income Level as per Education Level")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))



### 3.Income Level as per Marital Status & Age
income1$marital=factor(income1$marital,levels = c("Single", "Married", "Seperated"))
ggplot(data=income1)+geom_boxplot(aes(x=marital,y=age,fill=as.factor(incomelevel)))+
                    labs(title="Income Level as per Marital Status & Age")+xlab("Marital Status")+ylab("Age")+theme(plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))


#### 4.
ggplot(income1,aes(x=age,fill=as.factor(incomelevel)))+geom_histogram(binwidth=0.2,position="dodge")

### 5.Income level as per occupation and hours per week
income1$occupation=factor(income1$occupation,levels = c("Admin/Clerical", "Technical", "Managerial","others"))
ggplot(data=income1)+geom_boxplot(aes(x=occupation,y=hpw,fill=as.factor(incomelevel)))+labs(title="Income Level as per Occupation & Hpw")+xlab("Occupation")+ylab("hpw")+theme(plot.background = element_rect(fill = "white"),
                                                                                                                                                                                       panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))
## 6.Income level as per relationship
income1 %>% 
  count(relationship = factor(relationship,levels=c("Husband","Wife","Parent","others")), incomelevel = factor(incomelevel)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = relationship, y = Percentage, fill = incomelevel, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Income Level as per Relationship")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))

###.7.Income Level as per Race
income1 %>% 
  count(race = factor(race,levels=c("Amer_Indian_Eskimo","Asian_Pac_Islander","White","Black","Other")), incomelevel = factor(incomelevel)) %>% 
  mutate(Percentage = round(prop.table(n),2)) %>% 
  ggplot(aes(x = race, y = Percentage, fill = incomelevel, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Income Level as per Race")+xlab("Race")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))

## 8. Income level as per sex

ggplot(data=income1)+geom_boxplot(aes(x=sex,y=age,fill=as.factor(incomelevel)))+
  labs(title="Income Level as per Sex & Age")+xlab("Sex")+ylab("Age")+
  theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),
        axis.line = element_line(color='black'))

### 9.Income Level as per Capital Change                                                                                                                                                          
ggplot(data=income,aes(y=cap_change,x=as.factor(incomelevel),col=as.factor(incomelevel)))+geom_point()+
  labs(title="Income Level as per Capital Change")+xlab("Income Level")+ylab("Capital Change")+
  theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),
        axis.line = element_line(color='black'))

## 10.
income1 %>% 
count(country = factor(country,levels=c("US","Asia","Europe","UK")), incomelevel = factor(incomelevel)) %>% 
  mutate(Percentage = round(prop.table(n),2)) %>% 
  ggplot(aes(x = country, y = Percentage, fill = incomelevel, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') +labs(title="Income Level as per Country")+xlab("Race")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(plot.background = element_rect(fill = "white"),
                                                     panel.background = element_rect(fill = "white"),
                                                     axis.line = element_line(color='black'))



#Test for categorical variables
#1.workclass
Tab=table(income$incomelevel,income$workclass)
chisq.test(Tab,correct=FALSE)
# X-squared = 1045.7, df = 8, p-value < 2.2e-16
#2.education
Tab1=table(income$incomelevel,income$education)
chisq.test(Tab1)
summary(Tab1)
#3.marital
Tab2=table(income$incomelevel,income$marital)
chisq.test(Tab2)
summary(Tab2)
#4.occupation
Tab3=table(income1$incomelevel,income1$occupation)
chisq.test(Tab3)
#5.relationship
Tab4=table(income1$incomelevel,income1$relationship)
chisq.test(Tab4)
#6.race
Tab5=table(income1$incomelevel,income1$race)
chisq.test(Tab5)
#7.sex
Tab6=table(income1$incomelevel,income1$sex)
chisq.test(Tab6)
#8.country
Tab7=table(income1$incomelevel,income1$country)
chisq.test(Tab7)

### Correlation (Numeric Variables)
summary(income)
library(psych)
pairs.panels(income[c("age","fnlwgt","edu_num","hpw","cap_change","incomelevel")],scale=FALSE,stars =TRUE,cex.cor=5,cex.labels=1)
income=income %>% select(-c(fnlwgt))
#Dummy creation
income=dummy_cols(income,select_columns =c("workclass","education","marital","occupation","relationship","race","sex","country"),remove_selected_columns = TRUE)
View(income)
str(income)

# Train and test dataset
set.seed(100)
n=nrow(income)
shuffle=income[sample(n),]

#split the data
train_indices=1:round(0.7*n)
test_indices=(round(0.7*n)+1):n

#Making the new data set
train=shuffle[train_indices,]
test=shuffle[test_indices,]

#Tables
table(train$incomelevel)
table(test$incomelevel)
prop.table(table(train$incomelevel))
prop.table(table(test$incomelevel))

str(income1)
#logistic Regression
#Model Creation

model=glm(incomelevel~.,family=binomial,data=train)
summary(model)
vif(model)
distinct(income(workclass))

#Predicting test data
result=predict(model,newdata=test,type="response")
result=ifelse(result>0.5,1,0)
result

(conf=table(test$incomelevel,result))
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

###Decision Tree
install.packages("rattle")
library(RColorBrewer)
library(rattle)
library(rpart)
library(rpart.plot)
tree=rpart(incomelevel~.,train,method="class")
fancyRpartPlot(tree,cex=0.50)

# Creating Confusion Matrix

pred=predict(tree, test, type = "class")
conf1=table(test$incomelevel, pred)
print(conf1)
TP=conf1[1,1]
FN=conf1[1,2]
FP=conf1[2,1]
TN=conf1[2,2]

#Calculating Accuracy, precision,and recall
acc=(TP+TN)/(TP+FN+FP+TN)
acc*100
#calculate and print the precision
prec=TP/(TP+FP)
prec*100
#calculate and print the recall
rec=TP/(TP+FN)
rec*100

#Random Forest
train$incomelevel = as.character(train$incomelevel)
train$incomelevel=as.factor(train$incomelevel)

fit=randomForest(incomelevel ~ .,data=train,ntree=10,na.action=na.roughfix)
fit1=predict(fit, test)

#Creating Confusion matrix
(conf2=table(test$incomelevel, fit1))
TP=conf2[1, 1]
FN<-conf2[1, 2]
FP<-conf2[2, 1]
TN<-conf2[2, 2]

#Calculating Accuracy
acc<-(TP+TN)/(TP+FN+FP+TN)
acc*100
#Calculate and print out the precision: prec
prec<-TP/(TP+FP)
prec*100

#Calculate and print out the recall: rec
rec<-TP/(TP+FN)
rec*100




