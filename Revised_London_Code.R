summary(London)
na.omit(London)
options(scipen = 999)
London1=London%>%
  select(Price,type="House Type",Area="Area in sq ft",Nbed="No. of Bedrooms",Nbath="No. of Bathrooms",Nrec="No. of Receptions",Region="Region")%>%
  mutate(type=recode(type,"Flat / Apartment"="Flat","House"="House","New development"="UnderConstruction","Duplex"="Duplex","Bungalow"="Bunglow","Mews"="Mews","Penthouse"="Penthouse","Studio"="Studio"))
London1$Price=London1$Price/1000
London1$type=as.factor(London1$type)
London1$Region=as.factor(London1$Region)

View(London1)
str(London1)
#Price
summary(London1$Price)
hist(London1$Price,main="Property Price(in thousands)",xlab="Price",col="skyblue")

#Avg Price vs Independent Variables
#1.Region
ggplot(London1,aes(as.factor(Region),Price))+geom_jitter(aes(colour=Region,alpha=0.7))+geom_boxplot(width=0.6,size=0.6,alpha=.6,outlier.color = NA)+scale_color_manual(breaks=c("East","West","North","South","NE","NW","SE","SW"),values=c("yellow","blue","red","orange","darkblue","gold","skyblue","grey"))+labs(title="Price vs Region")+xlab("Region")+ylab("Price")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))
aov1 <- aov(Price ~ Region, data = London1)
# Summary of the analysis
summary(aov1)
TukeyHSD(aov1)
plot(TukeyHSD(aov1),las=1)

#2.Type

ggplot(London1,aes(type,Price))+geom_jitter(aes(colour=type,alpha=0.7))+geom_boxplot(width=0.6,size=0.6,alpha=.6,outlier.color = NA)+labs(title="Type vs Price")+xlab("Type")+ylab("Price")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))
aov2 <- aov(Price ~ type, data = London1)
# Summary of the analysis
summary(aov2)
TukeyHSD(aov2)
plot(TukeyHSD(aov2),las=1)

#3.Area
ggplot(London1,aes(Area,Price,col=Nbed))+geom_jitter()+labs(title="Area vs Price")+xlab("Area")+ylab("Price")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))

#4.Nbed

ggplot(London1,aes(as.factor(Nbed),Price,fill=as.factor(Nbed)))+geom_boxplot(width=0.6,size=0.6,alpha=.6,outlier.color = NA)+labs(title="Nbed vs Price")+xlab("Nbed")+ylab("Price")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))
aov3 <- aov(Price ~ Nbed, data = London1)
# Summary of the analysis
summary(aov3)

ggplot(London1,aes(as.factor(Nbed),Price))+geom_jitter(col="red")+labs(title="Number of Bedroom vs Price")+xlab("Nbedrooms")+ylab("Price")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))
ggplot(London1,aes(as.factor(Nbath),Price))+geom_jitter(col="blue")+labs(title="Number of Bathroom vs Price")+xlab("NBath")+ylab("Price")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))
ggplot(London1,aes(as.factor(Nrec),Price))+geom_jitter(col="blue")+labs(title="Number of Reception vs Price")+xlab("NRec")+ylab("Price")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white"),axis.line = element_line(color='black'))


#Correlations  
str(London1)
London1$type=as.numeric(as.factor(London1$type))
London1$Region=as.numeric(as.factor(London1$Region))
cor(London1,method="pearson")
round(cor(London1),2)
cor(London1$Price,London1$Area)

library("corrplot")
corrplot.mixed(cor(London1),lower="number",upper="pie",title="Correlation Matrix",tl.srt =+45,tl.cex=0.8)
library(psych)
pairs.panels(London1,scale=FALSE,stars =TRUE,cex.cor=3,cex.labels=1)



#Linear Regression Models

#Model1
mymodel1=lm(Price~.,London1)
summary(mymodel1)
vif(mymodel1)
plot(mymodel1)

#Model2
mymodel2=lm(Price~.-Nrec-Nbath,London1)
summary(mymodel2)
vif(mymodel2)
plot(mymodel2)

#Model3
mymodel3=lm(Price~Area+Nbed+Region,London1)
summary(mymodel3)
vif(mymodel3)
plot(mymodel3)
View(London1)

#Model4
mymodel4=lm(Price~Area+Region,London1)
summary(mymodel4)
vif(mymodel4)
plot(mymodel4)
