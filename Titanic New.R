rm(list=ls())

#Importing files

titanic<-read.csv("F:/Project/Titanic/train.csv",header=T)
test<-read.csv("F:/Project/Titanic/test.csv",)
head(titanic)
head(test)

#Eleminating row 62 and 830 as they have no data corresponding to Emabarked variable
titanic<-titanic[-c(62,830),]

#attaching dataset

attach(titanic)

#Checking the type of different variable

str(titanic)

#Variable declaration ...
embarked<-as.integer(Embarked)
sex<-as.integer(Sex);pclass<-Pclass;parch<-Parch
fare<-Fare;sibsp<-SibSp;survived<-Survived
temp<-rep();j=1;temp2<-rep();k=1

#Calculating median age for male and female category to replace missing values.

for(i in 1:887){
if(sex[i]==2 & pclass[i]==3){
temp[j]<-Age[i]
j=j+1
}
if(sex[i]==1 & pclass[i]==3){
temp2[k]<-Age[i]
k=k+1
}
}
median3m<-median(temp,na.rm=T)
median3f<-median(temp2,na.rm=T)

#replacing missing values for the variable age

for(i in 1:length(Age)){
if(is.na(Age[i]) & Sex[i]=="female"){Age[i]=median3f}
if(is.na(Age[i]) & Sex[i]=="male"){Age[i]=median3m}
}

age<-Age
#Contingency table and data visualization
table1<-xtabs(~survived+sex+pclass,titanic)
ftable(table1)
fivenum(fare)
fivenum(age)
summary(fare==0)

#Finding the passengeres who have paid 0 fare and maximum fare

as.character(Name[which(fare==0)])
as.character(Name[which(fare==max(fare))])

#finding the passengers who have atleat 6 family members

length(which(parch+sibsp>=6))
sort(as.character(Name[which(parch+sibsp>=6)]))

#Finding the ticket numbers on which more than 5 passengers travelled.

tic<-as.vector(names(table(Ticket))[table(Ticket)>5])
n=0;

#Finding the passenger names who corresponds to the tickets on which more than 5 travelled
for(i in 1:6){
for(j in 1:length((Name))){
if(tic[i]==(Ticket)[j]){
print(as.vector(Name[j]))
n=n+1
}
}}

#Replacing missing values for the mean variable in test data

test[,5][which(is.na(test[,5]) & test[,4]=="male")]  <-median3m
test[,5][which(is.na(test[,5]) & test[,4]=="female")]<-median3f
test<-test[,-c(1,3,8,10,11)]
colnames(test)<-c("pclass","Sex","age","SibSp","parch","fare")
test$fare[which(is.na(test$fare))]<-mean(test$fare,na.rm=T)

#Fitting logistic model on the data 

m1<-glm(survived~Sex+pclass+parch+age+fare+SibSp,family="binomial"(link="logit"))

#x<-as.vector(round(predict(m1,test,type="response")))
x<-as.numeric((predict(m1,test,type="response")>0.6))

write.csv(data.frame(PassengerId=892:1309,x),file.choose())

#-------------------------------

