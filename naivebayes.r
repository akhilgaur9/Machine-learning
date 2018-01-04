
data<-read.csv("lead.csv",header = T,na.strings=c("NULL","NA","na","","Na","?","Not Found"," "))
View(data)
#installing libraries e1071 for naive bayes and caret for spiltting the data
newdata<-na.omit(data)
#dim(newdata)
#View(newdata)
library(caret)
library(e1071)
#splitting the datainto 70:30
splitdata <- createDataPartition(y=newdata$Quality, p=0.7, list = F,times = 1)
train_data <- newdata[splitdata,]
#print(train_data)
test_data <- newdata[-splitdata,]
#building the model using train data
fitnb <- naiveBayes(Quality~.,data = train_data)
#predicting the model on test data
pred <- predict(fitnb,test_data)
testtable <-table(pred,test_data$Quality)
categ<-write.csv(pred,"nboutput.csv")
#checking the accuracy of the model
accuracynb <- sum(diag(testtable))/sum(testtable)
accuracynb



---------------------------------------------------------------------------------------------
data<-read.csv("lead.csv",header = T,na.strings=c("NULL","NA","na","","Na","?","Not Found"," ", stringsAsFactors=FALSE))
View(data)
#data<-data[,-6]
dim(data)
(colSums(is.na(data)))/(nrow(data))*100
set.seed(123)
newdata<- na.omit(data)
View(newdata)
dim(newdata)
attach(newdata)
data(newdata)
tab1<-xtabs(~Leadcity + Call.Status,data=newdata)
install.packages("Boruta")
install.packages("corrgram")
install.packages("Hmisc")
library(Hmisc)
library(Boruta)
library(caret)
library(e1071)
library(corrgram)
splitdata <- createDataPartition(y=newdata$Quality, p=0.7, list = F,times = 1)
train_data <- newdata[splitdata,]
print(train_data)
set.seed(123)
boruta.train <- Boruta(Quality~., data = train_data, doTrace = 2)
boruta.train
print(boruta.train)
test_data <- newdata[-splitdata,]
fitnb <- naiveBayes(Quality~.,data = train_data)
fitnb
varImp(fitnb,scale =T)
nbtestdata <- test_data
nbtestdata$fitvalues <- predict(fitnb,nbtestdata)
testtable <-table(nbtestdata$fitvalues,nbtestdata$Quality)
accuracynb <- sum(diag(testtable))/sum(testtable)
accuracynb
#data("Arthritis")
#tab <- xtabs(~Improved + Treatment, data = Arthritis)
#summary(assocstats(tab))
attach(newdata)
data(newdata)
tab1<-xtabs(~LeadCity + Call.Status,data=newdata)
install.packages("vcd")
library(vcd)

assocstats(tab1)


plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
library(randomForest)
set.seed(123)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe.train <- rfe(train_data[,1:7],train_data[,8], rfeControl=control)

-------------------------------------------------------------------------------------------
library("nnet")
model <- multinom( Quality~ ., data = train_data)
pred1 <- predict(model,test_data)
tab1 <- table(test_data$leadcategory,pred1)
conmat1 <- sum(diag(tab)/sum(tab))
conmat1





#model<-polr(Quality ~ ., data = train_data, Hess=TRUE)
fitrpart <- rpart(Quality ~ .,method="class",data=train_data)
printcp(fitrpart)
plotcp(fitrpart)
#summary(fitrpart)
testrpart<- test_data
testrpart$fitvalues <- predict(fitrpart,testrpart,type = "class")
testtable <- table(testrpart$fitvalues,testrpart$Class)
accuracyrpart <- sum(diag(testtable))/sum(testtable)
accuracyrpart

#ctree
install.packages("data.table")
library(data.table)
install.packages("party")
library(party)
fitctree <- ctree(Quality~.,data = train_data,controls = ctree_control(maxdepth = 4))
plot(fitctree)
ctreetestdata <- test_data
ctreetestdata$fitvalues <- predict(fitctree,test_data)
testtable <- table(ctreetestdata$fitvalues,ctreetestdata$Quality)
accuracyctree <- sum(diag(testtable))/sum(testtable)
accuracyctree


