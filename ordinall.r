# Reading the csv file
data<-read.csv("UploadforR.csv",header = T)
# installing Hmisc package for Ordinal Logistic regression
#installing package gmodels for building Cross table
install.packages("Hmisc")
install.packages("gmodels")
#installing library MASS and Hmisc
library(MASS)
library(Hmisc)
#installing library caret for splitting the data
library(caret)
library(gmodels)
#converting the columns from integer to factor
cols <- c("leadcategory","course","region","Callstatus","Mainsource")
data[cols] <- lapply(data[cols],factor)
#splitting the data into 70:30 
splitdata <- createDataPartition(y=data$leadcategory, p=0.7, list = F,times = 1)
train_data <- data[splitdata,]
test_data<-data[-splitdata,]
#building the model using polr function in MASS package with train data
leadmodel<- polr(leadcategory ~ course+region+Callstatus+Mainsource,
                 data = train_data,Hess=T,method = "logistic")
              summary(leadmodel)
#predicting the model with the test data
pred <- predict(leadmodel,data=test_data,type="p")
pred
#exp(cbind(OR=coef(leadmodel),confint(leadmodel)))
 
#dim(pred)
#View(pred)
#writing the output in a csv file
categ<-write.csv(pred,"ordinal.csv")





                



tab<- CrossTable(true=test_data$leadcategory,pred)
tab1<-unlist(tab)
accuracy<- sum(diag(tab1))/sum(tab1)
accuracy


ctable <- coef(summary(Newleadmodel1))
ctable

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable



