library(ROCR)
library(e1071)
library(ggplot2)
library(dplyr)
library(rpart)
set.seed(190612)

#데이터로드

cardata<-read.csv('D:/HUFS/4-1/통계적기계학습/프로젝트/rawdata/car/car_data.csv',header=T)

head(cardata)


str(cardata)

#data Structure
cardata
cardata_sub1<-cardata %>%
  group_by(class) %>%
  summarise(n=n()) %>%
  arrange(desc(class))

cardata_sub1
ggplot(data=cardata_sub1, aes(x=class,y=n))+geom_bar(stat='identity',fill='skyblue')


#data split
train_index <- sample(1:nrow(cardata), round(nrow(cardata)*0.7))
cardataTrain <- cardata[train_index,]
cardataTest <- cardata[-train_index,]


#decision tree

tree.fit<-rpart(class~., data=cardataTrain)
{plot(tree.fit, main='Decision Tree')
  text(tree.fit)}

dt_pred<-predict(tree.fit, cardataTest, type='class')
dt_table<-table(actual=cardataTest$class, predict=dt_pred)
dt_table
sum(diag(dt_table))/sum(dt_table)


#Naive Bayes
nb<-naiveBayes(class~., data=cardataTrain)
nb_pred<-predict(nb, cardataTest, type='class')

nb_table<-table(actual=cardataTest$class, predict=nb_pred)
nb_table
sum(diag(nb_table))/sum(nb_table)

#SVM trainig
tobj2 <- tune.svm(class ~ ., data = cardataTrain, 
                 cost = 10^(-3:3), gamma = 10^(-3:3))
tobj2$best.parameters


svm.model <- svm(class ~ ., data = cardataTrain, 
                 cost = tobj2$best.parameters$cost, 
                 gamma = tobj2$best.parameters$gamma)


svm_pred <- predict(svm.model, cardataTest[,-7])
svm_pred

svm_table<-table(svm_pred, cardataTest$class) # confusion matrix
svm_table
sum(diag(svm_table))/sum(svm_table)




