install.packages('ISLR')
library(ISLR)
str(Caravan)
data<-Caravan
summary(data$Purchase)
any(is.na(data))


var(data[,1])
var(data[,2])

purchase<-data[,86]
standardized.data<-scale(data[,-86])

#Trin test split
test.index<-1:1000
test.data<-standardized.data[test.index,]
test.purchase <-purchase[test.index]


#Train
train.data<-standardized.data[-test.index,]
train.purchase<-purchase[-test.index]

#KNN Model
library(class)
set.seed(101)
predict.purchase<-knn(train.data,test.data,train.purchase,k=5)
print(head(predict.purchase))


misclass.error<-mean(test.purchase !=predict.purchase)
print(misclass.error)

##Choosing K value
predict.purchase=NULL
error.rate<-NULL

for (i in 1:20) {
  set.seed(101)
  predict.purchase<-knn(train.data,test.data,train.purchase,k=i)
  error.rate[i]<-mean(test.purchase!=predict.purchase)
}

print(error.rate)


#Visualize elbow method
library(ggplot2)
k.values<-1:20
error.df<-data.frame(error.rate,k.values)
print(error.df)

pl<-ggplot(error.df,aes(error.rate,k.values))+geom_point()+geom_line()
print(pl)
