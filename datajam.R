setwd('Dropbox/Datajam/')
data_value = read.csv("property_values.csv", header=TRUE)
data_lihtc = read.csv("LIHTC.csv", header=TRUE)
data_demographics = read.csv("demographics.csv", header=TRUE)
data_demographics = subset(data_demographics, City=="Charlotte")
#---------------------------time series-------------------------------------
data_value=as.matrix(data_value)
data_value=t(data_value)
colnames(data_value) = data_value[1, ]
dim(data_value)
data_value = data_value[6:266,]
plot(ts(data_value[,1]))

#---------------------------classification-------------------------------------
data_demographics$LIHTC.Present...0...No..1...Yes. <- as.factor(data_demographics$LIHTC.Present...0...No..1...Yes.)
n_test = round(nrow(data_demographics)/5)
index_test = sample(1:nrow(data_demographics), n_test, replace=FALSE)
train = c(1:nrow(data_demographics))[-index_test]
test_data = data_demographics[index_test,4:17]
train_data = data_demographics[-index_test,4:17]
dim(test_data) #Check to make sure dimensions are correct
dim(train_data)
group.sizes = tapply(rep(1,nrow(train_data)), train_data$LIHTC.Present...0...No..1...Yes., sum)
train_pi = group.sizes/sum(group.sizes)
#Logistic Regression
require(nnet)
logit_model = multinom(LIHTC.Present...0...No..1...Yes. ~ ., data = train_data, maxit = 200)
summary(logit_model)
logit_res = summary(logit_model)
tratio = logit_res$coefficients/logit_res$standard.errors
print(2*pt(abs(tratio), df=logit_res$edf, lower.tail=F), digits=4) #all significant
logit.pred = 0
for (i in 1:6){
  logit.pred = logit.pred+logit_res$coefficients[i+1]*test_data[,i+3]
}
logit.pred = logit.pred+logit_res$coefficients[1]
logit.pred = exp(logit.pred)/(1+exp(logit.pred))
logit.pred = round(logit.pred, 2)
APER.logit = mean(logit.pred != test_data$LIHTC.Present...0...No..1...Yes.)

#Linear Discriminant Analysis
require(klaR)
out_lda <- lda(LIHTC.Present...0...No..1...Yes. ~ ., data_demographics[,4:9], prior=c(train_pi[1], train_pi[2]), subset=train)
names(out_lda) #what results we can view from LDA
lda.pred = predict(out_lda, test_data)$class
lda.pred
table(pred=lda.pred, true=test_data$LIHTC.Present...0...No..1...Yes.)
APER.lda = mean(lda.pred != test_data$LIHTC.Present...0...No..1...Yes.)

#Quadratic Discriminant Analysis
out_qda = qda(Y ~ ., dat, prior = c(train_pi[1], train_pi[2]), subset=train)
qda.pred = predict(out_qda, test_data)$class
table(pred=qda.pred, true = test_data$Y)
APER.qda = mean(qda.pred != test_data$Y)

#K Nearest Neighbor
library(class)
out_KNN15 = knn(train = train_data[,4:9], test = test_data[,4:9], cl=train_data$LIHTC.Present...0...No..1...Yes., k=15)
table(pred=out_KNN15, true=test_data$LIHTC.Present...0...No..1...Yes.)
APER.KNN15 = mean(out_KNN15 != test_data$LIHTC.Present...0...No..1...Yes.)
#Using Cross Validation to choose the optimal number of neighbors
misclass.train = rep(NA, 50)
misclass.test = rep(NA, 50)
for (k in 2:50){
  out = knn(train_data[,4:9], train_data[,4:9], train_data[,5], k=k)
  out.test = knn(train_data[,4:9], test_data[,4:9], train_data[,5], k=k)
  misclass.train[k] = mean(out != train_data[,5])
  misclass.test[k] = mean(out.test != test_data[,5])
}
plot(1:50, misclass.train, type="b", lwd=2, ylim=c(0, .5), cex=.2, 
     xlab="K", ylab="misclassification error")
lines(1:50, misclass.test, type="b", lwd=2, lty=2, col="red", cex=0.2)
legend("topright", lwd=c(2,2), lty=c(2,1), legend=c("Test", "Training"), col=c("red", "black"))
lowest_k = which(misclass.test==min(misclass.test, na.rm=T))
misclass.test[lowest_k]

#Classification Tree
library(tree)
out_tree = tree(LIHTC.Present...0...No..1...Yes. ~., data=data_demographics[4:9], subset=train)
plot(out_tree)
text(out_tree)
#prediction
pred1 = predict(out_tree, test_data, type="class")
table(pred=pred1, true=test_data$LIHTC.Present...0...No..1...Yes.)
mean(pred1 != test_data$LIHTC.Present...0...No..1...Yes.)

#Support Vector Machine
library(e1071)
svm_model = svm(LIHTC.Present...0...No..1...Yes.~., data=data_demographics[4:9], subset=train, probability = TRUE, cost=100) 
print(svm_model)
summary(svm_model)
pred1 = predict(svm_model, test_data) 
table(pred=pred1, test_data$LIHTC.Present...0...No..1...Yes.)  ## confusion matrix
mean(pred1 != test_data$LIHTC.Present...0...No..1...Yes.)
pred = predict(svm_model, test_data, probability = TRUE)
attr(pred, "probabilities")[1:10,] ## provides probability of each class

#
#---------------------------simple regression-----------------------------------
df_new = read.csv('new.csv', header=TRUE)
names(df_new)
dim(df_new)
model = lm(Dec.17_y-Apr.96_y~City_x+Size.Rank_x+Dec.17_x, df_new)
summary(model)


