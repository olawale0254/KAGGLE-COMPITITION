####import library
library(rpart)
library(rpart.plot)
library(rattle)
##### call in data 
mydata <- read.csv(file.choose(),header = TRUE)
###split data into test and train
index <-1:nrow(mydata)
 test_data_index <-sample(index, trunc(length(index)/3))
 test <-mydata[test_data_index,]
 train <-mydata[-test_data_index,]
###grow tree
fit <- rpart(er ~ cp + ir, data = train)
summary(fit)
##plot tree
rpart.plot(fit, # middle graph
           type=4,
           extra=101, 
           box.palette="GnBu",
           branch.lty=3, 
           shadow.col="gray", 
           nn=TRUE)
plot(fit, uniform = TRUE, main="regression tree")
text(fit, use.n = TRUE, all = TRUE, cex=.6)
par(mfrow=c(1,2))
rsq.rpart(fit)
text(fit, use.n = TRUE, all = TRUE, cex=.6)
###predict model
out1<-data.frame(test)
pred <- predict(fit,out1, method = "anova")
data.frame(pred)
accuracy1 <- colnames(pred)[max.col(pred, ties.method = c("random"))]
acc1 <- sqrt(mean((pred-test$er)^2))
acc1
acc2 <- mean(abs(pred-test$er))
acc2
######prune
printcp(fit)
##### visualize cross-validation results
plotcp(fit)
##get the optimal cp programmatically
min.xerror <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
min.xerror
##use xerror for prone tree
ptree <- prune(fit, cp=min.xerror)
###plot prune tree 
# Compute the accuracy of the pruned tree
preprun <- rpart(er ~ ir + cp, data = train, 
                 control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))
pred <- predict(preprun, test)
acc3 <- sqrt(mean((pred-test$er)^2))
acc4 <- mean(abs(pred-test$er))
acc4
### accuracy of model and prune
data.frame(acc2, acc4)

