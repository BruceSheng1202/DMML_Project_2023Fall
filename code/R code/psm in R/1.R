data <- read.csv("heart_attack_prediction_dataset.csv")
library(pROC)
library(randomForest)
train_sub = sample(nrow(data),7/10*nrow(data))
train_data = data[train_sub,]
test_data = data[-train_sub,]
rf1 <- randomForest(Heart.Attack.Risk~.,
                                  data = train_data,
                                  ntree =500,
                                  mtry=3,
                                  importance=TRUE ,
                                  proximity=TRUE)
varImpPlot(rf1, main = "variable importance")
#对测试集进行预测
pre_ran <- predict(rf1,newdata=test_data)
#将真实值和预测值整合到一起
obs_p_ran = data.frame(prob=pre_ran,obs=test_data$Heart.Attack.Risk)
#输出混淆矩阵
table(test_data$Heart.Attack.Risk,pre_ran,dnn=c("真实值","预测值"))
#绘制ROC曲线
ran_roc <- roc(test_data$Heart.Attack.Risk,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='随机森林模型ROC曲线,mtry=3,ntree=500')



summary(data)


split_values  <- strsplit(data$Blood.Pressure, "/")
data$High.Blood.Pressure <- as.numeric(sapply(split_values, function(x) as.numeric(x[1])))
data$Low.Blood.Pressure <- as.numeric(sapply(split_values, function(x) as.numeric(x[2])))
dt <- data[, -which(names(data) == "Blood.Pressure")]
dt <- dt[,-which(names(data)=='Patient.ID')]
dt$Sex <- as.factor(dt$Sex)
dt$Diabetes <- as.factor(dt$Diabetes)
dt$Family.History <- as.factor(dt$Family.History)
dt$Smoking <- as.factor(dt$Smoking)
dt$Obesity <- as.factor(dt$Obesity)
dt$Alcohol.Consumption <- as.factor(dt$Alcohol.Consumption)
dt$Diet <- as.factor(dt$Diet)
dt$Previous.Heart.Problems <- as.factor(dt$Previous.Heart.Problems)
dt$Medication.Use <- as.factor(dt$Medication.Use)
dt$Stress.Level <- as.factor(dt$Stress.Level)
dt$Country <- as.factor(dt$Country)
dt$Continent <- as.factor(dt$Continent)
dt$Hemisphere <- as.factor(dt$Hemisphere)
dt$Heart.Attack.Risk <- as.factor(dt$Heart.Attack.Risk)
summary(dt)
set.seed(1)
index <- sample(nrow(dt),5600)
train <- dt[index,]
test <- dt[-index,]
library(tree)
tree.1 <- tree(Heart.Attack.Risk~.,train)
summary(tree.1)
plot(tree.1)

tree1.pred <- predict(tree.1,test,type='class')
table(tree1.pred,test$Heart.Attack.Risk)


library(randomForest)
set.seed(1)
bag1 <- randomForest(Heart.Attack.Risk~.,data=train,mtry=ncol(dt)-1,importance=T)
bag1.pred <- predict(bag1,newdata=dt,type='class')
table(bag1.pred,dt$Heart.Attack.Risk)


testr <- ROSE(Heart.Attack.Risk~.,data=test,seed=1)$data

set.seed(1)
rf1 <- randomForest(Heart.Attack.Risk~.,data=train,importance=T)
rf1.pred <- predict(rf1,newdata=test,type='class')
table(rf1.pred,test$Heart.Attack.Risk)
varImpPlot(rf1)
rf2 <- randomForest(Heart.Attack.Risk~.,data=train[,c(1:20,22:26)],importance=T)
rf2.pred <- predict(rf2,newdata=test,type='class')
table(rf2.pred,test$Heart.Attack.Risk)
set.seed(1)
rf11 <- randomForest(Heart.Attack.Risk~Country+BMI+Stress.Level+Age+Exercise.Hours.Per.Week,data=train,importance=T,ntree=1000)
rf11.pred <- predict(rf11,newdata=test,type='class')
table(rf11.pred,test$Heart.Attack.Risk)



library(gbm)
train.a <- train
train.a$Heart.Attack.Risk <- as.numeric(levels(train.a$Heart.Attack.Risk))[train.a$Heart.Attack.Risk]
test.a <- test
test.a$Heart.Attack.Risk <- as.numeric(levels(test.a$Heart.Attack.Risk))[test.a$Heart.Attack.Risk]
set.seed(1)
adaboost1 <- gbm(Heart.Attack.Risk~.,data=train.a,distribution='adaboost',n.trees=5000,interaction.depth = 6)
adaboost1.pred <- predict(adaboost1,newdata=test.a,ntrees=5000,type = 'response')
adaboost1.predc <- ifelse(adaboost1.pred>0.9,1,0)
table(adaboost1.predc,test$Heart.Attack.Risk)


dt0 <- dt[,c(1:19,22:26)]
set.seed(12)
index1 <- sample(nrow(dt0),nrow(dt0)*0.7)
train0 <- dt0[index1,]
test0 <- dt0[-index1,]



library(ROSE)
train0_over <- ovun.sample(Heart.Attack.Risk~.,data=train0,method='over',N=7226)$data
tree.1o <- tree(Heart.Attack.Risk~.,train0_over)
summary(tree.1o)
plot(tree.1o)

tree1o.pred <- predict(tree.1o,test0,type='class')
table(tree1o.pred,test0$Heart.Attack.Risk)

library(rpart)
to <- rpart(Heart.Attack.Risk~.,data=train_over)

rf1o <- randomForest(Heart.Attack.Risk~.,data=train0_over,importance=T)
rf1o.pred <- predict(rf1o,newdata=test0,type='class')
table(rf1o.pred,test0$Heart.Attack.Risk)


library(pROC)
p1 <- predict(rf1o,newdata = test0)
roc.curve(test0$Heart.Attack.Risk,p1)



trainr <- rose <- ROSE(Heart.Attack.Risk~.,data=train0,seed=1)$data
table(trainr$Heart.Attack.Risk)
t1 <- rpart(Heart.Attack.Risk~.,data=trainr)
pr <- predict(t1,newdata = test0,type='class')
roc.curve(test0$Heart.Attack.Risk,pr[,2])
table(pr,test0$Heart.Attack.Risk)



rf2o <- randomForest(Heart.Attack.Risk~.,data=trainr,importance=T,ntree=5000)
pr1 <- predict(rf2o,newdata=test0,type='class')
table(pr1,test0$Heart.Attack.Risk)
