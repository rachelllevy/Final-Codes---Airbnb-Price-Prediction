install.packages("dplyr")
install.packages("ggplot2")
install.packages("xgboost")
install.packages("caret")
library(dplyr)
library(ggplot2)
library(xgboost)
library(caret)


#XGBoost Dataset - factor variables, non-correlated numeric variables & distance from epicentre

drop <- c("Latitude","Longitude","Neighbourhood Cleansed","Host Neighbourhood")
cleaned_nochar <- cleaned_data_2[ , !(names(cleaned_data_2) %in% c(char_variables, drop))]
xgb_data <- as.data.frame(cbind(cleaned_nochar,dist_from_epicentre))


#Train and Test Data

set.seed(111)
idx <- sample(2,nrow(xgb_data),prob=c(0.8,0.2),replace=T)
train_xgb <- xgb_data[idx==1,]
test_xgb <- xgb_data[idx==2,]
dim(train_xgb) 


#One Hot Encoding for Train & Test

data_ohe <-as.data.frame(model.matrix(~.-1,data=train_xgb))
ohe_label <- data_ohe[,'Price']

test_ohe <- as.data.frame(model.matrix(~.-1,data=test_xgb))
test_label <- test_ohe[,'Price']



##Training XGBoost Model

dtrain <- xgb.DMatrix(as.matrix(data_ohe%>%select(-Price)),label=ohe_label)
dtest <- xgb.DMatrix(as.matrix(test_ohe%>%select(-Price)),label=test_label)


set.seed(500)

w <- list(train=dtrain,test= dtest)


##Model 1 (Best Model)

xgb_model1 <- xgb.train(data=dtrain,booster='gbtree',
                        nrounds=800,max_depth=6,eval_metric='rmse',eta=0.135,watchlist=w,early_stopping_rounds = 30) 

#Stopping. Best iteration:
#[323]	train-rmse:32.015938	test-rmse:67.942757
#[133]	train-rmse:38.535183	test-rmse:68.901184
#[169]	train-rmse:37.332573	test-rmse:68.566948



##Model 2 

xgb_model2 <- xgb.train(data=dtrain,booster='gbtree',
                        nrounds=800,max_depth=6,eval_metric='rmse',eta=0.1,watchlist=w,early_stopping_rounds = 30) 

#Stopping. Best iteration:
#  [200]	train-rmse:39.140881	test-rmse:68.702187
#  [119]	train-rmse:41.596725	test-rmse:68.499969
#  [211]	train-rmse:38.459400	test-rmse:68.347969

##Model 3 

xgb_model3 <- xgb.train(data=dtrain,booster='gbtree',nrounds=1000,max_depth=8,eval_metric='rmse',eta=0.3,watchlist=w,early_stopping_rounds = 30)  

#Stopping. Best iteration:
#  [84]	train-rmse:25.081161	test-rmse:69.532578


##Model 4

xgb_model4 <- xgb.train(data=dtrain,booster='gbtree',nrounds=1000,max_depth=4,eval_metric='rmse',eta=0.135,watchlist=w,early_stopping_rounds = 30)

#Stopping. Best iteration:
#  [210]	train-rmse:47.883202	test-rmse:68.922653



##Prediction for Test Set 


best_model <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',eta=0.1,watchlist=w,early_stopping_rounds = 30) 


pred_price <- predict(best_model,newdata = dtest,class='response')
pred_price <- round(pred_price)
head(pred_price)


##Feature Importance

imp <- xgb.importance(colnames(dtrain),model=best_model)
xgb.plot.importance(imp)





#XGBoost Dataset - NUMERIC VARIABLES ONLY


set.seed(111)
idx <- sample(2,nrow(xgb_data),prob=c(0.8,0.2),replace=T)
train_cleaned <- cleaned_nochar[idx==1,]
test_cleaned <- cleaned_nochar[idx==2,]
dim(train_cleaned) 


#One Hot Encoding for Train & Test

data_ohe <-as.data.frame(model.matrix(~.-1,data=train_cleaned))
ohe_label <- data_ohe[,'Price']

test_ohe <- as.data.frame(model.matrix(~.-1,data=test_cleaned))
test_label <- test_ohe[,'Price']



##Training XGBoost Model

dtrain <- xgb.DMatrix(as.matrix(data_ohe%>%select(-Price)),label=ohe_label)
dtest <- xgb.DMatrix(as.matrix(test_ohe%>%select(-Price)),label=test_label)


set.seed(500)

w <- list(train=dtrain,test= dtest)


##Model 1

xgb_model1 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',eta=0.135,watchlist=w,early_stopping_rounds = 30) 

#[72]	train-rmse:42.879215	test-rmse:69.294624



##Model 2 (Best Model)

xgb_model2 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',eta=0.1,watchlist=w,early_stopping_rounds = 30) 

#[127]	train-rmse:41.877975	test-rmse:69.236412


##Model 3 

xgb_model3 <- xgb.train(data=dtrain,booster='gbtree',nrounds=1000,max_depth=8,eval_metric='rmse',eta=0.3,watchlist=w,early_stopping_rounds = 30)  

#[26]	train-rmse:33.698750	test-rmse:70.635521



##Model 4

xgb_model4 <- xgb.train(data=dtrain,booster='gbtree',nrounds=1000,max_depth=4,eval_metric='rmse',eta=0.135,watchlist=w,early_stopping_rounds = 30)

#[111]	train-rmse:52.174213	test-rmse:69.885681



##Prediction for Test Set 


best_model <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',eta=0.1,watchlist=w,early_stopping_rounds = 30) 


pred_price <- predict(best_model,newdata = dtest,class='response')
pred_price <- round(pred_price)
head(pred_price)


##Feature Importance

imp <- xgb.importance(colnames(dtrain),model=best_model)
xgb.plot.importance(imp)











