install.packages("rbenchmark")
library(rbenchmark)

##Efficiency Benchmark 


Efficiency_benchmark <- benchmark("lm" = {
  model_mlr <- lm(Price~., data= train)
  prediction <- predict(model_mlr, interval="prediction", newdata = test)
},
"svm model" = {
  svm_model <- svm(Price ~ . , data = cleaned_nochar, type = 'eps-regression')
  predictYsvm <- predict(svm_model, cleaned_nochar)
},
"XGBoost" = {
  xgb_model2 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',eta=0.1,watchlist=w,early_stopping_rounds = 30) 
},
replications = 10,
columns = c("test", "replications", "elapsed","relative", "user.self", "sys.self"))



# Efficiency_benchmark
#       test replications elapsed relative user.self sys.self
#1        lm           10    0.10      1.0      0.09     0.00
#2 svm model           10  464.73   4647.3    451.42     0.58
#3   XGBoost           10   64.85    648.5    166.28    11.87