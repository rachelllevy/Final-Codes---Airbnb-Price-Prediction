##Support Vector Regression

install.packages("Metrics")
install.packages("e1071")
library(e1071)
library(Metrics)


## SVR with numeric data only

svm_model <- svm(Price ~ . , data = cleaned_nochar, type = 'eps-regression')

predictYsvm <- predict(svm_model, cleaned_nochar)

svr_RMSE <- RMSE(xgb_data$Price, predictYsvm)  
print(paste('svr RMSE = ', 
            svr_RMSE))

#"svr RMSE =  63.6992573784753"


#Fine Tune SVR model

tuneResult1 <- tune.svm(Price ~ ., data = svm_Trsf, gamma = 2^(-1:1), cost = 2^(2:4))

##not enough memory to compute 


## SVR with feature selected variables

svm_model <- svm(Price ~ . , data = feature_data, type = 'eps-regression')

predictYsvm <- predict(svm_model, feature_data)

svr_RMSE <- RMSE(feature_data$Price, predictYsvm)  
print(paste('svr RMSE = ', 
            svr_RMSE))

#"svr RMSE =  64.2883867842318"


## SVR with distance from epicentre

svm_model <- svm(Price ~ . , data = my_dist_Trsf, type = 'eps-regression')

predictYsvm <- predict(svm_model, my_dist_Trsf)

svr_RMSE <- RMSE(my_dist_Trsf$Price, predictYsvm)  
print(paste('svr RMSE = ', 
            svr_RMSE))

#"svr RMSE =  64.3076531317148"
