#Install Packages

install.packages('RCurl')
install.packages('MASS')
install.packages('leaps')
library(RCurl) # getURL 
library(MASS) # stepwise regression
library(leaps) # all subsets regression

#Remove char variables

char_variables <- c("Name","Description","Host Verifications","Amenities","Geolocation","Features")
factor_variables <- c("Host Neighbourhood","Neighbourhood Cleansed","Zipcode","Property Type","Room Type","Bed Type","Calendar Updated","Cancellation Policy")
cleaned_nochar <- cleaned_data_2[ , !(names(cleaned_data_2) %in% c(char_variables,factor_variables))]


#Create training and test set - 80% and 20% 

rn_train <- sample(nrow(cleaned_nochar), floor(nrow(cleaned_nochar)*0.8))
train <- cleaned_nochar[rn_train,]
test <- cleaned_nochar[-rn_train,]

#Build a multiple linear regression model to predict the 'price' variable. Will train the model on the training set and do the prediction on the test set.

model_mlr <- lm(Price~., data= train)
prediction <- predict(model_mlr, interval="prediction", newdata = test)
model_mlr

#Plot errors on a histogram. 

errors <- prediction[,"fit"] - test$Price
hist(errors)

#Compute the root mean square error and find the percentage of cases with less than 25% error.

rmse <- sqrt(sum((prediction[,"fit"] - test$Price)^2)/nrow(test))
rmse
#72.56279
#66.49682
#69.32965
rel_change <- 1 - ((test$Price - abs(errors)) / test$Price)

pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
table(rel_change)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)
#"PRED(25): 0.433306055646481"
#"PRED(25): 0.43126022913257"

#Forward Selection

full <- lm(Price~.,data=cleaned_nochar)
null <- lm(Price~1,data=cleaned_nochar)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), direction= "forward", trace=TRUE)
summary(stepF)

## ForwardStep:  AIC=103890

stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)

# BackwardStep: AIC =103890 


#Train forward and backward

feature_variables <- c("Price","Accommodates","Latitude","Longitude","Cleaning Fee","Availability 30","Number of Reviews","Review Scores Location","Review Scores Rating","Host Response Rate","Extra People")
feature_data <- cleaned_data_2[ , (names(cleaned_data_2) %in% feature_variables)]

f_train <- sample(nrow(feature_data), floor(nrow(feature_data)*0.8))
feature_train <- feature_data[f_train,]
feature_test <- feature_data[-f_train,]
  
feature_model <- lm(Price ~., data = feature_train)
summary.lm(feature_model)

feature_prediction <- predict(feature_model, interval="prediction", newdata = feature_test)

feature_errors <- feature_prediction[,"fit"] - feature_test$Price

feature_rmse <- sqrt(sum((feature_prediction[,"fit"] - feature_test$Price)^2)/nrow(feature_test))
feature_rmse
#71.54741
#68.5377
#65.57387
feature_rel_change <- 1 - ((feature_test$Price - abs(errors)) / feature_test$Price)

featurepred25 <- table(feature_rel_change<0.25)["TRUE"] / nrow(feature_test)
table(feature_rel_change)
paste("RMSE:", feature_rmse)
paste("PRED(25):", featurepred25)
#"PRED(25): 0.449263502454992"


