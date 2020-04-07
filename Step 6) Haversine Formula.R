install.packages("geosphere")
install.packages("caret")
library(geosphere)
library(caret)

#Calculate Haversine Distance from Downtown Toronto (epicentre)

Toronto_lat <- 43.651070
Toronto_long <- -79.347015

dist_from_epicentre <- as.data.frame(distVincentyEllipsoid(cleaned_data_2[,10:11],  c(Toronto_lat, Toronto_long))/ 1000)  ## Much more accurate*
## [1] 110574.4 221149.5


##Drop unncessary location fields, merge dataframes and dummify Zipcode and other factor variables 

distance_variables <- c("Latitude","Longitude","Host Neighbourhood","Neighbourhood Cleansed")
ddata <- cleaned_data_2[ , !(names(cleaned_data_2) %in% c(distance_variables,char_variables))]

distance_data <- as.data.frame(cbind(ddata,dist_from_epicentre))

my_dist_dmy <- dummyVars(" ~ .", data = distance_data)
my_dist_Trsf <- data.frame(predict(my_dist_dmy, newdata = distance_data))

str(my_dist_Trsf)


#Train Distance Data 

d_train <- sample(nrow(my_dist_Trsf), floor(nrow(my_dist_Trsf)*0.8))
distance_train <- my_dist_Trsf[d_train,]
distance_test <- my_dist_Trsf[-d_train,]

distance_model <- lm(Price ~., data = my_dist_Trsf)
summary(distance_model)
#Adjusted R-squared:  0.4936

distance_prediction <- predict(distance_model, interval="prediction", newdata = distance_test)


distance_errors <- distance_prediction[,"fit"] - distance_test$Price

distance_rmse <- sqrt(sum((distance_prediction[,"fit"] - distance_test$Price)^2)/nrow(distance_test))
distance_rmse
#65.4595
#66.3857

distance_rel_change <- 1 - ((distance_test$Price - abs(errors)) / distance_test$Price)

distancepred25 <- table(distance_rel_change<0.25)["TRUE"] / nrow(distance_test)
table(distance_rel_change)
paste("RMSE:", distance_rmse)
paste("PRED(25):", distancepred25)
#"PRED(25): 0.442307692307692"
