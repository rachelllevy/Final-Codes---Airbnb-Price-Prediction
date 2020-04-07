
# load the library

install.packages("RCurl")
install.packages("caret")
install.packages("psych")
install.packages("corrplot")
library(RCurl)
library(caret)
library(psych)
library(corrplot)

##flatten square matrix 

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}


##Cor prob

cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}


#Remove character columns

drops <- c("Host Since","Name","Description","Host Verifications","Amenities","Geolocation","Features","Host Neighbourhood","Neighbourhood Cleansed","Zipcode","Property Type","Room Type","Bed Type", "Calendar Updated", "Cancellation Policy")
only_numeric <- cleaned_data[ , !(names(cleaned_data) %in% drops)]

#Create Correlations

cor <- cor(only_numeric)


#Create Table

zdf <- as.data.frame(as.table(cor))


#Find Highest Correlations 

highest_cor <- subset(zdf, abs(Freq) > 0.5)


#Which variables are highly correlated to independent variable - price?


corMasterList <- flattenSquareMatrix (cor.prob(only_numeric))
corList <- corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,10))

selectedSub <- subset(corList, (abs(cor) & j == 'Price'))
print(selectedSub)


#Of highly correlated pairs, which ones have the highest correlation to Price?

Avail_30 <- cor.test(only_numeric$Price, only_numeric$`Availability 30`, method = "pearson")


#Keep 
Accommodates
Host Listings Count 
Review Scores Rating
Availability 30
Review Scores Rating
Number of Reviews
Host Response Rate


#Remove 
Bedrooms 
Beds
Bathrooms 
Guests Included
Availability 365
Availability 60
Availablility 90
Review Scores Value 
Review Scores Accuracy
Review Scores Checkin
Reviews per Month


#Create new list

linear_drop <- c("Bedrooms","Beds","Bathrooms","Guests Included","Availability 365","Availability 60","Availability 90","Review Scores Value","Review Scores Accuracy","Review Scores Cleanliness","Review Scores Checkin","Reviews per Month","Review Scores Communication")
cleaned_data_2 <- cleaned_data[ , !(names(cleaned_data) %in% linear_drop)]





