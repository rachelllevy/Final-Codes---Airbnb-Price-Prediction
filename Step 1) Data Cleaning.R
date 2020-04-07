#Import Dataset 

library(readr)
airbnb_listings_Toronto_fromwebsite <- read_csv("C:/Users/levyr/Desktop/CKME 136/airbnb-listings-Toronto_fromwebsite.csv")

#Name dataset & save as data.frame

airbnb_data <- airbnb_listings_Toronto_fromwebsite
as.data.frame(airbnb_data)

#Number of rows in dataset

nrow(airbnb_data)

#Check for Missing Values 

colSums(is.na(airbnb_data))

#Drop variables with too many missing fields and irrelevant or duplicate features

df <- airbnb_data[,!(names(airbnb_data) %in% c("Summary","ID","Last Scraped","Neighbourhood","Calendar last Scraped","Medium Url","Street","First Review","Last Review","Jurisdiction Names","Listing Url","Scrape ID","Experiences Offered","Thumbnail Url","Medium URL","Picture Url","XL Picture Url","Host ID","Host URL","Host Name","Host Response Time","Host About","Host Location","Host Thumbnail Url","Host Acceptance Rate","Host Picture Url","City","State","Market","Smart Location","Country Code","Country","License","Jurisdiction","Host Total Listings Count","Calculated host listings count","Neighbourhood Group Cleansed","Neighborhood Overview", "Square Feet", "Space", "Notes", "Transit", "Access", "Interaction", "House Rules", "Host About", "Weekly Price", "Monthly Price", "Security Deposit", "Has Availability"))]
colSums(is.na(df))

str(df)
summary(df)


#Change from characters to factors 

df$`Room Type`<-as.factor(df$`Room Type`)
df$`Host Neighbourhood`<-as.factor(df$`Host Neighbourhood`)
df$`Neighbourhood Cleansed`<- as.factor(df$`Neighbourhood Cleansed`)
df$`Property Type` <- as.factor(df$`Property Type`)
df$`Bed Type`<- as.factor(df$`Bed Type`)
df$`Calendar Updated`<- as.factor(df$`Calendar Updated`)
df$`Cancellation Policy`<- as.factor(df$`Cancellation Policy`)

#Change from characters to dates

df$`Host Since`<- as.Date(df$`Host Since`, "%m/%d/%y")
df$`Host Since`<- as.Date(df$`Host Since`, "%m/%d/%y")


###Zip code Cleaning###



#convert all to upper case

df$Zipcode <- toupper(df$Zipcode)

#Sub postal codes that start with ON: 
  
df$Zipcode <- gsub("^ON", "", df$Zipcode)


#Remove rows where zip code starts with a space, bullet or has less than 3 characters: 
  
df$Zipcode <- gsub("^ ", "", df$Zipcode)


#Take only first three characters of the ZIP column: 

df$Zipcode <- substr(df$Zipcode, 0, 3)

#Remove any Zipcodes less than 3 characters 

df = df[(which(nchar(df$Zipcode) == 3)),]


#convert Zip code to a factor 

df$Zipcode <- as.factor(df$Zipcode)


### Missing Values ### 


colSums(is.na(df))
summary(df)

#Replace numeric features' missing values with the feature's mean


df_2 <- df
df_2$`Review Scores Rating`[is.na(df_2$`Review Scores Rating`)] <- mean(df_2$`Review Scores Rating`, na.rm = TRUE)
df_2$`Review Scores Accuracy`[is.na(df_2$`Review Scores Accuracy`)] <- mean(df_2$`Review Scores Accuracy`, na.rm = TRUE)
df_2$`Review Scores Cleanliness`[is.na(df_2$`Review Scores Cleanliness`)] <- mean(df_2$`Review Scores Cleanliness`, na.rm = TRUE)
df_2$`Review Scores Checkin`[is.na(df_2$`Review Scores Checkin`)] <- mean(df_2$`Review Scores Checkin`, na.rm = TRUE)
df_2$`Review Scores Communication`[is.na(df_2$`Review Scores Communication`)] <- mean(df_2$`Review Scores Communication`, na.rm = TRUE)
df_2$`Review Scores Location`[is.na(df_2$`Review Scores Location`)] <- mean(df_2$`Review Scores Location`, na.rm = TRUE)
df_2$`Review Scores Value`[is.na(df_2$`Review Scores Value`)] <- mean(df_2$`Review Scores Value`, na.rm = TRUE)
df_2$`Review Scores Location`[is.na(df_2$`Review Scores Location`)] <- mean(df_2$`Review Scores Location`, na.rm = TRUE)
df_2$`Cleaning Fee`[is.na(df_2$`Cleaning Fee`)] <- mean(df_2$`Cleaning Fee`, na.rm = TRUE)
df_2$`Host Response Rate`[is.na(df_2$`Host Response Rate`)] <- mean(df_2$`Host Response Rate`, na.rm = TRUE)
df_2$`Reviews per Month`[is.na(df_2$`Reviews per Month`)] <- mean(df_2$`Reviews per Month`, na.rm = TRUE)

colSums(is.na(df_2))


#Replace missing Host Neighorhood with "Missing" category 

df_4 <- df_2
df_4$`Host Neighbourhood` <- as.character(df_4$`Host Neighbourhood`)
df_4$`Host Neighbourhood`[which(is.na(df_4$`Host Neighbourhood`))] <- "Missing"
df_4$`Host Neighbourhood` <- as.factor(df_4$`Host Neighbourhood`)

colSums(is.na(df_4))

#Remove remaining missing values

cleaned_data <- df_4[complete.cases(df_4),]
as.data.frame(cleaned_data)


##Remove unncessary factor levels

install.packages("plyr")
install.packages("dplyr")
library(plyr)
library(dplyr)

#Property Type has too many levels (only 4 are worthwhile)

levels(cleaned_data$`Property Type`) <- c("Apartment","Other","Other","Other","Other","Other","Other","Condominium","Other","Other",
                                           "Other","Other","House","Other","Other",
                                           "Other","Other","Other","Other","Other",
                                           "Other")

#Calendar Updated 
                    
levels(cleaned_data$`Calendar Updated`) <- c("1 week ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",
                                          ">Month Ago",">Month Ago",">Month Ago","Few days ago",">Month Ago","2 weeks ago",">Month Ago",">Month Ago",
                                          ">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",
                                          "Few days ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",
                                          ">Month Ago",">Month Ago",">Month Ago","1 week ago",">Month Ago",">Month Ago",">Month Ago",">Month Ago",
                                          "1 week ago",">Month Ago",">Month Ago",">Month Ago","1 week ago",">Month Ago",">Month Ago",">Month Ago",
                                          ">Month Ago",">Month Ago",">Month Ago",">Month Ago","1 week ago",">Month Ago","today","yesterday")

count(cleaned_data$`Calendar Updated`)

str(cleaned_data)
colSums(is.na(cleaned_data))