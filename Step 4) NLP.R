#Install Packages

install.packages("tm")
install.packages("text2vec")
install.packages("SnowballC")
install.packages("slam")
library(tm)
library(text2vec)
library(SnowballC)
library(slam)

install.packages("Matrix")
library(Matrix)

#Remove commas with spaces

NLP_df <- cleaned_data
NLP_df$Description <- gsub(",", " ", NLP_df$Description)
NLP_df$Amenities <- gsub(",", " ", NLP_df$Amenities)
NLP_df$`Host Verifications` <- gsub(",", " ", NLP_df$`Host Verifications`)
NLP_df$Name <- gsub(",", " ", NLP_df$Name)
NLP_df$Features <- gsub(",", " ", NLP_df$Features)


#Create Corpus

Corpus_name <- Corpus(VectorSource(NLP_df$Name))
Corpus_desc <- Corpus(VectorSource(NLP_df$Description))
Corpus_host_ver <- Corpus(VectorSource(NLP_df$`Host Verifications`))
Corpus_amenities <- Corpus(VectorSource(NLP_df$Amenities))
Corpus_features <- Corpus(VectorSource(NLP_df$Features))

#Clean Text

cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus, tolower)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords())
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, stemDocument)
}

Corpus_name_clean <- cleanCorpus(Corpus_name)
Corpus_desc_clean <- cleanCorpus(Corpus_desc)
Corpus_host_ver_clean <- cleanCorpus(Corpus_host_ver)
Corpus_amenities_clean <- cleanCorpus(Corpus_amenities)
Corpus_features_clean <- cleanCorpus(Corpus_features)

inspect(Corpus_amenities_clean[1:3])


#Create a TDM applying TF-IDF weighting instead of term frequency

Desc_tdm = TermDocumentMatrix(Corpus_desc_clean,
                         control = list(weighting = weightTfIdf))
Desc_tdm2 <- removeSparseTerms(Desc_tdm, 0.55)


Name_tdm = TermDocumentMatrix(Corpus_name_clean,
                              control = list(weighting = weightTfIdf))
Name_tdm2 <- removeSparseTerms(Name_tdm, 0.85)

#Will not be using Name_tdm2 in further analysis


Host_Ver_tdm = TermDocumentMatrix(Corpus_host_ver_clean,
                              control = list(weighting = weightTfIdf))
Host_Ver_tdm2 <- removeSparseTerms(Host_Ver_tdm, 0.5)


amenities_tdm = TermDocumentMatrix(Corpus_amenities_clean,
                                  control = list(weighting = weightTfIdf))
amenities_tdm2 <- removeSparseTerms(amenities_tdm, 0.5)


features_tdm = TermDocumentMatrix(Corpus_features_clean,
                                   control = list(weighting = weightTfIdf))
features_tdm2 <- removeSparseTerms(features_tdm, 0.5)


#Frequent terms

freq = rowSums(as.matrix(Desc_tdm2))
head(freq)

#See the ten most frequent terms

tail(sort(freq), n = 10)

#Transpose Matrix and create new dataset with TF-IDF and price



features_tdm3 <- data.matrix(features_tdm2)
Matrix(features_tdm3, sparse = TRUE)
features <- as.data.frame(t(features_tdm3))

amen_tdm3 <- data.matrix(amenities_tdm2)
Matrix(amen_tdm3, sparse = TRUE)
amenities <- as.data.frame(t(amen_tdm3))

Host_Ver_tdm3 <- data.matrix(Host_Ver_tdm2)
Matrix(Host_Ver_tdm3, sparse = TRUE)
host_verification <- as.data.frame(t(Host_Ver_tdm3))

desc_tdm3 <- data.matrix(Desc_tdm2)
Matrix(desc_tdm3, sparse = TRUE)
description <- as.data.frame(t(desc_tdm3))

processed_NLP <- cbind(features,amenities,host_verification,description)
Price <- cleaned_data$Price 
NLP_price <- as.data.frame(cbind(processed_NLP,Price))

names(NLP_price)[9] <- "kitchen_1"
names(NLP_price)[33] <- "kitchen_2"
names(NLP_price)[3] <- "locat_1"
names(NLP_price)[35] <- "locat_2"

lm <- lm(Price ~ ., data = NLP_price)
summary(lm)
