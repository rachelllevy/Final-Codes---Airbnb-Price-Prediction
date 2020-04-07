#Load Library

install.packages("pls")
library(pls)
install.packages("clusterSim")
library(clusterSim)

#create test and train

rn_train <- sample(nrow(NLP_price), floor(nrow(NLP_price))*0.8)
NLP_train <- NLP_price[rn_train,]
NLP_test <- NLP_price[-rn_train,]

#Perform PCA on indepedent variables & Transform data 

NLP_df1 <-NLP_price[,1:39]
NLP_y <- NLP_price[,40]

#Normalize data

NLP.norm <- data.Normalization(NLP_df1, type="n1", normalization="column")
NLP.y.norm <- data.Normalization(NLP_y, type="n1", normalization="column")

#PCA and outputs

NLP.pca.normdata <- prcomp(NLP.norm, scale = TRUE, center= TRUE)
NLP.pca.normdata$rotation
head(NLP.pca.normdata$x)


#Plot the proportion of variance explained by each component. How many components will you choose to capture maximum variability in the data set?

plot(NLP.pca.normdata, type = "l", main='with data normalization')
plot(summary(NLP.pca.normdata)$importance[3,])

#Scatterplots showing the relation between PCs and y

pcs <- as.data.frame(NLP.pca.normdata$x)
plot(NLP.y.norm, pcs$PC1)
plot(NLP.y.norm, pcs$PC2)
plot(NLP.y.norm, pcs$PC3)
plot(NLP.y.norm, pcs$PC4)


#Regression

ols.data <- cbind(NLP.y.norm, pcs)
lmodel_PCA <- lm(NLP.y.norm ~ ., data = ols.data)
summary(lmodel_PCA)

beta.Z <- as.matrix(lmodel_PCA$coefficients[2:40])
V <- as.matrix(NLP.pca.normdata$rotation)
beta.X <- V %*% beta.Z
beta.X

NLP_test.norm <- data.Normalization(NLP_test[,1:39])
NLP_test.norm.y <- data.Normalization(NLP_test[,40])

fit <- pcr(NLP.y.norm ~., data = cbind(NLP.norm, NLP.y.norm))
y.pred.test1 <- predict(fit,newdata=NLP_test.norm)
y.pred.test1%>%  dim
#[1] 2444    1   39

y.pred.test1 <- y.pred.test1[1:2444,1,39]
dim(y.pred.test1) <- c(2444,1)

pred.test2 <- as.matrix(NLP_test.norm)
y.pred.test2 <- pred.test2 %*% beta.X

head(y.pred.test1)
head(y.pred.test2)
plot(y.pred.test1, y.pred.test2)

validationplot(fit, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 12, col = "blue", lty = 3)

validationplot(fit, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 12, col = "blue", lty = 3)
