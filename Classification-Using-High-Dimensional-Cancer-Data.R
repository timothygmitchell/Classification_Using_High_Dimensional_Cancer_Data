# Data was downloaded March 30, 2020 from the UCI Machine Learning repository:
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)

########################### IMPORT DATA #################################################

# 'wdbc' = Wisconsin diagnostic breast cancer data

url="https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
wdbc <- read.csv(url, header = F)[,-1] # remove index
wdbc[,1] <- factor(wdbc[,1])
colnames(wdbc) <- paste("V", 1:31, sep="")
m <- data.matrix(wdbc) # as matrix

########################### EDA AND VISUALIZATION #######################################

# 2 kinds of tumors: 357 benign and 212 malignant
unique(m[,1])
sum(m[,1] == 1)
sum(m[,1] == 2)

# Visualize data
boxplot(m[,2:31], main="Boxplot of Independent Variables",
        names=1:30, xlab="Independent Variable", ylab="Value",
)
boxplot(scale(m[,2:31]), main="Boxplot of Scaled Independent Variables",
        names=1:30, xlab="Independent Variable", ylab="Standard Deviation",
)

# Inspect variation by class
# Need to reshape data for ggplot
library(tidyr)
library(ggplot2)
wdbc.long <- gather(wdbc, key="Variable", value="Value", V2:V31, factor_key = T)
ggplot(wdbc.long, aes(x=Variable, y=log(Value + 1e-2), fill=V1)) + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + scale_y_discrete(breaks=NULL) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Separation of Features for Benign and Malignant Tumors") + 
  labs(y=expression(Log[10]~Value), fill = "Class")

############# NORMALITY, MULTICOLLINEARITY, AND EQUAL VARIANCE ASSUMPTIONS ##############

# Test for a difference in mean vectors

library(ICSNP) # Hotelling's T^2 test
HotellingsT2(m[,2:31] ~ m[,1]) # Do mean vectors differ between tumor classes?

# Examine multivariate normality

library(MVN)
mvn(m[,2:31], mvnTest = "royston")

# Function to randomly generate bivariate contour plots

par(mfrow=c(2, 3))
i <- sample(2:31, size = 6)
j <- sample(c(2:31)[-i+1], size = 6)
mapply(function(x,y) mvn(m[, c(x, y)], 
                         mvnTest = "royston", multivariatePlot = "contour"), i, j)
dev.off()

# Examine pairwise correlations

library(corrplot)
corrplot(cor(m), main="Correlation Plot")

# Some variables are functions of other variables: multicollinearity should be high
# Fit a logistic model and calculate VIF

library(car) # vif function
lm <- glm(V1 ~ ., data = wdbc, family = "binomial", control = list(maxit = 50))
vif(lm)

# Do the variance-covariance matrix and correlation matrix differ between classes?

all.equal(cov(subset(wdbc, V1=="B")[,2:31]), cov(subset(wdbc, V1=="M")[,2:31]))
all.equal(cor(subset(wdbc, V1=="B")[,2:31]), cor(subset(wdbc, V1=="M")[,2:31]))

#################### PRINCIPAL COMPONENT ANALYSIS #######################################

pca <- prcomp(m[,2:31], retx = T, scale = T, center = T) # PCA

# Visualize cumulative proportion of variance explained by each PC

plot(cumsum(pca$sdev^2)/30, main="Scree Plot", 
     ylab="Proportion of Explained Variance", xlab="Principal Component")
lines(cumsum(pca$sdev^2)/30)

# Visualize the projection of data onto PC1 and PC2

plot(pca$x[,1:2], col = c("black", "red")[m[,1]], 
     main = "Separation of Malignant Tumors (Red) and Benign Tumors (Black) after PCA")

# Draw confidence ellipses

dataEllipse(x=pca$x[which(wdbc$V1 == "B"),1], y=pca$x[which(wdbc$V1 == "B"),2], 
            center.cex=.75, xlim=c(-15, 6), ylim=c(-13, 9), center.pch=3, 
            xlab="PC1", ylab="PC2",log="", levels=c(0.5, 0.95), col="black", 
            main="0.50 and 0.95 Confidence Ellipses for Benign and Malignant Tumors")
dataEllipse(x=pca$x[which(wdbc$V1 == "M"),1], y=pca$x[which(wdbc$V1 == "M"),2],
            center.cex=.75, log="", levels=c(0.5, 0.95), 
            center.pch=3,col="red", add=TRUE)
legend(-15,-4, fill=c("black","red"),legend=c("Benign Tumor","Malignant Tumor"),
       bg="transparent", cex = 0.75, bty = "n")

#################### QUADRATIC DISCRIMINANT ANALSIS #####################################
########################### USING PC1 AND PC2 ###########################################

library(MASS) # quadratic discriminant analysis (QDA)

pc.dat <- data.frame(cbind(m[,1], pca$x[,1:2])) # prepare data

qda.pca <- qda(V1~PC1+PC2, data=pc.dat, prior=c(0.5,0.5)) # QDA

CM <- table(m[,1], predict(qda.pca, pc.dat)$class); CM # confusion matrix

(sum(CM)-sum(diag(CM)))/sum(CM)*100 # APER (apparent error rate) is approx. 5.62%

holdout.class <- NULL
for (i in 1:569){
  z <- qda(V1~PC1+PC2, data=pc.dat, prior=c(0.5,0.5), subset = c(1:569)[-i])
  holdout.class[i] <- predict(z, pc.dat[i, 2:3])$class
}

CM <- table(m[,1], holdout.class); CM # confusion matrix

(sum(CM)-sum(diag(CM)))/sum(CM)*100 # Expectation of actual error rate = 5.98%

#################### QUADRATIC DISCRIMINANT ANALSIS #####################################
########################## USING ALL VARIABLES ##########################################

qda <- qda(m[,1]~m[,2:31], prior=c(0.5,0.5))

CM <- table(m[,1], predict(qda, wdbc)$class); CM # confusion matrix

(sum(CM)-sum(diag(CM)))/sum(CM)*100 # APER (apparent error rate) is approx. 2.46%

holdout.class <- NULL
for (i in 1:569){
  z <- qda(V1 ~ ., data=wdbc[,1:31], prior=c(0.5,0.5), subset = c(1:569)[-i])
  holdout.class[i] <- predict(z, wdbc[i, 2:31])$class
}

CM <- table(m[,1], holdout.class); CM # confusion matrix

(sum(CM)-sum(diag(CM)))/sum(CM)*100 # Expectation of actual error rate = 4.39%

################## PARAMETER TUNING FOR RANDOM FOREST ###################################

# Below section is computationally intensive

set.seed(1234)
library(caret) # tools for parameter tuning
library(randomForest) # random forests
library(ranger) # fast implementation of random forests
library(beepr) # noise to alert end of a long process

# Configure the train() function

ctrl <- trainControl(method='repeatedcv', 				# k-fold cross validation
                     number=5, 							# 5 folds
                     repeats=20, 						# 20 repetitions
                     search='grid') 					# manual grid search

# Evaluate optimal number of features to sample at each node:
# Vary the 'mtry' parameter in randomForest and compute the error

rf_gridsearch <- train(V1 ~ ., 
                       data = wdbc, 
                       method = 'rf',
                       metric = 'Accuracy', 
                       trControl = ctrl,
                       tuneGrid = expand.grid(.mtry = (1:20)) # mtry values to test
); beepr::beep(2)

print(rf_gridsearch) # results of grid search

plot(rf_gridsearch, xlab="Randomly selected predictors", 
     ylab="Accuracy (Repeated Cross-Validation)",
     main = "Optimal Number of Features to Sample at each Node\n
     Based on 20 Repetitions of 5-Fold Cross-Validation") 

# Based on 5-Fold Cross-Validation, mtry=15 has the lowest error.

# Investigate a reasonable choice for the number of trees

plot(randomForest(V1 ~ ., data = wdbc, mtry = 15, ntree = 1000),
     main="Error as a Function of Number of Trees\n
     Model Error (Black), Benign Tumors (Green), and Malignant Tumors (Red)")
legend(600, 0.12, fill=c("green", "black", "red"),
       legend=c("Error for Benign Tumors", "Model Error", "Error for Malignant Tumors"),
       bg="transparent", cex = 0.75, bty = "n")

# Verify results using the ranger() implementation and test minimum node size

tunegrid <- expand.grid(.mtry = (1:30), 			# features sampled at each node
                        .splitrule = "gini", 		# gini impurity, related to entropy
                        .min.node.size = (1:5)) 	# minimum node size

ranger.gridsearch <- train(V1 ~ ., 
                           data = wdbc, 
                           method = 'ranger',
                           metric = 'Accuracy', 
                           trControl = ctrl,
                           tuneGrid = tunegrid
); beepr::beep(3)

print(ranger.gridsearch) # results of grid search

plot(ranger.gridsearch, 
     xlab="Number of Features Randomly Sampled at Each Node", 
     ylab="Accuracy (Repeated Cross-Validation)",
     main = "Optimal Hyperparameters Based on 20 Repetitions of 5-Fold Cross-Validation") 

# Based on 5-Fold Cross-Validation, mtry=10 has the lowest error and min.node.size = 2.

############################### RANDOM FOREST MODEL #####################################

# Fit random forest using randomForest()

(rf <- randomForest(V1 ~ ., data = wdbc, mtry = 15, ntree = 400)) # 2.99% OOB error
(CM <- rf$confusion[,1:2]); (sum(CM)-sum(diag(CM)))/sum(CM)*100 # 2.99% prediction error

# Fit random forest using ranger()

(rf2 <- ranger(V1 ~ ., data = wdbc, mtry = 10, num.trees = 400, min.node.size=2)) # 3.34%
(CM <- table(rf2$predictions, m[,1])); (sum(CM)-sum(diag(CM)))/sum(CM)*100 # 3.34%

########################### VISUALIZE RANDOM FOREST #####################################

# Visualize a representative tree aggregated from the random forest

library("reprtree")
avgtree <- ReprTree(rf, wdbc, metric='d2')
reprtree:::plot.reprtree(avgtree)
