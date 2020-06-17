# Classification_Using_High_Dimensional_Cancer_Data

This repository contains **code** and a **17 page formal [PDF report](https://github.com/timothygmitchell/Classification_Using_High_Dimensional_Cancer_Data/blob/master/Classification-Using-High-Dimensional-Cancer-Data.pdf)** for my final project in STAT 488 (Multivariate Statistics). Here I analyze high-dimensional cancer data in a data set containing 569 observations of 30 features. Each observation represents a benign or malignant breast cancer tumor.

I implement a variety of methods in multivariate analysis, including classification (quadratic discriminant analysis and random forests), dimension reduction (PCA), and inference of mean vectors.

To assess assumptions, I investigate multicollinearity, multivariate normality, and equality of variances.

To evaluate model error, I use misclassification rate and jackknifing (for QDA) and out-of-bag error (for random forests). I implement a parameter search to identify the best possible random forest model.

I find that QDA and random forest perform well in classification. In addition, PCA successfully reduces the number of dimensions necessary to discriminate between classes. PCA reveals partial separation of tumor classes with only two principal components.
