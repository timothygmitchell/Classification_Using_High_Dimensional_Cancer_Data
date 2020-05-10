# Classification-High-Dimensional-Cancer-Data

This repository contains code and a formal PDF report for my Multivariate Analysis final project. Here I analyze high-dimensional cancer data in a data set containing 569 observations of 30 features. Each observation represents a benign or malignant breast cancer tumor.

I implement a variety of methods in multivariate analysis, including classification (quadratic discriminant analysis and random forests), dimension reduction (PCA), and inference of mean vectors.

To assess assumptions, I investigate multicollinearity, multivariate normality, and equality of variances.

To evaluate model error, I use misclassification rate and jackknifing (for QDA) and out-of-bag error (for random forests). I implement a parameter search to identify the best possible random forest model.

I find that QDA and random forest perform well in classification. In addition, PCA successfully reduces the number of dimensions necessary to discriminate between classes. PCA reveals partial separation of tumor classes with only two principal components.
