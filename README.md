# Classification-High-Dimensional-Cancer-Data

This project showcases a variety of techniques in high-dimensional data analysis, including classification (quadratic discriminant analysis, random forests), dimension reduction (principal component analysis), and multivariate tests of mean vectors.

I find that separation of tumor classes (benign, malignant) is visible in 2D principal component space.

A key component of this analysis is evaluating assumptions such as multivariate normality, equality of variances, and absence of multicollinearity.

For QDA, I use misclassification rate and jackknifing to evaluate model error. For random forests, I use out-of-bag error. I implement a parameter search to identify the best possible model. Both QDA and random forest achieve high accuracy.
