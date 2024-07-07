# PredictiveAnalytics for Hotel Booking Cancellations
Programming language: R
Objective: Predict whether a hotel booking will be cancelled
Data set details: 19 Features, 700000 Observations

Highlights: Feature Engineering, Unsupervised Machine Learning, Supervised Machine Learning, Visual Analytics, Predictive Analytics
ML Model: Support Vector Machine

Summary
Proposed solution was to perform unsupervised ML on the dataset for feature selection and supervised machine learning was used to perform predictions.

Steps:

Explatory Data analytics - Visualizations(Box plots, histograms & maps) using ggplot for understanding the skewness of the data

Unsupervised Machine Learning - Feature selection using apropri & arules libraries. Features were determined by looking at their p-value to determine stastical signifance.

Data Split - The data set was randomly divided into training and test to avoid overfitting.

Supervised Machine Learning - Support vector machine was used to perform predictions. Performed grid search for multiple values of C and cross for model tuning.

Accuracy Testing - Confusion matrix was created to assess the accuracy of the model

