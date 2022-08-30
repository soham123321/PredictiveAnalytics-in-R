# PredictiveAnalytics-in-R
Summary - Analytical project for understanding trends in hotel booking cancellations. Encompasses Supervised & Unsupervised Machine Learning model to perform predictive analytics for the cancellations
The data set is made of 19 attributes and 700000 observations. Each observation is a hotel booking and gives insight into different attributes for that booking.
Initially various visualizations were created to find patterns in the dataset for different attributes. Some of the visualizations were box plots, bar graphs & maps.
Post this stage unsupervised machine learning was perfromed using apropri & arules libraries to determine the statistically significant attributes from the dataset.
The dataset was divided into training and test data to reduce overfitting
A support vector machine was created using the training data. The predictive model was tested with different values of C and cross for the SVM and the model with the best accuracy was selected. Confusion matrix was created to test the accuracay of the data. Final model with 80% prediction accuracy was achieved to predict whether a hotel booking will result in a cancellation.
