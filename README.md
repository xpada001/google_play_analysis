# Google Play Analysis - Summary

Nowadays, Google play store is gaining more popularity with an increasing demand on the smartphones in the market. As a result, consumers spend a significant amount of time on using different kinds of applications on their electronic gadgets, and application creators are interested in knowing the driven factors that make an application successful. Hence, using the dataset containing detailed information on thousands of applications, it would be very useful to analyze and predict the application rating in order to find successful products.

This proejct will be divided in several sections. First, we will examine the details of the Google
Play Store data obtained from Kaggle. Then, we will discuss some of the preprocessing methods
applied to this dataset, after which we will look into data analysis and data prediction using the smoothing method, the random forest
and different boosting methods. Furthermore, I will present the performance comparison between the models presented in this project. Moreover, we will interpret the findings in the context of this project and draw conclusions for potential real business to understand. The programming portion of this project is attached to Appendix.

## Data

The dataset is obtained on Kaggle, where the author scraped it from Google Play Store. The source is as follows: https://www.kaggle.com/lava18/google-play-store-apps. The dataset has size 10841 entries and 13 variables.

Our goal is to predict the Rating of each application ranged from 0 to 5 using the rest of the features. Hence, various feature visualizations are shown in the report to grasp a general idea of the data. Some variable value distribution are shown below:

![alt text](https://github.com/xpada001/google_play_analysis/blob/main/variable_plot.png?raw=true)

As we can observed, most ratings given to applications are quite positive, i.e. between rating 4 and 5. However, the Rating distribution is heavily left-skewed, hence it is needed to transform it during the modeling phase. Due to rounding effect from the data, most applications have number of downloads rounded to the highest place value, and we can see that a large portion of applications have number of downloads close to a million. On the other hand, we can see in the Review distribution that a large portion of applications have either very few numbers of reviews, i.e. between 0 and 100, or very high volume of reviews of more than 10 thousand reviews, which means that the
opinions to applications are polarized. In addition, we can examine the application size distribution heavily right-skewed, which indicates that most of the applications have small sizes. Since Installs and Reviews have large values and they may impact greatly the response variable, it needs to do log-transform. Same for Size feature as it is right-skewed.

The plot below shows the number of applications sorted by cagetories:

![alt text](https://github.com/xpada001/google_play_analysis/blob/main/category_plot.png?raw=true)

we can see that software developers tend to create applications in the Family, Game and
Tools categories. A possible explanation why Family category is the most popular one is that it
is suited for the whole family, i.e. children, teenagers and parents. Hence it is more likely to
make a profit. Tools category is popular for the similar reasoning as it is appropriate to all ages.
Game is another popular category because players can easily spend some money to have a
better in-game experience.

We can observe that a general trend is that the more Installs and Reviews an application has, the more likely that its rating is high. We can conduct a feature engineering to create a new feature called RVI, which stands for Rating value indicator:

RVI = log(Reviews + 1) ∗ log (Installs + 1)


## Preprocessing

In this dataset, there are several issues that we must fix before doing any exploratory data analysis. The first step is to remove any entries that need to be deleted. For instance, there is one entry where one feature is missing, and all other features were left-shifted by one column. Since it occurred only in one entry, it was decided to remove this entry completely. Moreover, there are roughly 10% of entries have duplicated application name, however based on the nature of the dataset, it is assumed that each entry represents a unique application. Thus, it is necessary to remove these duplicated entries using the dplyr library.

Then, some data manipulations are performed. For example, we want to transform features such as Size, Installs, Price to numeric values. Also, the Last.Updated feature is originally a date type, but we want it to indicate the number of days since the most recent update of the application so that we can convert it into continuous variables. Missing values are also handled by imputation.

## Modeling
In the modeling phase, we used three models to perform predictions: Smoothing methods, Random Forest and Boosting (i.e. Gradient Boosting/XGBoost).

### Smoothing Method
Various modeling technics are experimented to assess this dataset, namely:
 - K-nearest neighbor
 - Locally weighted sums of squares with different span values
 - Generalized additive model (GAM) with visualization in 3D
 - Variable interaction using GAM

### Random Forest
Random forest is a powerful machine learning method for regression. When using it for regression purposes, the general idea of a random forest model is that the model uses a group of decision trees during the training phase, after which it gathers the result of each weak learners and output a final mean prediction. If we kept efficiency in mind, it is important to choose an optimal number of decision trees used in the model. The default setting is to use 500 trees, which will be extremely slow to run, and it is very likely to overfit the model. Hence, we can plot our random forest model to visualize the best number of trees to use, and we found that 100 trees would be the optimal choice.

The measure on the importance of variates can be done with two methods. The first method calculates the drop of residual sum of squares (RSS) when the corresponding variate is dropped (type 2 measure), whereas the second method is a native random forest method that calculate the average decrease in model prediction accuracy when the corresponding variate is dropped (type 1 measure).

The performance of random forest method using bagging and uncorrelated random forest is compared, and we can observe that uncorrelated have slightly lower error on all of the three terms, but the difference is not too significant. However, the computation time between the methods is large. The uncorrelated method performs better with a much shorter amount of time compared to the bagging method.

### Boosting
As we know, random forest methods use a group of decision trees and aggregates the results from each tree. However, there are other methods of combining the results of these weak learners like linear combination with weight assignment to each tree. Such method is called boosting, where it uses other tree’s performance to improve their own prediction power.

In the implementation of boosting method attached in the Appendix, it has an important parameter called M, which determines the number of trees involved in the prediction. It is
essential to select the optimal M as too many M will make the model prediction very slow, whereas too small M can result in high prediction errors. Similar analysis needs to be done on maximum depth of the boosting trees.

We can use the gradient boosting method to build a relative influence summary. More specifically, gradient boosting provides the relative influence of a variate amongst all the
predictors, and the relative influence can also be shown in scaled form and in percentage:

![alt text](https://github.com/xpada001/google_play_analysis/blob/main/relative_influence.png?raw=true)

We can observe that RVI, Category and Reviews are the most important features.

Cross validation is performed for obtaining the best tree depths, number of trees and shrinkage. Then, I also explored the performance of XGBoost to compare with other models.

### Results

The table below summarizes the error prediction and the time consumed for each of the method presented in this report:

![alt text](https://github.com/xpada001/google_play_analysis/blob/main/model_results.png?raw=true)

We can see that even though the differences in errors prediction are not too far apart, the random forest model is the best one among the four. Both of its error prediction and
computation time are ideal compared to the other models. Even though GAM has a much shorter computation time, its MSLE is not ideal. I would expect XGBoost to perform better than the other models, however due to insufficient time for parameter tuning, its performance is much inferior than expected, but its computation time is much better compared to gradient boosting, which proves its processing speed.
