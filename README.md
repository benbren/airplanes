
<!-- README.md is generated from README.Rmd. Please edit that file -->
Biostat 625 Final Project: Group 6
==================================

<!-- badges: start -->
<!-- badges: end -->
The goal of Project is to make predictions as to whether a person is going to recomend or not recommend based off of their review of an airline trip. This project contains four main folders, as detailed below.

R/
--

This is where our code is located. A quick overview of the scripts is as follows:

-   **attributes\_model.R** : Code for checking which non-text attributes were good predictors. Done with logistic regression.
-   **matrix\_text\_processing.R**: Turning the reviews of our data set into a functional Document-Term Matrix.
-   **train\_lr\_glmnet.R**: Training and testing the Logistic Regression model.
-   **train\_nb.R**: Training and testing the Naive Bayes model.
-   **train\_svm.R**: Training adn testing the Support Vector Machine model.
-   **train\_rf.R**: Training the Random Forest model.

### shiny/

This folder has the code and data corresponding to the Shiny application associated with this project. This app is not deployed on shinyapps.io as the memory usage (model, document term matrix, data manipulation, etc.) is too much for the free plan - and we prefer not to pay for the full plan for this project! Hopefully, in the future, we can optimize the memory usage of the app so that we can deploy it using the free plan. However, you can still run the app!

``` r
shiny::runGitHub('airplanes','benbren', subdir = 'shiny')
```

This should open a window that allows you to use the app in its current form, assuming you have the shiny package installed. If not - do so, and this will work.

### rds/

This is where our data is saved. Many of the datasets were taking too long to load as csv files (i.e 10 minutes), so we chose to save our data in RDS form instead. Here we saved the resuls of our training models, training and testing IDs (for accuracy of model comparison on the cluster) and different DTM matrices.

### Cluster\_Output/

Contains the results from all the training scripts in **R/** after they were run on the cluster.

### report/

The folder containg our final report in .Rmd format for this project (See this for more details on the project)

Group Members
-------------

-   **Tahmeed Tureen** *(<tureen@umich.edu>)*
-   **Mandy Meng Ni Ho** *(<mandyho@umich.edu>)*
-   **Ben Brennan** *(<brennben@umich.edu>)*

Thanks for checking out our project. :)
