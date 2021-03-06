---
  title: "ISYE 6501 - Week 4 Homework"
author: "Ujjawal Madan"
date: "04/06/2020"
header-includes:
  - \usepackage{float} 
- \floatplacement{figure}{H}
geometry: margin = 2cm
urlcolor: blue
output: 
  pdf_document:
  toc: true 
---
  
  ```{r global options, include=FALSE}
knitr::opts_chunk$set(echo = F, fig.width = 10, warning = F, fig.pos ='h', message = F, error = F)
```

```{r echo = F}

#Install missing packages and initiate necessary libraries
library(readr)
library(knitr)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(randomForest)
library(caret)
library(dplyr)

```

## Question 9.1

*Using the same crime data set uscrime.txt as in Question 8.2, apply Principal Component Analysis
and then create a regression model using the first few principal components. Specify your new model in
terms of the original variables (not the principal components), and compare its quality to that of your
solution to Question 8.2. You can use the R function prcomp for PCA. (Note that to first scale the data,
                                                                      you can include scale. = TRUE to scale as part of the PCA function. Don't forget that, to make a
                                                                      prediction for the new city, you'll need to unscale the coefficients (i.e., do the scaling calculation in
                                                                                                                                            reverse)!)*
  
  ```{r}

#Import file
us_crime <- read_delim("http://www.statsci.org/data/general/uscrime.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

```

Let's start by running the component analysis on the first 15 predictor variables.

```{r echo = T}
pca <- prcomp(us_crime[,-16], scale = TRUE)
```

Let's now examine how much variance each principal component accounts for.

```{r echo = T}
#Scree Plot
pca_var <- pca$sdev^2

variation <- sapply(pca_var, function(x){
  x/sum(pca_var)  })

barplot(variation, main = 'Scree Plot', xlab = 'Principal Component', ylab = 'Percent Variation')
```

Seeing it in terms of of culimative variance would allow us to select the number of principal components based on how much variance we are looking for. 

```{r}
cumalitive <- vector()
for (  i in seq(1, length(pca_var)) ){
  cumalitive[i] <- sum( pca_var[1:i] ) / sum(pca_var)
}

barplot(cumalitive, main = 'Cumalitive Variance', xlab = 'Principal Component', ylab = 'Percent Variance Explained')

```

For example, if we want to explain 90 percent of the variance, we should use approximately 6 principal components.

Using the principal components, let's create a linear model using only the first 6 principal components.

```{r echo = T}
ncomps <- 6
data <- data.frame(cbind(pca$x[,1:ncomps], us_crime[,16]))
colnames(data) <- c(as.vector(colnames(pca$x[,1:ncomps])), 'Crime')
model <- lm(Crime ~ ., data)
summary(model)$r.squared

```

Above is the R^2 value of the model. While this R^2 value may not be very high, it captures a little over 80 percent of the variance that our linear model from the previous homework assignment did. What if we tried using all 15 components, since we used 15 attributes in the linear model from last week?

```{r}
ncomps <- 15
data <- data.frame(cbind(pca$x[,1:ncomps], us_crime[,16]))
colnames(data) <- c(as.vector(colnames(pca$x[,1:ncomps])), 'Crime')
model <- lm(Crime ~ ., data)
summary(model)$r.squared

```

We do indeed end up achieving an R^2 value that is precisely the same as our previous model.

I have now created a function below that creates a linear model using the principal components and is then used to predict Crime from the dataset. This function takes in the number of principal components one wants to employ in the linear model and returns the prediction of the response value based on the input vector it receives (in terms of the original variables before scaling).

IMPORTANT - After the model is created using the principal components, the final calculation is indeed done using the original variables. The input vector consists of original variables before scaling rather than principal components.

```{r echo = T}

input <- function(ncomps, inputvector = us_crime[-16,1]){
  
  pca <- prcomp(us_crime[,-16], scale = TRUE)
  mat <- as.matrix(us_crime[,-16])
  sds <- apply(mat, 2, sd)
  means <- apply(mat, 2, mean)
  scaled <- scale(as.matrix(us_crime[,-16]), center = TRUE, scale = TRUE)
  newpcavars <- scaled %*% as.matrix(pca$rotation[,1:ncomps])
  #How many of those vars you want to use is up to you
  data <- data.frame(cbind(newpcavars, us_crime[,16]))
  model <- lm(Crime ~ ., data)
  intercept <- model$coefficients[1]
  coefficients <- model$coefficients[2:length(model$coefficients)]
  
  original_variables <<- pca$rotation %*% coefficients
  
  after_scaling_equations <<- vector()
  #newcoef <- apply(pca$rotation*coefficients, 1, mean)
  for (i in seq(1, 15)){
    after_scaling_equations[i] <<- paste(toString(original_variables[i] / sds[i]), 
                                  ' *x', i, ' - ', toString((original_variables[i]/sds[i])*means[i]), sep = '')
  }
  after_scaling_equations[16] <<- intercept
  
  scaled_input <- (inputvector - means)/sds
  transformed_input <- scaled_input %*% as.matrix(pca$rotation[,1:ncomps])
  output <- intercept + (transformed_input %*% coefficients)
  return (output)
}

```

```{r echo = T}
inputvector <- as.vector(as.matrix(us_crime[1,-16]))
us_crime[1,16]
input(15, inputvector)
```

For example, if we use all 15 principal components, then we achieve a regression value of 755 for row 1 when the actual value was 791. Below is the linear model transformed in terms of the original variables after scaling.

```{r eco = T}
kable(original_variables)
```

Below is a table which consists of the linear equations one can employ for unscaled variables. For example, entering the exact original values of row 1 in the us_crime dataset into the equations below and adding them will get you the 755 value we saw just previously. 

```{r echo = T}
kable(after_scaling_equations)
```


## Question 10.1

*Using the same crime data set uscrime.txt as in Questions 8.2 and 9.1, find the best model you can
using (a) a regression tree model, and (b) a random forest model.*

*In R, you can use the tree package or the rpart package, and the randomForest package. For
each model, describe one or two qualitative takeaways you get from analyzing the results (i.e., don't just stop when you have a good model, but interpret it too).*

Let's start by building the classification tree using the rpart package. There are a number of tuning parameters which allow us to configure our model. In our case, the ones we should consider include minsplit (the smallest number of observations in the parent node that could be split further), minbucket (the smallest number of observations that are allowed in a terminal nod)and cp (minimum improvement in the model needed at each node).

If we set minbucket and minsplit to be 1, then our model will be overfitted. As mentioned in the lecture, each leaf node should have at least 5 percent of the data points. Therefore let's set our minbucket to be 3. Our cp parameter is a bit more difficult to determine since there is no predefined formula for what the minimum improvement should be. However, after some experimentation, 0.05 seems to be a reasonable value. If the cost is too low for any reason, we can always prune the three (I have included the code)

```{r}

fit <- rpart(formula = Crime ~., 
             data = us_crime, 
             minbucket = 3,
             cp = 0.05)

# plot mytree
fancyRpartPlot(fit, caption = NULL)

#Pruning the tree
#newfit <- prune(fit, cp = 0.0)
#fancyRpartPlot(newfit, caption = NULL)



```

Taking a look at our model, it seems that we have 7 leaf nodes. There are a total of only 6 features variables utilized in our splits with Po1 used for our first split and the most deep split using prob. 

We can also take a look at our predicted values vs actual values. A measure like RMSE would not be particularly useful to run on our predicted values since we are not running our model on a testing or validation set. We could create a model so that it completely fit the training data and RMSE equaled 0 on our training set if we wanted. Neverthless, it gives an idea of to what degree our model has fit the training data.

```{r}
kable(cbind(predict(fit, us_crime[,-16]), us_crime$Crime))

```

Let's also take a look at variable importance.

```{r}
kable(fit$variable.importance)
```

Let's jump to the random forest model now. The two parameters that are of concern to us (if we use the randomForest package) are mtry and ntrees. As Joel has stated, any number between 500 and 1000 trees ought to suffice. So let's split it and pick 750. Mtry (the number of factors considered during a split) is typically log(m) with m being number of feature variables. After some research of my own, it seems that another formula sometimes used is sqrt(m). Let's try different values of mtry and see what we get.

```{r}
set.seed(3, sample.kind = 'Rounding')
forest_fits <- lapply(seq(3, 6, 1), function(factors){
  randomForest(formula = Crime ~., 
                               data=us_crime,
                               mtry = factors,
                               ntree = 1000,
                               importance=TRUE)
  })

print(forest_fits)
```

As we can see, the model that explains the most variance is the one with mtry at 4. So let's choose that. Although we know that ntree set to a 1000 is an appropriate number of trees, we can take a look to confirm. 

```{r}
plot(forest_fits[[2]])
```

We can see that adding additional trees after 500 does not do much to reduce our error. Nor does it increase it.

One of the great things about the randomforest model in the randomForest package is that we can take a look at variable importance. Using a randomForest model in a machine learning task can provide us with the variable importance and help us select features or at least determine how much predictive power each variable has on it's own.

```{r}

kable(importance(forest_fits[[2]]))

```

It seems that Po1 is one of the most important variables which would in fact line up with our previous decision tree that used Po1 as a variable for the first split. However, our classification tree also used M and Pop which do not seems to be very important variables according to our random forest model. 

We can also take a look at our predicted values vs our actual values just as we did on for the rpart model.


```{r}
kable(cbind(forest_fits[[2]]$predicted, us_crime$Crime))
```


## Question 10.2

*Describe a situation or problem from your job, everyday life, current events, etc., for which a logistic regression model would be appropriate. List some (up to 5) predictors that you might use.*

I believe an interesting use case for logistic regression would be in a credit card fraud detection algorithm. For example, predictors could include location (with crossreference to previous locations the individual made purchases at) , cost of purchase (with crossreferences with previous purchases made), type of purchase (with crossreference to previous type of purchases), date and time of day etc. The advantages of logistic regression vs SVM or KNN would be that the algorithm would generate a probability for there being a fraud. And the threshold for what is marked as fraud and what is not could be adjusted based on the costs associated with False Negatives and False Positives. In such a scenario, I would imagine that the threshold might be a bit lower to be safe, since the cost of overlooking a fraud is much higher than the cost of alerting the customer that a certain suspicious transcation was made. 

## Question 10.3

### Question 10.3.1

*Using the GermanCredit data set germancredit.txt from http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german / (description at http://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29 ), use logistic
regression to find a good predictive model for whether credit applicants are good credit risks or not. Show your model (factors used and their coefficients), the software output, and the quality of fit. You can use the glm function in R. To get a logistic regression (logit) model on data where the response is either zero or one, use family=binomial(link="logit") in your glm function call.*

Let's start by importing the data and preparing it.

```{r echo = T}

german <- read_table2("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
names(german)[length(names(german))]<-'Response' 

cols <- c("A11", 'A34', 'A43', 'A65', 'A75', 'A93', 'A101', 'A121', 'A143', 'A152', 'A173', 'A192', 'A201')
german[cols] <- lapply(german[cols], factor)
german$Response <- german$Response -1
german$Response <- as.factor(german$Response)

```

Let's now run the model with default parameters and threshold set to 0.5

```{r}

fit <- glm(Response ~ ., data = german,  family=binomial)
print(fit)
summary(fit)
prediction <- predict(fit, german[,-ncol(german)], type = 'response')
prediction_0.5 <- ifelse (prediction > 0.5, 1, 0)

```

Let's take a look at the confusionMatrix for this model

```{r}
cm <- caret::confusionMatrix(factor(prediction_0.5), german$Response)
print(cm)
```

### Question 10.3.2

*Because the model gives a result between 0 and 1, it requires setting a threshold probability to
separate between "good" and "bad" answers. In this data set, they estimate that incorrectly
identifying a bad customer as good, is 5 times worse than incorrectly classifying a good
customer as bad. Determine a good threshold probability based on your model.*
  
  With the threshold set to 0.5, the model incorrectly predicts 140 of a 1000 data points as good customers. And incorrectly predicts good customers as bad 74/1000 cases. Let's see if we can optimize this. 


```{r echo = T}
#False Negative (incorrectly predicted as good customers)
cm$table[1,2]

#False Positive (incorrectly predicted as bad cusomters)
cm$table[2,1]
```
Our false negative and our false positive can both be found from the confusion matrix. We can use this to come up with a linear equation.  

Y = 5 * False Negative(incorrectly predicted as good customers) + False Positive (incorrectly predicted as bad cusomters)

```{r echo = T}

#Linear Equation
5 * cm$table[1,2] + cm$table[2,1]

```

Let's run different probability thresholds and see what probability threshold most reduces total error.

```{r echo= T}
counter <- 1
value <- vector()
prediction <- predict(fit, german[,-ncol(german)], type = 'response')
thresholds <- seq(0.01, 0.99, 0.01)
for (i in thresholds){
  prediction1 <- ifelse (prediction > i, 1, 0)
  cm <- caret::confusionMatrix(factor(prediction1), german$Response)
  value[counter] <- 5 * cm$table[1,2] + cm$table[2,1]
  counter <- counter + 1
}
table <- cbind(thresholds, value)
colnames(table) <- c('Threshold', 'Total_Error')
as.data.frame(table) %>% ggplot(aes(Threshold, Total_Error)) + geom_point() + ggtitle('Total Error Vs Threshold')
```

As we can see, a probability threhsold of approximately 0.2 would most reduce our total error. Let's find the exact minimum of this plot.

```{r echo = T}
thresholds[which.min(value)]
```

The exact minimum is 0.13. Let's see what our new model looks like along with the confusion matrix when the probability threshold is 0.13

```{r echo = T}
prediction <- predict(fit, german[,-ncol(german)], type = 'response')
prediction_optimized <- ifelse (prediction > 0.13, 1, 0)
caret::confusionMatrix(factor(prediction_optimized), german$Response)
```

As we can see, while our false positive hav greatly risen, there are very few instances where our model is now incorrectly predicting a bad customer as good. In layman's terms, the bank is now very conservative in lending out loans. They are denying a large number of applications, many of which may actually not be risky. However, for those of whom they are lending to, the probability of any one of them being a credit risk is very very low.


