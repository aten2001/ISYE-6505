#Install packages
install.packages('kernlab', 'caret', 'kknn')





### QUESTION 2.2



## Question 1

#Import Dataset- importing dataset with headers. Please change file path!
df <- read.delim("~/GaTech/ISYE 6501/Week 1/Question 2.2/credit_card_data-headers.txt")

#Import kernlab
library(kernlab)

#Set Seed
set.seed(1, sample.kind = 'Rounding')

#Running base ksvm model
df <- as.matrix(df)
model <- ksvm (as.matrix(df[,1:10]), as.matrix(df[,11]), type = 'C-svc', kernel = 'vanilladot', C=100, scaled = TRUE)
pred <- predict(model,df[,1:10])

#The below method will not only provide accuracy but will provide confusion matrix as well as other performance measures. 
library(caret)
confusionMatrix(factor(pred), factor(df[,11]))








#This will run the ksvm function with different costs. By manually tuning the cost, we can select the model that works best.  


#This tunegrid consists of extremely low values for cost
#tunegrid <- seq(0.0001, 0.01, 0.0001)
#tunegrid <- seq(0.001, 0.002, 0.00001)
tunegrid <- seq(0.00137, 0.00141, 0.000001)
rangeforloop <- 1: length(tunegrid)

models <- list()
results <- list()

for (i in rangeforloop){
  models[i] <- ksvm (as.matrix(df[,1:10]), as.matrix(df[,11]), type = 'C-svc', kernel = 'vanilladot', C=tunegrid[i], scaled = TRUE)
  }

#This gives the accuracy of all of our models in a list
svm_results1a <- sapply(models, function(model){
  pred <- predict(model,df[,1:10])
  sum(pred == df[,11]) / nrow(df)
})

plot(tunegrid, svm_results1a)

#This gives us the confusion matrix(s) in a list
svm_results2a <- lapply (models, function (model){
  pred <- predict(model,df[,1:10])
  confusionMatrix(factor(pred), factor(df[,11]))
})


#As we can see, for extremely low values for cost (0.0001 to approximately 0.001), the model does not perform well. 
tunegrid[which.max(svm_results1a)]
#However, there does seem to be a local maximum for accuracy around 0.001389. Let's hang on to that value for now.












#Let's take a look at extremely high values of cost. 

#tunegrid <- seq(100000, 4100000, 100000)
tunegrid <- seq(85000, 105000, 2500)

#This specifies the list we will iterate over
rangeforloop <- 1: length(tunegrid)

models <- list()
results <- list()

#Let's once again run these models for very high values of C
for (i in rangeforloop){
  models[i] <- ksvm (as.matrix(df[,1:10]), as.matrix(df[,11]), type = 'C-svc', kernel = 'vanilladot', C=tunegrid[i], scaled = TRUE)
}

#This gives the accuracy of all of our models in a list
svm_results1b <- sapply(models, function(model){
  pred <- predict(model,df[,1:10])
  sum(pred == df[,11]) / nrow(df)
})

#This gives us the confusion matrix(s) in a list
svm_results2b <- lapply (models, function (model){
  pred <- predict(model,df[,1:10])
  confusionMatrix(factor(pred), factor(df[,11]))
})


#Let's take a closer look
plot(tunegrid, svm_results1b)



#As we can see, an optimized higher cost BARELY increases the accuracy. The highest accuracy achieved with a relatively higher cost was 90,000 after some experimentation.
#Let's examine the equation of the model with cost 0.001389.

svm_best_model <- ksvm (df[,1:10], df[,11], type = 'C-svc', kernel = 'vanilladot', C=0.001389, scaled = TRUE)
pred <- predict(svm_best_model,df[,1:10])
confusionMatrix(factor(pred), factor(df[,11]))$overall['Accuracy']

#As we can see, this accuracy of 0.872 is marginally better than the model created without a specified cost.


#These are the weights of each of the feature variables
colSums(svm_best_model@xmatrix[[1]] * svm_best_model@coef[[1]])

#This is the value, b, in the svm equation.
svm_best_model@b












## Question 2

#Let's try the Radial Basis kernel to explore other kernels.


#Set seed
set.seed(1, sample.kind = 'Rounding')

#tunegrid <- seq(1, 1001, 10)
#tunegrid <- seq(1, 10000, 100)
#tunegrid <- seq(1, 100000, 1000)
#tunegrid <- seq(1, 1000000, 10000)
tunegrid <- seq(1, 10000000, 100000)
rangeforloop <- 1: length(tunegrid)

models <- list()
results <- list()

for (i in rangeforloop){
  models[i] <- ksvm (as.matrix(df[,1:10]), as.matrix(df[,11]), type = 'C-svc', kernel = 'rbfdot', C=tunegrid[i], scaled = TRUE)
}

#This gives the accuracy of all of our models in a list
svm_results1c <- sapply(models, function(model){
  pred <- predict(model,df[,1:10])
  sum(pred == df[,11]) / nrow(df)
})

plot(tunegrid, svm_results1c)

#This gives us the confusion matrix(s) in a list
svm_results2c <- lapply (models, function (model){
  pred <- predict(model,df[,1:10])
  confusionMatrix(factor(pred), factor(df[,11]))
})

#Based on this plot, we should be able to classify the data with 100 percent accuracy. 

svm_radial_final <- ksvm (as.matrix(df[,1:10]), as.matrix(df[,11]), type = 'C-svc', kernel = 'rbfdot', C=5000000, scaled = TRUE)
pred <- predict(svm_radial_final,df[,1:10])
confusionMatrix(factor(pred), factor(df[,11]))

#According to the confusion matrix, we can with 100 percent accuracy classify this dataset. However, as cost increases, the model may be at risk of overfitting. Let's test it on a hypothetical training and validation set.

m <- nrow(df)
index <- sample(1:m, round(m/5), replace = FALSE)

svm_radial_final <- ksvm (as.matrix(df[-index,1:10]), as.matrix(df[-index,11]), type = 'C-svc', kernel = 'rbfdot', C=5000000, scaled = TRUE)
pred <- predict(svm_radial_final,df[index,1:10])
confusionMatrix(factor(pred), factor(df[index,11]))


#As we can see this doesn't work in practice. Having a very high cost results in overfitting in and does not perform well with random testing sets.










## Question 3

#Import kknn package
library(kknn)


#Set our seed
set.seed(1, sample.kind = 'Rounding')

#Creating our training and validation sets
m <- nrow(df)
index <- sample(1:m, round(m/5), replace = FALSE)


k_list <- seq(1, 51, 2)
threshold_list <- seq(0.2, 0.8, 0.02)

#Let us now train our model with different values of k.
models = lapply(k_list, function(neighbours){model <- kknn(formula = R1 ~ ., train = as.data.frame(df)[-index,], test = as.data.frame(df)[index,], k = neighbours, scale = T)
                                fit <- fitted(model) 
                                cbind(fit, df[index, 11]) })

#One could automatically round values greater than or equal to 0.5 to 1, and round down values less than that 0.
#However, a better approach in my opinion would be to find the threshold that best optimizes for accuracy. 



KNN_results <- list()
Max_accuracy <- matrix( nrow = 0, ncol = 3)
colnames(Max_accuracy) <- c('Number of Neighbours', 'Ideal Threshold', 'Maximum Accuracy Achieved')

for (i in 1:length(k_list)){
  model <- models[[i]][,1]
  knn_table <- lapply(threshold_list, function(threshold){
    factor(ifelse(model > threshold, 1, 0))
  })
  
  knn_accuracy <- sapply(knn_table, function(prediction){
    confusionMatrix(prediction, factor(df[index, 11]))$overall['Accuracy']
  })
  
  KNN_results[[i]] <- cbind(threshold_list, knn_accuracy)
  Max_accuracy <- rbind(Max_accuracy, c(k_list[i], threshold_list[which.max(knn_accuracy)], max(knn_accuracy)))
}


#Seeing more detailed results based on each neighbour. Below is an example.
KNN_results[[10]]

#Number of Neighbours, threshold, and maximum accuracy
Max_accuracy
Max_accuracy[which.max(Max_accuracy[,3]),]

#Ironically, it seems that the best model is achieved when the neighbours is set to 11 and threshold is kept at 0.5. So we don't have to change our threshold to get the best accuracy.












### QUESTION 3.1


## Question 3.1a


#Currently, the SVM model is 0.871
#Currently, the KNN model is 0.916


#Let's work with the KNN Model right now.

#Set Seed
set.seed(1, sample.kind = 'Rounding')


#Cross validation knn
knn_model_cv <- cv.kknn(formula = R1 ~ ., 
                          data = as.data.frame(df),
                          scale = T,
                          kcv = 10)

prediction <- knn_model_cv[[1]][,2]
actual <- factor(knn_model_cv[[1]][,1])

prediction <- factor(ifelse(prediction > 0.5, 1, 0))

#This is the accuracy after cross validation
confusionMatrix(prediction, actual)$overall['Accuracy']

#As said to us by Joel, the final model will be trained on the whole training set. This is just used to gauge performance.








#Let's work with the SVM Model now.

set.seed(1, sample.kind = 'Rounding')

#Randomly shuffle the data
df<-df[sample(nrow(df)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(df)),breaks=10,labels=FALSE)


Accuracy <- matrix(nrow = 0, ncol = 1)
colnames(Accuracy) <- ('Maximum Accuracy Achieved')

#Perform 10 fold cross validation
for(i in 1:10){

  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df[testIndexes, ]
  trainData <- df[-testIndexes, ]
  
  #Based on previous paramater tuning, cost set to 0.001389
  model <- ksvm (trainData[,1:10], trainData[,11], type = 'C-svc', kernel = 'vanilladot', C=0.001389, scaled = TRUE)
  pred <- predict(model,testData[,1:10])
  Accuracy <- rbind(Accuracy, confusionMatrix(factor(pred), factor(testData[,11]))$overall['Accuracy'])
}


#This is the average accuracy from cross validation.
mean(Accuracy)

#This displays the results from the 10 fold cross validation. As we can see, the accuracy does vary quite a bit. Of course, we are using the cost calculated from the previous question so perhaps modifying that may yield more stable results. 
Accuracy




























## Question 3.1b


#Reset our df
#Import Dataset- importing dataset with headers. Please change file path!
df <- read.delim("~/GaTech/ISYE 6501/Week 1/Question 2.2/credit_card_data-headers.txt")

#Set Seed
set.seed(1, sample.kind = 'Rounding')

#Running base ksvm model
df <- as.matrix(df)

#Create our training, validation, and testing indeces
m <- nrow(df)
testing_index <- sample(1:m, round(m/5), replace = FALSE)
training_validation_index <- (1:m)[-testing_index]
validation_index <- sample(training_validation_index, round(length(training_validation_index)/4), replace = F)
training_index = training_validation_index[-validation_index]


#The K_List being the neigbours and the treshold list being the list of thresholds
k_list <- seq(1, 51, 2)
threshold_list <- seq(0.2, 0.8, 0.02)

#Let us now train our model with different values of k.
models = lapply(k_list, function(neighbours){model <- kknn(formula = R1 ~ ., train = as.data.frame(df)[training_index,], test = as.data.frame(df)[validation_index,], k = neighbours, scale = T)
  fit <- fitted(model) 
  cbind(fit, df[validation_index, 11]) })

#Let's not only find the best number of neighbours with the training data, but also the best threshold, just like before

KNN_results <- list()
Max_accuracy <- matrix( nrow = 0, ncol = 3)
colnames(Max_accuracy) <- c('Number of Neighbours', 'Ideal Threshold', 'Maximum Accuracy Achieved')

for (i in 1:length(k_list)){
  model <- models[[i]][,1]
  knn_table <- lapply(threshold_list, function(threshold){
    factor(ifelse(model > threshold, 1, 0))
  })
  
  counter <- 0
  knn_accuracy <- sapply(knn_table, function(prediction){
    confusionMatrix(prediction, factor(df[validation_index, 11]))$overall['Accuracy']
  })
  
  KNN_results[[i]] <- cbind(threshold_list, knn_accuracy)
  Max_accuracy <- rbind(Max_accuracy, c(k_list[i], threshold_list[which.max(knn_accuracy)], max(knn_accuracy)))

}


#Seeing more detailed results based on each neighbour. 
KNN_results[[1]]

#Number of Neighbours, threshold, and maximum accuracy
Max_accuracy[which.max(Max_accuracy[,3]),]
KNN_results[[3]]
#It seems that according to our analysis with the training and validation set, the ideal number of neighbours is 5 and ideal threshold is between 0.48 and 0.5. Let's keep it at 0.5. This matches what we did in question 3. 






#Let's test it on the testing set now.

#Running our model
test_model <- kknn(formula = R1 ~ ., train = as.data.frame(df)[training_index,], test = as.data.frame(df)[testing_index,], k = 5, scale = T)
fit <- fitted(test_model) 

#Our Prediction
prediction <- factor(ifelse(fit > 0.5, 1, 0))

#Let's see how we did
confusionMatrix(prediction, factor(df[testing_index, 11]))$overall['Accuracy']

#Not bad! We achieved an accuracy close to what we did on the training set as well. 

