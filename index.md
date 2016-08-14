# Machine Learning Assignment



## Introduction

This assignment is about predicting the manner how barlifts are lifted, i.e. measuring the quality of the exercise. The data for this project comes from the source http://groupware.les.inf.puc-rio.br/har.

**Important Note**: The html output, which supports a browser, can found here [panomarix.github.io](http://panomarix.github.io) and the markdown file in repository [https://github.com/panomarix/panomarix.github.io](https://github.com/panomarix/panomarix.github.io)

## Data Loading and preparation


```r
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
trainingdata <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testingdata <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
```

The "classe" indicates how well the exercise has been performed. The training dataset is to be used for training the model and testing it. The testingdata dataset includes data of 20 different test cases that have to be predicted with the trained.

At first we will perform some data preparation: by removing all NA's in the trainingdata, remove all Columns that have marginal variability and by removing irrelevant columns (e.g user name).


```r
trainingdata <- trainingdata[ , colSums(is.na(trainingdata)) == 0]
zeroVarCols <- nearZeroVar(trainingdata,saveMetrics=TRUE)
trainingdata <- trainingdata[,!zeroVarCols$nzv]
IrrelevantCols <- grepl("X|user|timestamp|window",names(trainingdata))
trainingdata <- trainingdata[,!IrrelevantCols]
dim(trainingdata)
```

```
## [1] 19622    53
```

The dataset is therefore reduced to 53 columns, including the variable to predict.

## Training model 

I have opted to try out random forest, due to the high number of quantitive data and highly correlated values.
In order to also calculate an out of sample error later on, I split up the data in a training and testing dataset.


```r
set.seed(3452)

inTrain <- createDataPartition(trainingdata$classe, p=0.7 , list=FALSE)
training <- trainingdata[inTrain,]
testing <-  trainingdata[-inTrain,]

control=trainControl(method="cv",number=5)
mod1 <- train(classe ~ ., method="rf", trControl=control , data=training)
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
mod1
```

```
## Random Forest 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 10987, 10991, 10991, 10990, 10989 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9920653  0.9899626
##   27    0.9922103  0.9901465
##   52    0.9886438  0.9856337
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

The random forest model has an accuracy of 99.22 %.

## Cross validation

I now use the testset to calulate an out of sample error.


```r
prediction <- predict(mod1,testing)
confusionMatrix(testing$classe,prediction)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1670    2    2    0    0
##          B   11 1123    4    1    0
##          C    0    5 1018    3    0
##          D    0    0   12  952    0
##          E    0    2    0    0 1080
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9929          
##                  95% CI : (0.9904, 0.9949)
##     No Information Rate : 0.2856          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.991           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9935   0.9920   0.9826   0.9958   1.0000
## Specificity            0.9990   0.9966   0.9984   0.9976   0.9996
## Pos Pred Value         0.9976   0.9860   0.9922   0.9876   0.9982
## Neg Pred Value         0.9974   0.9981   0.9963   0.9992   1.0000
## Prevalence             0.2856   0.1924   0.1760   0.1624   0.1835
## Detection Rate         0.2838   0.1908   0.1730   0.1618   0.1835
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9963   0.9943   0.9905   0.9967   0.9998
```

```r
outOfSampleError <- sum(prediction!=testing$classe)/length(testing$classe)
outOfSampleError
```

```
## [1] 0.007136788
```

The out of sample error is 0.7%. Which is fair enough for me.

## Prediction of 20 different test cases (Quiz input)

Part of the assignment is to predict the quality of how well 20 test cases were performed, as a preparation for the additional quiz.


```r
testingdata2 <- testingdata[ , colSums(is.na(testingdata)) == 0] 
prediction2 <- predict(mod1,testingdata2)
prediction2
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```


