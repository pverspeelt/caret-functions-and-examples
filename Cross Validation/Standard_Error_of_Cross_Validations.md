Standard Error of Cross Validation Predictions
================

This [question](http://stats.stackexchange.com/questions/206139/is-there-a-way-to-return-the-standard-error-of-cross-validation-predictions-usin/206509#206509) was asked at cross validtated. I recreated the code from the book Applied Predictive Modelling.

``` r
library(caret)
data(GermanCredit)
GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL


set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

library(kernlab)
set.seed(231)
sigDist <- sigest(Class ~ ., data = GermanCreditTrain, frac = 1)
svmTuneGrid <- data.frame(sigma = as.vector(sigDist)[1], C = 2^(-2:7))

set.seed(1056)
svmFit10CV <- train(Class ~ .,
                    data = GermanCreditTrain,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneGrid = svmTuneGrid,
                    trControl = trainControl(method = "cv", number = 10))
```

The plot is based on the information from chapter 4 of Applied Predictive Modelling.

``` r
library(dplyr)

svmFit10CV$results %>%
  mutate(accuracySD_low = Accuracy - 2*(AccuracySD/sqrt(svmFit10CV$control$number * svmFit10CV$control$repeats)),
         accuracySD_high = Accuracy + 2*(AccuracySD/sqrt(svmFit10CV$control$number * svmFit10CV$control$repeats))) %>%
  ggplot(aes(x = C)) +
  geom_line(aes(y = Accuracy)) +
  geom_point(aes(y = Accuracy)) +
  scale_x_log10() + 
  ylim(0.65, 0.8) + 
  geom_errorbar(aes(ymin=accuracySD_low, ymax=accuracySD_high), 
                colour="gray50",
                width=.1) +
  labs(title="CV accuracy\nwith 2 SD errror bars")
```

![](Standard_Error_of_Cross_Validations_files/figure-markdown_github/pressure-1.png)
