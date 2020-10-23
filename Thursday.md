Project 2
================
Jackie Steffan
10/16/2020

``` r
#load in required packages
library(tidyverse)
library(corrplot)
library(ggplot2)
library(GGally)
library(tree)
library(caret)
library(gbm)
library(rpart)
library(rmarkdown)
library(knitr)
```

# Introduction

This data set looks at articles that were published on the website
Mashable. The articles included were published over a 2 year period. The
goal of the data set was to look at the frequency of sharing and the
popularity of each article. According to the data documentation, the
variables url and timedelta are non-predicitve, so I chose not to use
them.  
This analysis is intended to predict the number of shares an article
will have based on other factors such as word count, number of pictures,
etc. I am using 2 tree methods that you will see later in this document.
One is a non-ensemble regression tree and the other is a bosoted tree.

# Data

In this section I am reading in the data described above.  
I then separate the data into a training data set that contains 70% of
the data and a testing data set that contains the remaining 30% of the
data.

``` r
#read in data
popData <- read_csv("./OnlineNewsPopularity.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#combine boolean varaibles to make them easier to work with using mutate
popData <- popData %>% mutate(weekday = ifelse(weekday_is_monday == 1, "Monday", 
                                    ifelse(weekday_is_tuesday == 1, "Tuesday",
                                           ifelse(weekday_is_wednesday==1, "Wednesday",
                                                  ifelse(weekday_is_thursday==1, "Thursday",
                                                         ifelse(weekday_is_friday==1, "Friday",
                                                                ifelse(weekday_is_saturday==1, "Saturday", "Sunday"))))))) %>%
  mutate(channel_type = ifelse(data_channel_is_lifestyle == 1, "Lifestyle",
                               ifelse(data_channel_is_entertainment ==1, "Entertainment",
                                      ifelse(data_channel_is_bus ==1, "Business",
                                             ifelse(data_channel_is_socmed ==1, "Social Media",
                                                    ifelse(data_channel_is_tech == 1, "Tech",
                                                           ifelse(data_channel_is_world ==1, "World", "Other"))))))) %>%
                                    select(-starts_with("weekday_is"), -starts_with("data_channel"))

#filter to select appropriate weekday
weekdayDat <- filter(popData, weekday == params$weekday)

#set seed for reproducibility
set.seed(5)
#split into training and test sets
train <- sample(1:nrow(weekdayDat), size = nrow(weekdayDat)*0.7)
test <- dplyr::setdiff(1:nrow(weekdayDat), train)
DayTrain <- weekdayDat[train, ]
DayTest <- weekdayDat[test, ]
```

# Summarization

Here I am creating summaries for all the predictive variables that are
numeric, the summaries included are min, max, median, mean, and the
first and 3rd quantiles. I am also including a correlation plot that
shows the correlation between our response variable, shares, and the
other predictive numeric variables. Lastly I included a pairs plot of
the response variable and some other predictive variables, which shows
what the 2 variables look like plotted against one another.

``` r
#summary of numeric variables
summary(select(DayTrain,-c("url", "weekday", "timedelta", "channel_type", "is_weekend")))
```

    ##  n_tokens_title  n_tokens_content n_unique_tokens  n_non_stop_words n_non_stop_unique_tokens
    ##  Min.   : 3.00   Min.   :   0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000          
    ##  1st Qu.: 9.00   1st Qu.: 244.0   1st Qu.:0.4742   1st Qu.:1.0000   1st Qu.:0.6269          
    ##  Median :10.00   Median : 395.0   Median :0.5430   Median :1.0000   Median :0.6927          
    ##  Mean   :10.34   Mean   : 536.1   Mean   :0.5333   Mean   :0.9727   Mean   :0.6756          
    ##  3rd Qu.:12.00   3rd Qu.: 698.0   3rd Qu.:0.6098   3rd Qu.:1.0000   3rd Qu.:0.7548          
    ##  Max.   :20.00   Max.   :6159.0   Max.   :0.9545   Max.   :1.0000   Max.   :1.0000          
    ##    num_hrefs      num_self_hrefs     num_imgs         num_videos     average_token_length  num_keywords   
    ##  Min.   :  0.00   Min.   : 0.00   Min.   :  0.000   Min.   : 0.000   Min.   :0.000        Min.   : 1.000  
    ##  1st Qu.:  4.00   1st Qu.: 1.00   1st Qu.:  1.000   1st Qu.: 0.000   1st Qu.:4.484        1st Qu.: 6.000  
    ##  Median :  7.00   Median : 2.00   Median :  1.000   Median : 0.000   Median :4.676        Median : 7.000  
    ##  Mean   : 10.67   Mean   : 3.16   Mean   :  4.527   Mean   : 1.161   Mean   :4.562        Mean   : 7.181  
    ##  3rd Qu.: 13.00   3rd Qu.: 4.00   3rd Qu.:  4.000   3rd Qu.: 1.000   3rd Qu.:4.863        3rd Qu.: 9.000  
    ##  Max.   :120.00   Max.   :56.00   Max.   :100.000   Max.   :51.000   Max.   :6.198        Max.   :10.000  
    ##    kw_min_min       kw_max_min      kw_avg_min        kw_min_max       kw_max_max       kw_avg_max    
    ##  Min.   : -1.00   Min.   :    0   Min.   :   -1.0   Min.   :     0   Min.   : 15000   Min.   :  3842  
    ##  1st Qu.: -1.00   1st Qu.:  445   1st Qu.:  144.6   1st Qu.:     0   1st Qu.:843300   1st Qu.:171327  
    ##  Median : -1.00   Median :  651   Median :  238.9   Median :  1300   Median :843300   Median :245686  
    ##  Mean   : 27.81   Mean   : 1079   Mean   :  302.3   Mean   : 14318   Mean   :749139   Mean   :261054  
    ##  3rd Qu.:  4.00   3rd Qu.: 1000   3rd Qu.:  354.5   3rd Qu.:  7900   3rd Qu.:843300   3rd Qu.:334128  
    ##  Max.   :377.00   Max.   :80400   Max.   :13744.8   Max.   :843300   Max.   :843300   Max.   :843300  
    ##    kw_min_avg       kw_max_avg      kw_avg_avg      self_reference_min_shares self_reference_max_shares
    ##  Min.   :   0.0   Min.   : 2241   Min.   :  675.2   Min.   :     0            Min.   :     0           
    ##  1st Qu.:   0.0   1st Qu.: 3573   1st Qu.: 2372.2   1st Qu.:   596            1st Qu.:  1000           
    ##  Median : 980.5   Median : 4313   Median : 2849.8   Median :  1200            Median :  2800           
    ##  Mean   :1098.3   Mean   : 5561   Mean   : 3111.4   Mean   :  4076            Mean   :  9790           
    ##  3rd Qu.:2045.7   3rd Qu.: 6010   3rd Qu.: 3578.7   3rd Qu.:  2500            3rd Qu.:  7800           
    ##  Max.   :3610.1   Max.   :98032   Max.   :19429.3   Max.   :690400            Max.   :690400           
    ##  self_reference_avg_sharess     LDA_00            LDA_01            LDA_02            LDA_03       
    ##  Min.   :     0.0           Min.   :0.01818   Min.   :0.01818   Min.   :0.01818   Min.   :0.01818  
    ##  1st Qu.:   940.8           1st Qu.:0.02518   1st Qu.:0.02501   1st Qu.:0.02857   1st Qu.:0.02567  
    ##  Median :  2200.0           Median :0.03363   Median :0.03335   Median :0.04001   Median :0.04000  
    ##  Mean   :  6303.8           Mean   :0.19691   Mean   :0.13809   Mean   :0.21508   Mean   :0.21676  
    ##  3rd Qu.:  5100.0           3rd Qu.:0.27598   3rd Qu.:0.15114   3rd Qu.:0.32399   3rd Qu.:0.35336  
    ##  Max.   :690400.0           Max.   :0.92000   Max.   :0.91997   Max.   :0.92000   Max.   :0.91994  
    ##      LDA_04        global_subjectivity global_sentiment_polarity global_rate_positive_words
    ##  Min.   :0.01818   Min.   :0.0000      Min.   :-0.37766          Min.   :0.00000           
    ##  1st Qu.:0.02857   1st Qu.:0.3963      1st Qu.: 0.05795          1st Qu.:0.02878           
    ##  Median :0.05000   Median :0.4526      Median : 0.12012          Median :0.03906           
    ##  Mean   :0.23317   Mean   :0.4435      Mean   : 0.11996          Mean   :0.03959           
    ##  3rd Qu.:0.40045   3rd Qu.:0.5087      3rd Qu.: 0.17823          3rd Qu.:0.05031           
    ##  Max.   :0.91999   Max.   :0.9375      Max.   : 0.72784          Max.   :0.15278           
    ##  global_rate_negative_words rate_positive_words rate_negative_words avg_positive_polarity
    ##  Min.   :0.000000           Min.   :0.0000      Min.   :0.0000      Min.   :0.0000       
    ##  1st Qu.:0.009585           1st Qu.:0.6000      1st Qu.:0.1834      1st Qu.:0.3051       
    ##  Median :0.015504           Median :0.7143      Median :0.2804      Median :0.3576       
    ##  Mean   :0.016652           Mean   :0.6839      Mean   :0.2887      Mean   :0.3528       
    ##  3rd Qu.:0.021739           3rd Qu.:0.8000      3rd Qu.:0.3792      3rd Qu.:0.4117       
    ##  Max.   :0.162037           Max.   :1.0000      Max.   :1.0000      Max.   :0.8500       
    ##  min_positive_polarity max_positive_polarity avg_negative_polarity min_negative_polarity
    ##  Min.   :0.00000       Min.   :0.0000        Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:0.05000       1st Qu.:0.6000        1st Qu.:-0.3263       1st Qu.:-0.7000      
    ##  Median :0.10000       Median :0.8000        Median :-0.2502       Median :-0.5000      
    ##  Mean   :0.09608       Mean   :0.7532        Mean   :-0.2576       Mean   :-0.5165      
    ##  3rd Qu.:0.10000       3rd Qu.:1.0000        3rd Qu.:-0.1843       3rd Qu.:-0.3000      
    ##  Max.   :0.80000       Max.   :1.0000        Max.   : 0.0000       Max.   : 0.0000      
    ##  max_negative_polarity title_subjectivity title_sentiment_polarity abs_title_subjectivity
    ##  Min.   :-1.0000       Min.   :0.0000     Min.   :-1.0000          Min.   :0.0000        
    ##  1st Qu.:-0.1250       1st Qu.:0.0000     1st Qu.: 0.0000          1st Qu.:0.1667        
    ##  Median :-0.1000       Median :0.1667     Median : 0.0000          Median :0.5000        
    ##  Mean   :-0.1076       Mean   :0.2865     Mean   : 0.0720          Mean   :0.3428        
    ##  3rd Qu.:-0.0500       3rd Qu.:0.5000     3rd Qu.: 0.1396          3rd Qu.:0.5000        
    ##  Max.   : 0.0000       Max.   :1.0000     Max.   : 1.0000          Max.   :0.5000        
    ##  abs_title_sentiment_polarity     shares        
    ##  Min.   :0.0000               Min.   :    22.0  
    ##  1st Qu.:0.0000               1st Qu.:   899.2  
    ##  Median :0.0000               Median :  1400.0  
    ##  Mean   :0.1567               Mean   :  3169.5  
    ##  3rd Qu.:0.2500               3rd Qu.:  2600.0  
    ##  Max.   :1.0000               Max.   :306100.0

``` r
#correlation of selected variables
cors <- cor(select(DayTrain, -url, -timedelta, -weekday, -channel_type, -is_weekend))
#correlation plot showing just correlation with the response variable shares
corrplot(cors["shares",,drop=FALSE], type = "upper", tl.pos = "lt",cl.pos = "n")
```

![](Thursday_files/figure-gfm/summary-1.png)<!-- -->

``` r
#pairs data with selected variables
pairs(select(DayTrain, shares, num_hrefs, self_reference_min_shares, kw_max_min, self_reference_avg_sharess, kw_avg_avg, kw_avg_min, average_token_length, global_sentiment_polarity))
```

![](Thursday_files/figure-gfm/summary-2.png)<!-- -->

``` r
pairs(select(DayTrain, shares,num_videos, n_non_stop_unique_tokens, LDA_00, LDA_03, LDA_01, LDA_04, min_negative_polarity, n_unique_tokens))
```

![](Thursday_files/figure-gfm/summary-3.png)<!-- -->

# Models

In this section I am creating 2 tree models. The first is a non-ensemble
based tree selected using leave one out cross-validation. I am also
using cp between 0 and 0.2 for my tuning parameter. The final model is
printed below.  
The second model is a boosted tree selected using repeated cross
validation and the default tuning parameters. The final model is also
printed in the output.  
Lastly, you will find a comparison of RMSE, when deciding which model is
the “better” model you should select the one that has the lower RMSE.

``` r
#select relevant datapoints
treeDat <- select(DayTrain,-c("url", "weekday", "is_weekend", "timedelta"))
#build regression tree using rpart and train using LOOCV and tuning parameter cp
classTree <- train(shares~., data= select(DayTrain,-url, -weekday, -is_weekend, -timedelta), 
                   method= "rpart",
                   trControl = trainControl(method = "LOOCV"),
                   tuneGrid = data.frame(cp=0:0.2),
                   metric = "MAE")
#print results of tree
classTree$results
```

    ##   cp     RMSE    Rsquared      MAE
    ## 1  0 10497.12 0.001732272 3413.992

``` r
#predict the number of shares on the test dataset using the above tree
pred<- predict(classTree, newdata = select(DayTest, -c("url", starts_with("weekday_is"), "is_weekend")))
#calculate RMSE
trRMSE<- sqrt(mean((pred-DayTest$shares)^2))


#boosted model
#build boosted tree using gbm and train with repeated cross validation and default tuning parameters.
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:3)*50, 
                        shrinkage = c(0.1, 0.2),
                        n.minobsinnode = c(10,20))
boostTree <- train(shares ~ ., data = treeDat, method = "gbm",
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                   preProcess = c("center", "scale"), verbose = FALSE,
                   tuneGrid = gbmGrid)
#print results of boosted tree
boostTree$results
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees     RMSE    Rsquared      MAE   RMSESD  RsquaredSD
    ## 1        0.1                 1             10      50 8416.148 0.021365995 2909.298 4496.821 0.022895243
    ## 4        0.1                 1             20      50 8395.233 0.023753635 2890.216 4500.578 0.021921956
    ## 19       0.2                 1             10      50 8464.767 0.021752476 2926.149 4457.743 0.021899080
    ## 22       0.2                 1             20      50 8449.515 0.021598400 2922.446 4466.874 0.019239002
    ## 7        0.1                 5             10      50 8706.700 0.014841969 3018.697 4330.423 0.017415965
    ## 10       0.1                 5             20      50 8579.754 0.016349166 2969.391 4407.126 0.016147358
    ## 25       0.2                 5             10      50 9008.349 0.012576504 3250.114 4223.146 0.014528492
    ## 28       0.2                 5             20      50 8817.179 0.014108443 3217.639 4303.856 0.016922842
    ## 13       0.1                 9             10      50 8771.433 0.013816345 3069.583 4300.482 0.016276688
    ## 16       0.1                 9             20      50 8595.158 0.017195656 3006.467 4398.789 0.014986238
    ## 31       0.2                 9             10      50 9108.968 0.012287303 3392.685 4197.232 0.016385778
    ## 34       0.2                 9             20      50 8904.726 0.011261774 3355.933 4238.893 0.010390272
    ## 2        0.1                 1             10     100 8452.794 0.021813057 2918.124 4467.633 0.021355769
    ## 5        0.1                 1             20     100 8431.038 0.023909808 2903.034 4472.075 0.021280593
    ## 20       0.2                 1             10     100 8470.303 0.022435892 2935.795 4452.234 0.021821252
    ## 23       0.2                 1             20     100 8456.638 0.023859090 2923.224 4459.582 0.020694527
    ## 8        0.1                 5             10     100 8862.749 0.014019619 3142.141 4255.549 0.015835017
    ## 11       0.1                 5             20     100 8727.726 0.014599307 3102.351 4338.304 0.016038080
    ## 26       0.2                 5             10     100 9212.929 0.012270085 3504.073 4124.128 0.013597498
    ## 29       0.2                 5             20     100 9027.686 0.011346296 3475.281 4227.320 0.014582353
    ## 14       0.1                 9             10     100 8988.763 0.012958207 3260.222 4201.332 0.015993698
    ## 17       0.1                 9             20     100 8769.454 0.013716840 3193.038 4313.147 0.013236875
    ## 32       0.2                 9             10     100 9402.941 0.010436905 3733.547 4086.595 0.011282142
    ## 35       0.2                 9             20     100 9210.123 0.008783468 3694.028 4098.604 0.009401073
    ## 3        0.1                 1             10     150 8451.830 0.022656951 2915.602 4459.171 0.020808123
    ## 6        0.1                 1             20     150 8444.555 0.024044647 2910.625 4466.283 0.021722108
    ## 21       0.2                 1             10     150 8483.682 0.023523634 2945.873 4449.227 0.023481869
    ## 24       0.2                 1             20     150 8466.930 0.023696652 2956.567 4452.883 0.020857950
    ## 9        0.1                 5             10     150 8950.697 0.014288678 3243.952 4228.864 0.016667436
    ## 12       0.1                 5             20     150 8805.655 0.014044518 3216.579 4307.014 0.015878581
    ## 27       0.2                 5             10     150 9401.666 0.010187953 3714.470 4067.820 0.010942881
    ## 30       0.2                 5             20     150 9186.281 0.010945958 3673.207 4159.245 0.016058199
    ## 15       0.1                 9             10     150 9133.545 0.011865355 3404.538 4139.379 0.014914359
    ## 18       0.1                 9             20     150 8899.535 0.011747343 3343.122 4266.990 0.011541299
    ## 33       0.2                 9             10     150 9616.573 0.009094839 3983.334 4006.042 0.009934290
    ## 36       0.2                 9             20     150 9409.158 0.007727757 3918.956 4025.297 0.008457698
    ##       MAESD
    ## 1  357.4555
    ## 4  346.7280
    ## 19 346.8172
    ## 22 337.6753
    ## 7  336.3315
    ## 10 333.3913
    ## 25 324.1938
    ## 28 320.0278
    ## 13 321.7907
    ## 16 347.3381
    ## 31 324.9288
    ## 34 296.7601
    ## 2  343.0434
    ## 5  338.4989
    ## 20 343.8515
    ## 23 333.7139
    ## 8  318.9431
    ## 11 321.2929
    ## 26 303.9587
    ## 29 307.0377
    ## 14 292.3863
    ## 17 315.5359
    ## 32 298.5019
    ## 35 267.9244
    ## 3  343.6006
    ## 6  337.3032
    ## 21 346.6183
    ## 24 325.3118
    ## 9  306.3716
    ## 12 305.2085
    ## 27 281.6274
    ## 30 292.1544
    ## 15 271.7929
    ## 18 303.5385
    ## 33 293.2365
    ## 36 258.5561

``` r
#print best tree
boostTree$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 4      50                 1       0.1             20

``` r
#predict number of shares on the test data set using the boosted tree
boostPred <- predict(boostTree, newdata = select(DayTest, -c("url", starts_with("weekday_is"), "is_weekend")), n.trees = 5000)
#calculate RMSE
boostRMSE<- sqrt(mean((boostPred-DayTest$shares)^2))


#print RMSE for both models and compare results.
c(tree = trRMSE, boost = boostRMSE)
```

    ##     tree    boost 
    ## 9770.388 9050.685

# Linear Regression Model

This part of the code was added by Hannah Park as part II of the
project. Here, a linear regression model is fit on the training data set
using the same predictor variables as those used in the two tree models.
Then, predictions of the model are made on the test data set, and the
test MSE from the linear regression model is compared to those of the
tree models.

``` r
# Fit a linear regression
linReg <- train(shares ~ .,
                data = select(DayTrain,-url, -weekday, -is_weekend, -timedelta),
                method = "lm",
                trControl = trainControl(method = "cv", number = 10))
summary(linReg)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -25692  -2223  -1055    128 300033 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -2.631e+03  2.000e+03  -1.316 0.188390    
    ## n_tokens_title                8.430e+01  6.465e+01   1.304 0.192277    
    ## n_tokens_content             -5.024e-02  5.419e-01  -0.093 0.926136    
    ## n_unique_tokens               4.853e+03  4.402e+03   1.103 0.270264    
    ## n_non_stop_words             -6.359e+02  3.786e+03  -0.168 0.866628    
    ## n_non_stop_unique_tokens     -4.142e+03  3.718e+03  -1.114 0.265341    
    ## num_hrefs                     3.875e-01  1.608e+01   0.024 0.980774    
    ## num_self_hrefs               -4.578e+01  4.576e+01  -1.000 0.317134    
    ## num_imgs                      4.538e+01  2.054e+01   2.210 0.027170 *  
    ## num_videos                    1.465e+01  4.183e+01   0.350 0.726142    
    ## average_token_length         -2.459e+02  5.670e+02  -0.434 0.664496    
    ## num_keywords                 -1.477e+02  8.336e+01  -1.772 0.076490 .  
    ## kw_min_min                    1.374e+01  3.823e+00   3.593 0.000331 ***
    ## kw_max_min                   -6.040e-02  1.745e-01  -0.346 0.729313    
    ## kw_avg_min                    2.416e-01  1.153e+00   0.210 0.834002    
    ## kw_min_max                   -1.454e-03  2.512e-03  -0.579 0.562739    
    ## kw_max_max                    3.581e-03  1.355e-03   2.643 0.008242 ** 
    ## kw_avg_max                   -3.092e-03  1.894e-03  -1.633 0.102610    
    ## kw_min_avg                   -7.677e-01  1.758e-01  -4.366 1.29e-05 ***
    ## kw_max_avg                   -2.011e-01  6.609e-02  -3.042 0.002361 ** 
    ## kw_avg_avg                    2.148e+00  3.465e-01   6.200 6.11e-10 ***
    ## self_reference_min_shares     7.848e-02  1.744e-02   4.500 6.96e-06 ***
    ## self_reference_max_shares     9.477e-02  1.061e-02   8.933  < 2e-16 ***
    ## self_reference_avg_sharess   -1.707e-01  2.546e-02  -6.706 2.22e-11 ***
    ## LDA_00                       -9.166e+02  1.033e+03  -0.887 0.375062    
    ## LDA_01                       -9.058e+02  1.155e+03  -0.784 0.432845    
    ## LDA_02                       -2.166e+03  1.033e+03  -2.096 0.036089 *  
    ## LDA_03                       -1.789e+03  1.095e+03  -1.634 0.102347    
    ## LDA_04                               NA         NA      NA       NA    
    ## global_subjectivity           4.356e+03  1.965e+03   2.217 0.026689 *  
    ## global_sentiment_polarity     2.149e+02  3.758e+03   0.057 0.954395    
    ## global_rate_positive_words   -2.061e+04  1.615e+04  -1.276 0.201954    
    ## global_rate_negative_words    1.057e+04  2.984e+04   0.354 0.723087    
    ## rate_positive_words           1.885e+03  2.443e+03   0.772 0.440286    
    ## rate_negative_words                  NA         NA      NA       NA    
    ## avg_positive_polarity        -2.440e+03  3.107e+03  -0.785 0.432305    
    ## min_positive_polarity        -1.938e+03  2.630e+03  -0.737 0.461239    
    ## max_positive_polarity         2.149e+02  9.831e+02   0.219 0.827011    
    ## avg_negative_polarity         2.502e+03  2.896e+03   0.864 0.387586    
    ## min_negative_polarity        -1.177e+03  1.063e+03  -1.108 0.268011    
    ## max_negative_polarity        -4.922e+03  2.431e+03  -2.025 0.042916 *  
    ## title_subjectivity           -4.502e+02  5.994e+02  -0.751 0.452657    
    ## title_sentiment_polarity     -7.321e+02  5.757e+02  -1.272 0.203533    
    ## abs_title_subjectivity        1.907e+02  8.267e+02   0.231 0.817589    
    ## abs_title_sentiment_polarity  1.853e+03  8.956e+02   2.069 0.038604 *  
    ## channel_typeEntertainment    -1.020e+03  8.273e+02  -1.233 0.217819    
    ## channel_typeLifestyle        -7.636e+02  8.321e+02  -0.918 0.358805    
    ## channel_typeOther            -6.953e+02  8.696e+02  -0.800 0.424031    
    ## `channel_typeSocial Media`   -1.023e+03  6.862e+02  -1.490 0.136232    
    ## channel_typeTech             -1.202e+03  7.480e+02  -1.607 0.108085    
    ## channel_typeWorld             5.440e+02  7.944e+02   0.685 0.493514    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9335 on 5037 degrees of freedom
    ## Multiple R-squared:  0.05081,    Adjusted R-squared:  0.04177 
    ## F-statistic: 5.618 on 48 and 5037 DF,  p-value: < 2.2e-16

``` r
# Make predictions using the Test set
lmPred <- predict(linReg, DayTest)
DayTest$predictions <- lmPred
kable(head(select(DayTest, shares, predictions)))
```

| shares | predictions |
| -----: | ----------: |
|   1200 |   1944.4038 |
|   4500 |   1701.7096 |
|   1200 |   2424.7137 |
|    377 |    342.1755 |
|    972 |   1220.9359 |
|    737 |   1978.3525 |

``` r
# Compare test RMSE of lm model to those of the two tree models
lmRMSE <- sqrt(mean((lmPred - DayTest$shares)^2))
kable(c("Regression Tree" = trRMSE, 
        "Boosted Tree" = boostRMSE, 
        "Linear Regression" = lmRMSE),
      col.names = "RMSE")
```

|                   |     RMSE |
| ----------------- | -------: |
| Regression Tree   | 9770.388 |
| Boosted Tree      | 9050.685 |
| Linear Regression | 9101.346 |
