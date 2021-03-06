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
    ##  Min.   : 4.00   Min.   :   0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000          
    ##  1st Qu.: 9.00   1st Qu.: 244.0   1st Qu.:0.4726   1st Qu.:1.0000   1st Qu.:0.6278          
    ##  Median :10.00   Median : 401.0   Median :0.5408   Median :1.0000   Median :0.6927          
    ##  Mean   :10.43   Mean   : 531.1   Mean   :0.5315   Mean   :0.9693   Mean   :0.6747          
    ##  3rd Qu.:12.00   3rd Qu.: 701.2   3rd Qu.:0.6111   3rd Qu.:1.0000   3rd Qu.:0.7576          
    ##  Max.   :18.00   Max.   :4747.0   Max.   :0.9714   Max.   :1.0000   Max.   :1.0000          
    ##    num_hrefs      num_self_hrefs      num_imgs        num_videos     average_token_length  num_keywords   
    ##  Min.   :  0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000        Min.   : 1.000  
    ##  1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 0.000   1st Qu.:4.478        1st Qu.: 6.000  
    ##  Median :  7.00   Median : 2.000   Median : 1.000   Median : 0.000   Median :4.662        Median : 7.000  
    ##  Mean   : 10.15   Mean   : 3.132   Mean   : 4.096   Mean   : 1.239   Mean   :4.542        Mean   : 7.136  
    ##  3rd Qu.: 12.00   3rd Qu.: 4.000   3rd Qu.: 3.000   3rd Qu.: 1.000   3rd Qu.:4.851        3rd Qu.: 9.000  
    ##  Max.   :150.00   Max.   :43.000   Max.   :92.000   Max.   :58.000   Max.   :6.610        Max.   :10.000  
    ##    kw_min_min      kw_max_min       kw_avg_min        kw_min_max       kw_max_max       kw_avg_max    
    ##  Min.   : -1.0   Min.   :     0   Min.   :   -1.0   Min.   :     0   Min.   : 17100   Min.   :  2240  
    ##  1st Qu.: -1.0   1st Qu.:   442   1st Qu.:  141.4   1st Qu.:     0   1st Qu.:690400   1st Qu.:171686  
    ##  Median : -1.0   Median :   657   Median :  237.8   Median :  1300   Median :843300   Median :245568  
    ##  Mean   : 27.5   Mean   :  1187   Mean   :  319.9   Mean   : 14609   Mean   :746712   Mean   :261087  
    ##  3rd Qu.:  4.0   3rd Qu.:  1000   3rd Qu.:  358.1   3rd Qu.:  7300   3rd Qu.:843300   3rd Qu.:334905  
    ##  Max.   :294.0   Max.   :111300   Max.   :18687.8   Max.   :843300   Max.   :843300   Max.   :843300  
    ##    kw_min_avg     kw_max_avg       kw_avg_avg      self_reference_min_shares self_reference_max_shares
    ##  Min.   :  -1   Min.   :  1953   Min.   :  503.4   Min.   :     0.0          Min.   :     0           
    ##  1st Qu.:   0   1st Qu.:  3529   1st Qu.: 2364.6   1st Qu.:   631.2          1st Qu.:  1000           
    ##  Median :1000   Median :  4266   Median : 2823.0   Median :  1200.0          Median :  2900           
    ##  Mean   :1084   Mean   :  5532   Mean   : 3085.4   Mean   :  4005.2          Mean   : 10131           
    ##  3rd Qu.:1988   3rd Qu.:  5863   3rd Qu.: 3523.7   3rd Qu.:  2700.0          3rd Qu.:  7800           
    ##  Max.   :3613   Max.   :135125   Max.   :21000.7   Max.   :690400.0          Max.   :837700           
    ##  self_reference_avg_sharess     LDA_00            LDA_01            LDA_02            LDA_03       
    ##  Min.   :     0.0           Min.   :0.01818   Min.   :0.01819   Min.   :0.01819   Min.   :0.01820  
    ##  1st Qu.:   963.1           1st Qu.:0.02519   1st Qu.:0.02503   1st Qu.:0.02857   1st Qu.:0.02857  
    ##  Median :  2200.0           Median :0.03351   Median :0.03335   Median :0.04002   Median :0.04000  
    ##  Mean   :  6418.6           Mean   :0.19268   Mean   :0.13545   Mean   :0.21660   Mean   :0.21522  
    ##  3rd Qu.:  5065.3           3rd Qu.:0.26865   3rd Qu.:0.14492   3rd Qu.:0.33315   3rd Qu.:0.34046  
    ##  Max.   :690400.0           Max.   :0.92000   Max.   :0.91998   Max.   :0.92000   Max.   :0.91998  
    ##      LDA_04        global_subjectivity global_sentiment_polarity global_rate_positive_words
    ##  Min.   :0.01818   Min.   :0.0000      Min.   :-0.37500          Min.   :0.00000           
    ##  1st Qu.:0.02858   1st Qu.:0.3944      1st Qu.: 0.05949          1st Qu.:0.02817           
    ##  Median :0.05000   Median :0.4507      Median : 0.11988          Median :0.03880           
    ##  Mean   :0.24005   Mean   :0.4420      Mean   : 0.11857          Mean   :0.03948           
    ##  3rd Qu.:0.41687   3rd Qu.:0.5061      3rd Qu.: 0.17779          3rd Qu.:0.04968           
    ##  Max.   :0.92712   Max.   :1.0000      Max.   : 0.57374          Max.   :0.15549           
    ##  global_rate_negative_words rate_positive_words rate_negative_words avg_positive_polarity
    ##  Min.   :0.000000           Min.   :0.0000      Min.   :0.0000      Min.   :0.0000       
    ##  1st Qu.:0.009434           1st Qu.:0.6000      1st Qu.:0.1818      1st Qu.:0.3067       
    ##  Median :0.015038           Median :0.7143      Median :0.2753      Median :0.3590       
    ##  Mean   :0.016287           Mean   :0.6849      Mean   :0.2844      Mean   :0.3524       
    ##  3rd Qu.:0.021390           3rd Qu.:0.8057      3rd Qu.:0.3824      3rd Qu.:0.4086       
    ##  Max.   :0.085897           Max.   :1.0000      Max.   :1.0000      Max.   :1.0000       
    ##  min_positive_polarity max_positive_polarity avg_negative_polarity min_negative_polarity
    ##  Min.   :0.00000       Min.   :0.000         Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:0.05000       1st Qu.:0.600         1st Qu.:-0.3284       1st Qu.:-0.7000      
    ##  Median :0.10000       Median :0.800         Median :-0.2506       Median :-0.5000      
    ##  Mean   :0.09518       Mean   :0.751         Mean   :-0.2582       Mean   :-0.5176      
    ##  3rd Qu.:0.10000       3rd Qu.:1.000         3rd Qu.:-0.1853       3rd Qu.:-0.3000      
    ##  Max.   :1.00000       Max.   :1.000         Max.   : 0.0000       Max.   : 0.0000      
    ##  max_negative_polarity title_subjectivity title_sentiment_polarity abs_title_subjectivity
    ##  Min.   :-1.0000       Min.   :0.00       Min.   :-1.0000          Min.   :0.0000        
    ##  1st Qu.:-0.1250       1st Qu.:0.00       1st Qu.: 0.0000          1st Qu.:0.1667        
    ##  Median :-0.1000       Median :0.10       Median : 0.0000          Median :0.5000        
    ##  Mean   :-0.1071       Mean   :0.28       Mean   : 0.0660          Mean   :0.3440        
    ##  3rd Qu.:-0.0500       3rd Qu.:0.50       3rd Qu.: 0.1364          3rd Qu.:0.5000        
    ##  Max.   : 0.0000       Max.   :1.00       Max.   : 1.0000          Max.   :0.5000        
    ##  abs_title_sentiment_polarity     shares      
    ##  Min.   :0.0000               Min.   :    49  
    ##  1st Qu.:0.0000               1st Qu.:   885  
    ##  Median :0.0000               Median :  1300  
    ##  Mean   :0.1533               Mean   :  3007  
    ##  3rd Qu.:0.2500               3rd Qu.:  2500  
    ##  Max.   :1.0000               Max.   :158900

``` r
#correlation of selected variables
cors <- cor(select(DayTrain, -url, -timedelta, -weekday, -channel_type, -is_weekend))
#correlation plot showing just correlation with the response variable shares
corrplot(cors["shares",,drop=FALSE], type = "upper", tl.pos = "lt",cl.pos = "n")
```

![](Wednesday_files/figure-gfm/summary-1.png)<!-- -->

``` r
#pairs data with selected variables
pairs(select(DayTrain, shares, num_hrefs, self_reference_min_shares, kw_max_min, self_reference_avg_sharess, kw_avg_avg, kw_avg_min, average_token_length, global_sentiment_polarity))
```

![](Wednesday_files/figure-gfm/summary-2.png)<!-- -->

``` r
pairs(select(DayTrain, shares,num_videos, n_non_stop_unique_tokens, LDA_00, LDA_03, LDA_01, LDA_04, min_negative_polarity, n_unique_tokens))
```

![](Wednesday_files/figure-gfm/summary-3.png)<!-- -->

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

    ##   cp     RMSE  Rsquared      MAE
    ## 1  0 7595.666 0.0118088 3062.321

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

    ##    shrinkage interaction.depth n.minobsinnode n.trees     RMSE   Rsquared      MAE   RMSESD  RsquaredSD
    ## 1        0.1                 1             10      50 6704.796 0.03343407 2678.325 1609.596 0.017278486
    ## 4        0.1                 1             20      50 6683.875 0.03748253 2662.586 1618.063 0.019494691
    ## 19       0.2                 1             10      50 6734.165 0.03300895 2692.802 1590.047 0.018832557
    ## 22       0.2                 1             20      50 6704.123 0.03769590 2682.602 1587.570 0.018178235
    ## 7        0.1                 5             10      50 6839.470 0.02656350 2729.991 1541.223 0.017615572
    ## 10       0.1                 5             20      50 6773.076 0.03299520 2707.993 1578.576 0.020866254
    ## 25       0.2                 5             10      50 7022.539 0.02197833 2840.810 1489.137 0.019949325
    ## 28       0.2                 5             20      50 6917.864 0.02475880 2815.415 1511.610 0.014203179
    ## 13       0.1                 9             10      50 6876.605 0.02623702 2763.103 1554.031 0.016877949
    ## 16       0.1                 9             20      50 6789.950 0.03176348 2726.609 1577.008 0.017818079
    ## 31       0.2                 9             10      50 7171.800 0.01623151 2967.030 1491.808 0.012561541
    ## 34       0.2                 9             20      50 6994.946 0.02436489 2886.826 1526.515 0.020633831
    ## 2        0.1                 1             10     100 6719.971 0.03384646 2679.306 1591.745 0.017446020
    ## 5        0.1                 1             20     100 6704.868 0.03707942 2675.155 1595.923 0.018988465
    ## 20       0.2                 1             10     100 6756.320 0.03197000 2698.332 1565.784 0.018172089
    ## 23       0.2                 1             20     100 6719.155 0.03687615 2690.343 1580.512 0.018323800
    ## 8        0.1                 5             10     100 6937.359 0.02232331 2781.516 1513.547 0.014366225
    ## 11       0.1                 5             20     100 6842.030 0.02913167 2747.960 1555.067 0.020190352
    ## 26       0.2                 5             10     100 7177.883 0.01862030 2977.566 1440.711 0.016556801
    ## 29       0.2                 5             20     100 7040.226 0.02160727 2927.211 1488.159 0.014810079
    ## 14       0.1                 9             10     100 6999.931 0.02250452 2843.417 1516.177 0.016015919
    ## 17       0.1                 9             20     100 6891.532 0.02579427 2806.358 1551.582 0.014654270
    ## 32       0.2                 9             10     100 7358.082 0.01293567 3152.413 1457.293 0.010219894
    ## 35       0.2                 9             20     100 7163.791 0.01900331 3073.345 1491.551 0.016824083
    ## 3        0.1                 1             10     150 6735.871 0.03364299 2689.396 1581.784 0.018311289
    ## 6        0.1                 1             20     150 6711.525 0.03730203 2684.575 1585.119 0.018223869
    ## 21       0.2                 1             10     150 6780.659 0.03019282 2715.000 1560.385 0.016611857
    ## 24       0.2                 1             20     150 6732.843 0.03538838 2698.984 1569.430 0.017770423
    ## 9        0.1                 5             10     150 7013.859 0.02043270 2830.233 1480.081 0.013758773
    ## 12       0.1                 5             20     150 6893.808 0.02644779 2793.465 1548.126 0.017992621
    ## 27       0.2                 5             10     150 7303.520 0.01556737 3079.843 1411.349 0.012856397
    ## 30       0.2                 5             20     150 7117.284 0.02120914 3026.557 1474.529 0.017048142
    ## 15       0.1                 9             10     150 7079.455 0.02035726 2917.694 1494.438 0.015045005
    ## 18       0.1                 9             20     150 6948.384 0.02498301 2876.553 1537.343 0.014872232
    ## 33       0.2                 9             10     150 7476.229 0.01203479 3286.950 1403.383 0.009851006
    ## 36       0.2                 9             20     150 7269.902 0.01712625 3198.546 1473.045 0.015379576
    ##       MAESD
    ## 1  236.1064
    ## 4  241.1385
    ## 19 237.1804
    ## 22 230.7682
    ## 7  234.5412
    ## 10 249.0260
    ## 25 242.4340
    ## 28 242.1068
    ## 13 258.6701
    ## 16 248.8297
    ## 31 258.3744
    ## 34 256.1373
    ## 2  235.6651
    ## 5  234.2933
    ## 20 236.5745
    ## 23 233.5682
    ## 8  235.4699
    ## 11 244.4749
    ## 26 236.5452
    ## 29 240.1617
    ## 14 260.9392
    ## 17 250.0410
    ## 32 250.7823
    ## 35 253.5273
    ## 3  232.5930
    ## 6  236.6227
    ## 21 240.4396
    ## 24 227.7126
    ## 9  238.6524
    ## 12 242.4060
    ## 27 234.2598
    ## 30 234.1350
    ## 15 259.7869
    ## 18 241.8541
    ## 33 257.2242
    ## 36 244.4407

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
    ## 24533.78 24262.93

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
    ## -11396  -2075   -976    167 150234 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -4.320e+03  1.391e+03  -3.105 0.001910 ** 
    ## n_tokens_title               -3.998e+00  4.677e+01  -0.085 0.931886    
    ## n_tokens_content              1.764e+00  4.190e-01   4.210 2.59e-05 ***
    ## n_unique_tokens               3.933e+03  3.201e+03   1.229 0.219241    
    ## n_non_stop_words              1.682e+03  2.800e+03   0.601 0.548101    
    ## n_non_stop_unique_tokens     -3.619e+02  2.703e+03  -0.134 0.893493    
    ## num_hrefs                     4.066e+01  1.204e+01   3.377 0.000738 ***
    ## num_self_hrefs               -1.177e+02  3.403e+01  -3.457 0.000550 ***
    ## num_imgs                     -7.867e-01  1.614e+01  -0.049 0.961136    
    ## num_videos                   -1.311e+01  2.790e+01  -0.470 0.638401    
    ## average_token_length         -7.901e+02  4.027e+02  -1.962 0.049802 *  
    ## num_keywords                  1.064e+02  6.170e+01   1.724 0.084698 .  
    ## kw_min_min                    6.424e+00  2.774e+00   2.316 0.020606 *  
    ## kw_max_min                    2.296e-01  1.203e-01   1.910 0.056235 .  
    ## kw_avg_min                   -1.451e+00  7.294e-01  -1.989 0.046775 *  
    ## kw_min_max                   -9.942e-04  1.673e-03  -0.594 0.552255    
    ## kw_max_max                    3.168e-04  9.749e-04   0.325 0.745257    
    ## kw_avg_max                   -2.155e-04  1.358e-03  -0.159 0.873903    
    ## kw_min_avg                   -2.510e-01  1.280e-01  -1.961 0.049952 *  
    ## kw_max_avg                   -1.961e-01  4.168e-02  -4.706 2.59e-06 ***
    ## kw_avg_avg                    1.705e+00  2.444e-01   6.976 3.42e-12 ***
    ## self_reference_min_shares    -1.323e-03  1.250e-02  -0.106 0.915668    
    ## self_reference_max_shares    -5.608e-03  6.916e-03  -0.811 0.417476    
    ## self_reference_avg_sharess    1.418e-02  1.738e-02   0.816 0.414601    
    ## LDA_00                        1.031e+03  7.542e+02   1.367 0.171727    
    ## LDA_01                        9.800e+01  8.591e+02   0.114 0.909186    
    ## LDA_02                        4.538e+01  7.596e+02   0.060 0.952359    
    ## LDA_03                        8.728e+02  8.024e+02   1.088 0.276738    
    ## LDA_04                               NA         NA      NA       NA    
    ## global_subjectivity          -1.272e+03  1.417e+03  -0.897 0.369538    
    ## global_sentiment_polarity     4.252e+03  2.805e+03   1.516 0.129552    
    ## global_rate_positive_words   -1.938e+04  1.188e+04  -1.632 0.102742    
    ## global_rate_negative_words    3.604e+04  2.393e+04   1.506 0.132068    
    ## rate_positive_words           2.391e+03  1.982e+03   1.206 0.227897    
    ## rate_negative_words                  NA         NA      NA       NA    
    ## avg_positive_polarity        -2.517e+03  2.288e+03  -1.100 0.271190    
    ## min_positive_polarity        -3.211e+02  1.850e+03  -0.174 0.862177    
    ## max_positive_polarity        -9.137e+02  7.097e+02  -1.287 0.197995    
    ## avg_negative_polarity        -5.670e+03  2.063e+03  -2.748 0.006011 ** 
    ## min_negative_polarity         1.209e+03  7.556e+02   1.600 0.109750    
    ## max_negative_polarity         3.439e+03  1.723e+03   1.996 0.046011 *  
    ## title_subjectivity            1.231e+03  4.522e+02   2.723 0.006499 ** 
    ## title_sentiment_polarity      7.453e+02  4.039e+02   1.845 0.065081 .  
    ## abs_title_subjectivity        1.461e+03  5.973e+02   2.446 0.014488 *  
    ## abs_title_sentiment_polarity -5.395e+02  6.493e+02  -0.831 0.406067    
    ## channel_typeEntertainment    -1.580e+02  6.069e+02  -0.260 0.794652    
    ## channel_typeLifestyle        -2.925e+02  6.077e+02  -0.481 0.630370    
    ## channel_typeOther             2.401e+02  6.340e+02   0.379 0.704938    
    ## `channel_typeSocial Media`    2.566e+02  5.087e+02   0.504 0.613986    
    ## channel_typeTech              8.711e+02  5.407e+02   1.611 0.107235    
    ## channel_typeWorld             5.763e+01  5.899e+02   0.098 0.922184    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6794 on 5155 degrees of freedom
    ## Multiple R-squared:  0.05968,    Adjusted R-squared:  0.05093 
    ## F-statistic: 6.816 on 48 and 5155 DF,  p-value: < 2.2e-16

``` r
# Make predictions using the Test set
lmPred <- predict(linReg, DayTest)
DayTest$predictions <- lmPred
kable(head(select(DayTest, shares, predictions)))
```

| shares | predictions |
| -----: | ----------: |
|   1100 |   2059.3610 |
|    459 |    771.1894 |
|   1400 |   2871.4261 |
|    341 |   2532.6832 |
|   1200 |   3383.4562 |
|    663 |    917.6046 |

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
| Regression Tree   | 24533.78 |
| Boosted Tree      | 24262.93 |
| Linear Regression | 24290.42 |
