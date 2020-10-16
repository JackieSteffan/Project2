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
weekdayDat <- filter(popData, weekday== params$weekday)

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

    ##  n_tokens_title  n_tokens_content n_unique_tokens  n_non_stop_words n_non_stop_unique_tokens   num_hrefs      num_self_hrefs      num_imgs     
    ##  Min.   : 4.00   Min.   :   0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000           Min.   :  0.00   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.: 9.00   1st Qu.: 248.0   1st Qu.:0.4743   1st Qu.:1.0000   1st Qu.:0.6287           1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.: 1.000  
    ##  Median :10.00   Median : 396.0   Median :0.5410   Median :1.0000   Median :0.6911           Median :  7.00   Median : 3.000   Median : 1.000  
    ##  Mean   :10.45   Mean   : 541.2   Mean   :0.5309   Mean   :0.9704   Mean   :0.6724           Mean   : 10.69   Mean   : 3.307   Mean   : 4.499  
    ##  3rd Qu.:12.00   3rd Qu.: 691.0   3rd Qu.:0.6075   3rd Qu.:1.0000   3rd Qu.:0.7525           3rd Qu.: 13.00   3rd Qu.: 4.000   3rd Qu.: 4.000  
    ##  Max.   :19.00   Max.   :5530.0   Max.   :0.9730   Max.   :1.0000   Max.   :1.0000           Max.   :122.00   Max.   :62.000   Max.   :99.000  
    ##    num_videos     average_token_length  num_keywords      kw_min_min      kw_max_min       kw_avg_min        kw_min_max       kw_max_max    
    ##  Min.   : 0.000   Min.   :0.000        Min.   : 1.000   Min.   : -1.0   Min.   :     0   Min.   :   -1.0   Min.   :     0   Min.   : 17100  
    ##  1st Qu.: 0.000   1st Qu.:4.478        1st Qu.: 6.000   1st Qu.: -1.0   1st Qu.:   441   1st Qu.:  141.0   1st Qu.:     0   1st Qu.:843300  
    ##  Median : 0.000   Median :4.660        Median : 7.000   Median : -1.0   Median :   659   Median :  233.7   Median :  1300   Median :843300  
    ##  Mean   : 1.312   Mean   :4.545        Mean   : 7.178   Mean   : 25.1   Mean   :  1091   Mean   :  301.6   Mean   : 13017   Mean   :755226  
    ##  3rd Qu.: 1.000   3rd Qu.:4.853        3rd Qu.: 9.000   3rd Qu.:  4.0   3rd Qu.:  1000   3rd Qu.:  355.0   3rd Qu.:  8000   3rd Qu.:843300  
    ##  Max.   :59.000   Max.   :7.975        Max.   :10.000   Max.   :217.0   Max.   :139600   Max.   :15851.2   Max.   :843300   Max.   :843300  
    ##    kw_avg_max       kw_min_avg       kw_max_avg       kw_avg_avg      self_reference_min_shares self_reference_max_shares
    ##  Min.   :  3460   Min.   :  -1.0   Min.   :  2019   Min.   :  784.2   Min.   :     0            Min.   :     0           
    ##  1st Qu.:173100   1st Qu.:   0.0   1st Qu.:  3540   1st Qu.: 2373.9   1st Qu.:   642            1st Qu.:  1100           
    ##  Median :243387   Median : 981.2   Median :  4309   Median : 2849.9   Median :  1200            Median :  2900           
    ##  Mean   :261425   Mean   :1110.7   Mean   :  5616   Mean   : 3133.4   Mean   :  4229            Mean   : 10472           
    ##  3rd Qu.:333850   3rd Qu.:2070.5   3rd Qu.:  6021   3rd Qu.: 3565.8   3rd Qu.:  2700            3rd Qu.:  8300           
    ##  Max.   :843300   Max.   :3609.7   Max.   :178675   Max.   :29240.8   Max.   :690400            Max.   :843300           
    ##  self_reference_avg_sharess     LDA_00            LDA_01            LDA_02            LDA_03            LDA_04        global_subjectivity
    ##  Min.   :     0             Min.   :0.01819   Min.   :0.01825   Min.   :0.01818   Min.   :0.01818   Min.   :0.01844   Min.   :0.0000     
    ##  1st Qu.:  1000             1st Qu.:0.02507   1st Qu.:0.02502   1st Qu.:0.02857   1st Qu.:0.02857   1st Qu.:0.02857   1st Qu.:0.3949     
    ##  Median :  2267             Median :0.03337   Median :0.03334   Median :0.04001   Median :0.04000   Median :0.05000   Median :0.4520     
    ##  Mean   :  6630             Mean   :0.18155   Mean   :0.13718   Mean   :0.21561   Mean   :0.22252   Mean   :0.24314   Mean   :0.4414     
    ##  3rd Qu.:  5400             3rd Qu.:0.23969   3rd Qu.:0.13673   3rd Qu.:0.32799   3rd Qu.:0.36851   3rd Qu.:0.43521   3rd Qu.:0.5064     
    ##  Max.   :690400             Max.   :0.91998   Max.   :0.91994   Max.   :0.92000   Max.   :0.91997   Max.   :0.92719   Max.   :0.8714     
    ##  global_sentiment_polarity global_rate_positive_words global_rate_negative_words rate_positive_words rate_negative_words avg_positive_polarity
    ##  Min.   :-0.30881          Min.   :0.00000            Min.   :0.000000           Min.   :0.0000      Min.   :0.0000      Min.   :0.0000       
    ##  1st Qu.: 0.05865          1st Qu.:0.02857            1st Qu.:0.009504           1st Qu.:0.6000      1st Qu.:0.1818      1st Qu.:0.3045       
    ##  Median : 0.12042          Median :0.03918            Median :0.015177           Median :0.7143      Median :0.2759      Median :0.3567       
    ##  Mean   : 0.11915          Mean   :0.03969            Mean   :0.016436           Mean   :0.6853      Mean   :0.2847      Mean   :0.3505       
    ##  3rd Qu.: 0.17688          3rd Qu.:0.05008            3rd Qu.:0.021365           3rd Qu.:0.8000      3rd Qu.:0.3793      3rd Qu.:0.4071       
    ##  Max.   : 0.61923          Max.   :0.11458            Max.   :0.135294           Max.   :1.0000      Max.   :1.0000      Max.   :0.8000       
    ##  min_positive_polarity max_positive_polarity avg_negative_polarity min_negative_polarity max_negative_polarity title_subjectivity
    ##  Min.   :0.00000       Min.   :0.0000        Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000       Min.   :0.0000    
    ##  1st Qu.:0.05000       1st Qu.:0.6000        1st Qu.:-0.3250       1st Qu.:-0.7000       1st Qu.:-0.1250       1st Qu.:0.0000    
    ##  Median :0.10000       Median :0.8000        Median :-0.2500       Median :-0.5000       Median :-0.1000       Median :0.1250    
    ##  Mean   :0.09434       Mean   :0.7523        Mean   :-0.2566       Mean   :-0.5148       Mean   :-0.1074       Mean   :0.2797    
    ##  3rd Qu.:0.10000       3rd Qu.:1.0000        3rd Qu.:-0.1833       3rd Qu.:-0.3000       3rd Qu.:-0.0500       3rd Qu.:0.5000    
    ##  Max.   :0.60000       Max.   :1.0000        Max.   : 0.0000       Max.   : 0.0000       Max.   : 0.0000       Max.   :1.0000    
    ##  title_sentiment_polarity abs_title_subjectivity abs_title_sentiment_polarity     shares      
    ##  Min.   :-1.00000         Min.   :0.0000         Min.   :0.000                Min.   :    42  
    ##  1st Qu.: 0.00000         1st Qu.:0.1667         1st Qu.:0.000                1st Qu.:   900  
    ##  Median : 0.00000         Median :0.5000         Median :0.000                Median :  1300  
    ##  Mean   : 0.07156         Mean   :0.3446         Mean   :0.157                Mean   :  3262  
    ##  3rd Qu.: 0.13636         3rd Qu.:0.5000         3rd Qu.:0.250                3rd Qu.:  2500  
    ##  Max.   : 1.00000         Max.   :0.5000         Max.   :1.000                Max.   :441000

``` r
#correlation of selected variables
cors <- cor(select(DayTrain, -url, -timedelta, -weekday, -channel_type, -is_weekend))
#correlation plot showing just correlation with the response variable shares
corrplot(cors["shares",,drop=FALSE], type = "upper", tl.pos = "lt",cl.pos = "n")
```

![](Tuesday_files/figure-gfm/summary-1.png)<!-- -->

``` r
#pairs data with selected variables
pairs(select(DayTrain, shares, num_hrefs, self_reference_min_shares, kw_max_min, self_reference_avg_sharess, kw_avg_avg, kw_avg_min, average_token_length, global_sentiment_polarity))
```

![](Tuesday_files/figure-gfm/summary-2.png)<!-- -->

``` r
pairs(select(DayTrain, shares,num_videos, n_non_stop_unique_tokens, LDA_00, LDA_03, LDA_01, LDA_04, min_negative_polarity, n_unique_tokens))
```

![](Tuesday_files/figure-gfm/summary-3.png)<!-- -->

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
    ## 1  0 11717.55 0.003146384 3443.598

``` r
#predict the number of shares on the test dataset using the above tree
pred<- predict(classTree, newdata = select(DayTest, -c("url", starts_with("weekday_is"), "is_weekend")))
#calculate RMSE
trRMSE<- sqrt(mean((pred-DayTest$shares)^2))


#boosted model
#build boosted tree using gbm and train with repeated cross validation and default tuning parameters.
boostTree <- train(shares ~ ., data = treeDat, method = "gbm",
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                   preProcess = c("center", "scale"), verbose = FALSE)
#print results of boosted tree
boostTree$results
```

    ##   shrinkage interaction.depth n.minobsinnode n.trees     RMSE   Rsquared      MAE   RMSESD RsquaredSD    MAESD
    ## 1       0.1                 1             10      50 9373.352 0.03252371 3049.731 5038.198 0.02925832 440.3515
    ## 4       0.1                 2             10      50 9492.213 0.02828699 3083.565 4988.696 0.02656925 446.9339
    ## 7       0.1                 3             10      50 9617.186 0.02281443 3136.943 4918.473 0.02654742 422.7180
    ## 2       0.1                 1             10     100 9408.380 0.03229929 3062.392 5018.264 0.02978144 441.8990
    ## 5       0.1                 2             10     100 9596.079 0.02657570 3136.569 4950.914 0.02491164 441.1486
    ## 8       0.1                 3             10     100 9769.892 0.01952895 3222.491 4851.966 0.02370665 408.8830
    ## 3       0.1                 1             10     150 9435.446 0.03046813 3080.278 5006.067 0.02696026 435.2726
    ## 6       0.1                 2             10     150 9695.301 0.02355419 3197.325 4922.815 0.02309736 447.0157
    ## 9       0.1                 3             10     150 9913.733 0.01666869 3322.430 4798.417 0.01938310 395.5403

``` r
#print best tree
boostTree$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 1      50                 1       0.1             10

``` r
#predict number of shares on the test data set using the boosted tree
boostPred <- predict(boostTree, newdata = select(DayTest, -c("url", starts_with("weekday_is"), "is_weekend")), n.trees = 5000)
#calculate RMSE
boostRMSE<- sqrt(mean((boostPred-DayTest$shares)^2))


#print RMSE for both models and compare results.
c(tree = trRMSE, boost = boostRMSE)
```

    ##     tree    boost 
    ## 8545.452 7310.172
