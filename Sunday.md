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
    ##  Min.   : 5.00   Min.   :   0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000           Min.   :  0.00   Min.   : 0.000   Min.   :  0.000  
    ##  1st Qu.: 9.00   1st Qu.: 244.0   1st Qu.:0.4612   1st Qu.:1.0000   1st Qu.:0.6158           1st Qu.:  5.00   1st Qu.: 1.000   1st Qu.:  1.000  
    ##  Median :10.00   Median : 461.0   Median :0.5280   Median :1.0000   Median :0.6803           Median :  9.00   Median : 2.000   Median :  1.000  
    ##  Mean   :10.51   Mean   : 610.8   Mean   :0.5286   Mean   :0.9728   Mean   :0.6671           Mean   : 12.61   Mean   : 3.544   Mean   :  5.805  
    ##  3rd Qu.:12.00   3rd Qu.: 822.5   3rd Qu.:0.6087   3rd Qu.:1.0000   3rd Qu.:0.7534           3rd Qu.: 17.00   3rd Qu.: 4.000   3rd Qu.:  8.000  
    ##  Max.   :19.00   Max.   :8474.0   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000           Max.   :152.00   Max.   :40.000   Max.   :128.000  
    ##    num_videos     average_token_length  num_keywords     kw_min_min       kw_max_min      kw_avg_min        kw_min_max       kw_max_max    
    ##  Min.   : 0.000   Min.   :0.000        Min.   : 1.00   Min.   : -1.00   Min.   :    0   Min.   :   -1.0   Min.   :     0   Min.   : 37400  
    ##  1st Qu.: 0.000   1st Qu.:4.487        1st Qu.: 6.00   1st Qu.: -1.00   1st Qu.:  473   1st Qu.:  157.4   1st Qu.:     0   1st Qu.:843300  
    ##  Median : 0.000   Median :4.689        Median : 8.00   Median : -1.00   Median :  680   Median :  237.7   Median :  1900   Median :843300  
    ##  Mean   : 1.003   Mean   :4.589        Mean   : 7.65   Mean   : 26.52   Mean   : 1130   Mean   :  318.3   Mean   : 13246   Mean   :758191  
    ##  3rd Qu.: 1.000   3rd Qu.:4.890        3rd Qu.:10.00   3rd Qu.:  4.00   3rd Qu.: 1100   3rd Qu.:  365.4   3rd Qu.:  8900   3rd Qu.:843300  
    ##  Max.   :74.000   Max.   :7.218        Max.   :10.00   Max.   :217.00   Max.   :81200   Max.   :27123.0   Max.   :843300   Max.   :843300  
    ##    kw_avg_max       kw_min_avg     kw_max_avg       kw_avg_avg      self_reference_min_shares self_reference_max_shares
    ##  Min.   :  7328   Min.   :   0   Min.   :  2536   Min.   :  743.5   Min.   :     0            Min.   :     0           
    ##  1st Qu.:171983   1st Qu.:   0   1st Qu.:  3609   1st Qu.: 2469.9   1st Qu.:   631            1st Qu.:  1000           
    ##  Median :232358   Median :1200   Median :  4768   Median : 3040.8   Median :  1200            Median :  2700           
    ##  Mean   :245876   Mean   :1230   Mean   :  5966   Mean   : 3277.0   Mean   :  4988            Mean   : 10859           
    ##  3rd Qu.:310683   3rd Qu.:2159   3rd Qu.:  6825   3rd Qu.: 3818.8   3rd Qu.:  2700            3rd Qu.:  7600           
    ##  Max.   :843300   Max.   :3585   Max.   :120100   Max.   :15336.1   Max.   :843300            Max.   :843300           
    ##  self_reference_avg_sharess     LDA_00            LDA_01            LDA_02            LDA_03            LDA_04        global_subjectivity
    ##  Min.   :     0.0           Min.   :0.01887   Min.   :0.01820   Min.   :0.01819   Min.   :0.01830   Min.   :0.02000   Min.   :0.0000     
    ##  1st Qu.:   976.8           1st Qu.:0.02340   1st Qu.:0.02500   1st Qu.:0.02500   1st Qu.:0.02562   1st Qu.:0.02518   1st Qu.:0.3991     
    ##  Median :  2147.3           Median :0.03333   Median :0.03335   Median :0.03335   Median :0.05000   Median :0.04000   Median :0.4598     
    ##  Mean   :  7000.6           Mean   :0.16404   Mean   :0.16556   Mean   :0.20234   Mean   :0.25818   Mean   :0.20988   Mean   :0.4498     
    ##  3rd Qu.:  4907.5           3rd Qu.:0.18915   3rd Qu.:0.19478   3rd Qu.:0.29908   3rd Qu.:0.49532   3rd Qu.:0.32290   3rd Qu.:0.5158     
    ##  Max.   :843300.0           Max.   :0.91996   Max.   :0.91994   Max.   :0.92000   Max.   :0.91998   Max.   :0.92644   Max.   :0.9125     
    ##  global_sentiment_polarity global_rate_positive_words global_rate_negative_words rate_positive_words rate_negative_words avg_positive_polarity
    ##  Min.   :-0.37393          Min.   :0.00000            Min.   :0.00000            Min.   :0.0000      Min.   :0.0000      Min.   :0.0000       
    ##  1st Qu.: 0.05696          1st Qu.:0.02792            1st Qu.:0.01009            1st Qu.:0.6000      1st Qu.:0.1818      1st Qu.:0.3075       
    ##  Median : 0.12213          Median :0.03993            Median :0.01587            Median :0.7059      Median :0.2857      Median :0.3647       
    ##  Mean   : 0.12365          Mean   :0.04138            Mean   :0.01700            Mean   :0.6811      Mean   :0.2918      Mean   :0.3622       
    ##  3rd Qu.: 0.18800          3rd Qu.:0.05344            3rd Qu.:0.02244            3rd Qu.:0.8041      3rd Qu.:0.3871      3rd Qu.:0.4221       
    ##  Max.   : 0.60877          Max.   :0.15217            Max.   :0.10112            Max.   :1.0000      Max.   :1.0000      Max.   :1.0000       
    ##  min_positive_polarity max_positive_polarity avg_negative_polarity min_negative_polarity max_negative_polarity title_subjectivity
    ##  Min.   :0.00000       Min.   :0.0000        Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000       Min.   :0.0000    
    ##  1st Qu.:0.05000       1st Qu.:0.6000        1st Qu.:-0.3352       1st Qu.:-0.7500       1st Qu.:-0.1250       1st Qu.:0.0000    
    ##  Median :0.10000       Median :0.8000        Median :-0.2642       Median :-0.5000       Median :-0.1000       Median :0.2708    
    ##  Mean   :0.09748       Mean   :0.7774        Mean   :-0.2694       Mean   :-0.5472       Mean   :-0.1095       Mean   :0.3139    
    ##  3rd Qu.:0.10000       3rd Qu.:1.0000        3rd Qu.:-0.2000       3rd Qu.:-0.3944       3rd Qu.:-0.0500       3rd Qu.:0.5000    
    ##  Max.   :1.00000       Max.   :1.0000        Max.   : 0.0000       Max.   : 0.0000       Max.   : 0.0000       Max.   :1.0000    
    ##  title_sentiment_polarity abs_title_subjectivity abs_title_sentiment_polarity     shares     
    ##  Min.   :-1.00000         Min.   :0.0000         Min.   :0.0000               Min.   :   89  
    ##  1st Qu.: 0.00000         1st Qu.:0.1250         1st Qu.:0.0000               1st Qu.: 1200  
    ##  Median : 0.00000         Median :0.4000         Median :0.1000               Median : 1900  
    ##  Mean   : 0.09534         Mean   :0.3197         Mean   :0.1879               Mean   : 3808  
    ##  3rd Qu.: 0.25000         3rd Qu.:0.5000         3rd Qu.:0.3000               3rd Qu.: 3700  
    ##  Max.   : 1.00000         Max.   :0.5000         Max.   :1.0000               Max.   :83300

``` r
#correlation of selected variables
cors <- cor(select(DayTrain, -url, -timedelta, -weekday, -channel_type, -is_weekend))
#correlation plot showing just correlation with the response variable shares
corrplot(cors["shares",,drop=FALSE], type = "upper", tl.pos = "lt",cl.pos = "n")
```

![](Sunday_files/figure-gfm/summary-1.png)<!-- -->

``` r
#pairs data with selected variables
pairs(select(DayTrain, shares, num_hrefs, self_reference_min_shares, kw_max_min, self_reference_avg_sharess, kw_avg_avg, kw_avg_min, average_token_length, global_sentiment_polarity))
```

![](Sunday_files/figure-gfm/summary-2.png)<!-- -->

``` r
pairs(select(DayTrain, shares,num_videos, n_non_stop_unique_tokens, LDA_00, LDA_03, LDA_01, LDA_04, min_negative_polarity, n_unique_tokens))
```

![](Sunday_files/figure-gfm/summary-3.png)<!-- -->

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
    ## 1  0 7395.699 0.004033967 3520.731

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
    ## 1       0.1                 1             10      50 6381.344 0.02404790 3102.505 1438.300 0.02753011 316.3719
    ## 4       0.1                 2             10      50 6418.374 0.02549525 3111.779 1396.689 0.03094805 313.1963
    ## 7       0.1                 3             10      50 6420.379 0.02982099 3123.046 1363.703 0.03596412 304.5342
    ## 2       0.1                 1             10     100 6426.056 0.02218478 3094.182 1423.955 0.02573110 313.5077
    ## 5       0.1                 2             10     100 6491.276 0.02350113 3136.402 1350.708 0.02397184 307.5716
    ## 8       0.1                 3             10     100 6496.098 0.02852509 3155.806 1356.560 0.03206755 323.9846
    ## 3       0.1                 1             10     150 6441.610 0.02159761 3099.124 1431.971 0.02386735 316.4573
    ## 6       0.1                 2             10     150 6531.417 0.02210605 3182.584 1333.739 0.02006531 315.2883
    ## 9       0.1                 3             10     150 6554.726 0.02754234 3224.904 1346.869 0.02874089 323.6731

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
    ## 6716.743 5346.201
